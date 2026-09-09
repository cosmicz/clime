;;; clime-telemetry.el --- Privacy-safe local invocation telemetry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides metadata-only and result-aware local sinks for
;; `clime-invocation-event' values.  Result-aware records are explicitly
;; sensitive: direct inputs, handler returns, and error messages may contain
;; credentials or private data.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'clime-core)

;; `file-locked' is defined lazily by userlock.el, which does not provide a
;; feature.  Batch programs can reach the handler before that library loads.
(unless (get 'file-locked 'error-conditions)
  (define-error 'file-locked "File is locked" 'file-error))

(defgroup clime-telemetry nil
  "Local invocation telemetry sinks."
  :group 'clime)

(defcustom clime-telemetry-lock-retry-count 6
  "Maximum number of retries after a transient file-lock conflict."
  :type 'natnum
  :group 'clime-telemetry)

(defcustom clime-telemetry-lock-retry-delay 0.05
  "Seconds to wait between file-lock retries."
  :type 'number
  :group 'clime-telemetry)

(defun clime-telemetry--name (value)
  "Return VALUE as a JSON-friendly name string, preserving nil."
  (cond
   ((symbolp value) (and value (symbol-name value)))
   ((stringp value) value)
   (value (format "%s" value))))

(defun clime-telemetry--output-format-name (event)
  "Return the effective output-format name for EVENT."
  (let ((format (clime-invocation-event-format event)))
    (or (clime-telemetry--name
         (if (clime-output-format-p format)
             (clime-output-format-name format)
           format))
        "text")))

(defun clime-telemetry--safe-path (event)
  "Return EVENT's structured command path when it is safe to persist.

A serve `not-found' path consists of untrusted URL segments rather than a
resolved command path, so omit it."
  (unless (and (eq (clime-invocation-event-surface event) 'serve)
               (memq (clime-invocation-event-phase event)
                     '(not-found rejected)))
    (or (clime-invocation-event-display-path event)
        (clime-invocation-event-path event))))

;;;###autoload
(defun clime-telemetry-event-record (event)
  "Return a privacy-safe metadata alist for invocation EVENT.

The record omits all user-controlled values except the structured command
path.  In particular, it never includes argv, params, context, handler output,
return values, or error messages."
  (unless (clime-invocation-event-p event)
    (signal 'wrong-type-argument (list 'clime-invocation-event-p event)))
  (let ((start-time (clime-invocation-event-start-time event)))
    `((timestamp . ,(and start-time
                        (format-time-string "%FT%T%z"
                                            (seconds-to-time start-time))))
      (invocation_id . ,(clime-invocation-event-invocation-id event))
      (app . ,(clime-node-name (clime-invocation-event-app event)))
      (surface . ,(clime-telemetry--name
                   (clime-invocation-event-surface event)))
      (phase . ,(clime-telemetry--name
                 (clime-invocation-event-phase event)))
      (observer . ,(clime-telemetry--name
                    (clime-invocation-event-observer event)))
      (path . ,(clime-telemetry--safe-path event))
      (exit_code . ,(clime-invocation-event-exit-code event))
      (duration . ,(clime-invocation-event-duration event))
      (output_format . ,(clime-telemetry--output-format-name event))
      (error_type . ,(clime-telemetry--name
                      (clime-invocation-event-error-type event))))))

(defun clime-telemetry--readable-p (value)
  "Return non-nil when VALUE prints as one complete readable Lisp value."
  (condition-case nil
      (let* ((print-circle t)
             (print-escape-newlines t)
             (read-circle t)
             (printed (prin1-to-string value))
             (parsed (read-from-string printed)))
        (= (cdr parsed) (length printed)))
    (error nil)))

(defun clime-telemetry--json-encodable-p (value)
  "Return non-nil when VALUE can be encoded by the JSON codec."
  (condition-case nil
      (progn (json-encode value) t)
    (error nil)))

(defun clime-telemetry--fallback (value)
  "Return an explicit serialization fallback record for VALUE."
  (let ((print-circle t)
        (print-escape-newlines t)
        (print-level 4)
        (print-length 20))
    `((serialization_fallback . t)
      (type . ,(cond
                ((bufferp value) "buffer")
                ((functionp value) "function")
                ((symbolp value) "symbol")
                (t (format "%s" (type-of value)))))
      (printed . ,(condition-case nil
                      (prin1-to-string value)
                    (error "<unprintable>"))))))

(defun clime-telemetry--serializable-value (value)
  "Return VALUE when both sink codecs support it, otherwise a fallback."
  (if (and (clime-telemetry--json-encodable-p value)
           (clime-telemetry--readable-p value))
      value
    (clime-telemetry--fallback value)))

(defun clime-telemetry--params-record (params)
  "Convert PARAMS plist to an alist with independently safe values."
  (let (record)
    (while params
      (let ((name (pop params))
            (value (pop params)))
        (push (cons name (clime-telemetry--serializable-value value)) record)))
    (nreverse record)))

;;;###autoload
(defun clime-telemetry-invocation-record (event)
  "Return the fixed sensitive result-aware record for invocation EVENT.

Unlike `clime-telemetry-event-record', this includes direct user inputs, the
raw handler return, and the error message.  Unsupported parameter and return
values receive explicit field-level serialization fallback records."
  (let* ((metadata (clime-telemetry-event-record event))
         (error-type (clime-invocation-event-error-type event))
         (error-record
          (and error-type
               `((type . ,(clime-telemetry--name error-type))
                 (message . ,(clime-invocation-event-error-message event))))))
    (append metadata
            `((params . ,(clime-telemetry--params-record
                          (copy-sequence
                           (clime-invocation-event-provided-params event))))
              (returned . ,(if (clime-invocation-event-returned-p event)
                               t
                             :json-false))
              (return_value
               . ,(clime-telemetry--serializable-value
                   (clime-invocation-event-return-value event)))
              (error . ,error-record)
              (http_status
               . ,(and (eq (clime-invocation-event-adapter event) 'http)
                       (clime-invocation-event-response-status event)))))))

(defun clime-telemetry--append-line (file line)
  "Append LINE to FILE, retrying bounded transient lock conflicts."
  (let ((attempt 0))
    (while
        (condition-case err
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region line nil file 'append 'silent))
              nil)
          (file-locked
           (if (< attempt (max 0 clime-telemetry-lock-retry-count))
               (progn
                 (cl-incf attempt)
                 (when (> clime-telemetry-lock-retry-delay 0)
                   (sleep-for clime-telemetry-lock-retry-delay))
                 t)
              (signal (car err) (cdr err))))))))

(defun clime-telemetry--append-framed-line (file line)
  "Create FILE's parent, append LINE, and protect a newly created file."
  (let* ((expanded (expand-file-name file))
         (directory (file-name-directory expanded))
         (new-file-p (not (file-exists-p expanded))))
    (when directory
      (make-directory directory t))
    ;; Telemetry is append-only; Emacs lock files serialize concurrent writers
    ;; and can exhaust the bounded retry budget, silently mimicking a dead
    ;; invocation when the hook isolates its error.
    (let ((create-lockfiles nil))
      (clime-telemetry--append-line expanded line))
    (when new-file-p
      (set-file-modes expanded #o600)))
  file)

;;;###autoload
(defun clime-telemetry-append-jsonl (event file)
  "Append one privacy-safe JSON line for invocation EVENT to FILE.

Create FILE's parent directory when needed.  The append operation uses UTF-8
and bounded retries for transient Emacs file-lock conflicts."
  (clime-telemetry--append-framed-line
   file (concat (json-encode (clime-telemetry-event-record event)) "\n")))

(defun clime-telemetry--resolve-destination (destination event)
  "Resolve DESTINATION to a filename for EVENT."
  (let ((file (if (functionp destination)
                  (funcall destination event)
                destination)))
    (unless (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
    file))

;;;###autoload
(defun clime-telemetry-jsonl-sink (destination)
  "Return an invocation hook appending sensitive JSONL to DESTINATION.

DESTINATION is a filename or a function of one EVENT returning a filename."
  (unless (or (stringp destination) (functionp destination))
    (signal 'wrong-type-argument
            (list '(or stringp functionp) destination)))
  (lambda (event)
    (let ((file (clime-telemetry--resolve-destination destination event)))
      (clime-telemetry--append-framed-line
       file
       (concat (json-encode (clime-telemetry-invocation-record event)) "\n")))))

;;;###autoload
(defun clime-telemetry-eld-sink (destination)
  "Return an invocation hook appending sensitive ELD to DESTINATION.

DESTINATION is a filename or a function of one EVENT returning a filename.
Each line contains one complete readable Lisp record."
  (unless (or (stringp destination) (functionp destination))
    (signal 'wrong-type-argument
            (list '(or stringp functionp) destination)))
  (lambda (event)
    (let ((file (clime-telemetry--resolve-destination destination event))
          (print-circle t)
          (print-escape-newlines t))
      (clime-telemetry--append-framed-line
       file
       (concat (prin1-to-string
               (clime-telemetry-invocation-record event))
               "\n")))))

;;;###autoload
(defun clime-telemetry-lifecycle-jsonl-sink (destination)
  "Return an opt-in safe lifecycle JSONL hook writing to DESTINATION.
Unlike `clime-telemetry-jsonl-sink', this writer is suitable for
`:on-lifecycle': it writes both start and terminal metadata-only records."
  (unless (or (stringp destination) (functionp destination))
    (signal 'wrong-type-argument
            (list '(or stringp functionp) destination)))
  (lambda (event)
    (clime-telemetry-append-jsonl
     event (clime-telemetry--resolve-destination destination event))))

;;;###autoload
(defun clime-telemetry-lifecycle-eld-sink (destination)
  "Return an opt-in safe lifecycle ELD hook writing to DESTINATION."
  (unless (or (stringp destination) (functionp destination))
    (signal 'wrong-type-argument
            (list '(or stringp functionp) destination)))
  (lambda (event)
    (let ((file (clime-telemetry--resolve-destination destination event))
          (print-circle t)
          (print-escape-newlines t))
      (clime-telemetry--append-framed-line
       file (concat (prin1-to-string (clime-telemetry-event-record event))
                    "\n")))))

;;;###autoload
(defun clime-telemetry-append-parent-exit-jsonl
    (file invocation-id &optional exit-code signal)
  "Append a parent-observed process terminal record to FILE.
INVOCATION-ID must be propagated by a supervisor to the child.  EXIT-CODE or
SIGNAL describes only what that supervisor observed; this API never asserts an
application return value, error, handler phase, or output."
  (unless (stringp invocation-id)
    (signal 'wrong-type-argument (list 'stringp invocation-id)))
  (clime-telemetry--append-framed-line
   file
   (concat
    (json-encode
     `((timestamp . ,(format-time-string "%FT%T%z"))
       (invocation_id . ,invocation-id)
       (phase . "process-exit")
       (observer . "parent")
       (exit_code . ,exit-code)
       (signal . ,(clime-telemetry--name signal))))
    "\n")))

(provide 'clime-telemetry)
;;; clime-telemetry.el ends here
