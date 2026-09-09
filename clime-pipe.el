;;; clime-pipe.el --- Pipe/stdin transport for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Readable sexp request/response transport for warm Clime workers.  This
;; adapter normalizes pipe frames into `clime-dispatch-request' records,
;; dispatches through `clime-dispatch-run-request', and writes printable
;; response frames.  It does not depend on `clime-serve' or web-server.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'clime-core)
(require 'clime-dispatch)

(defvar read-eval)

(defconst clime-pipe-request-tag :clime/pipe-request
  "Tag at the head of pipe request frames.")

(defconst clime-pipe-response-tag :clime/pipe-response
  "Tag at the head of pipe response frames.")

(defconst clime-pipe-version 1
  "Readable pipe protocol version.")

(define-error 'clime-pipe-error "Pipe transport error")

(defun clime-pipe--plist-p (value)
  "Return non-nil when VALUE is a plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _val) on value by #'cddr
                always (keywordp key))))

(defun clime-pipe--frame-plist (frame expected-tag)
  "Return payload plist from FRAME after checking EXPECTED-TAG and version."
  (unless (and (consp frame)
               (eq (car frame) expected-tag)
               (equal (cadr frame) clime-pipe-version))
    (signal 'clime-pipe-error
            (list (format "Expected %S version %s frame"
                          expected-tag clime-pipe-version))))
  (let ((payload (cddr frame)))
    (unless (clime-pipe--plist-p payload)
      (signal 'clime-pipe-error
              (list "Pipe frame payload must be a plist")))
    payload))

(defun clime-pipe-frame-id (frame)
  "Return best-effort request id from FRAME, or nil."
  (when (and (consp frame) (clime-pipe--plist-p (cddr frame)))
    (plist-get (cddr frame) :id)))

(defun clime-pipe--validate-wire-value (value field)
  "Signal if VALUE under FIELD is not safe pipe wire data."
  (cond
   ((or (functionp value) (processp value) (bufferp value) (markerp value))
    (signal 'clime-pipe-error
            (list (format "Pipe request %s must not contain callable or live objects"
                          field))))
   ((consp value)
    (clime-pipe--validate-wire-value (car value) field)
    (clime-pipe--validate-wire-value (cdr value) field))
   ((vectorp value)
    (dotimes (idx (length value))
      (clime-pipe--validate-wire-value (aref value idx) field)))))

(defun clime-pipe--input-from-frame (input)
  "Convert INPUT plist into a `clime-dispatch-input'."
  (unless (clime-pipe--plist-p input)
    (signal 'clime-pipe-error (list "Pipe input must be a plist")))
  (let ((tail input))
    (while tail
      (unless (memq (car tail) '(:name :value :source :metadata))
        (signal 'clime-pipe-error
                (list (format "Unknown pipe input key %S" (car tail)))))
      (setq tail (cddr tail))))
  (clime-pipe--validate-wire-value (plist-get input :value) ":value")
  (clime-pipe--validate-wire-value (plist-get input :metadata) ":input metadata")
  (clime-make-dispatch-input
   :name (plist-get input :name)
   :value (plist-get input :value)
   :source (or (plist-get input :source) 'pipe)
   :metadata (plist-get input :metadata)))

(defun clime-pipe-request-from-frame (frame)
  "Normalize readable request FRAME into a `clime-dispatch-request'."
  (let* ((payload (clime-pipe--frame-plist frame clime-pipe-request-tag))
         (id (plist-get payload :id))
         (path (plist-get payload :path))
         (inputs (plist-get payload :inputs))
         (metadata (plist-get payload :metadata)))
    (unless (or (null id) (stringp id))
      (signal 'clime-pipe-error (list "Pipe request :id must be a string")))
    (unless (and (listp path) (cl-every #'stringp path))
      (signal 'clime-pipe-error
              (list "Pipe request :path must be a list of strings")))
    (unless (or (null inputs) (listp inputs))
      (signal 'clime-pipe-error (list "Pipe request :inputs must be a list")))
    (clime-pipe--validate-wire-value metadata ":metadata")
    (clime-make-dispatch-request
     :surface 'serve
     :adapter 'pipe
     :path path
     :format (plist-get payload :format)
     :inputs (mapcar #'clime-pipe--input-from-frame inputs)
     :metadata metadata
     :correlation-id id)))

(defun clime-pipe-response-frame (id response)
  "Convert dispatch RESPONSE into a readable pipe response frame for ID."
  (list clime-pipe-response-tag clime-pipe-version
        :id id
        :outcome (clime-dispatch-response-outcome response)
        :status (clime-dispatch-response-status response)
        :body (or (clime-dispatch-response-body response) "")
        :content-type (or (clime-dispatch-response-content-type response)
                          "text/plain; charset=utf-8")
        :error-type (clime-dispatch-response-error-type response)
        :error-message (clime-dispatch-response-error-message response)
        :metadata (clime-dispatch-response-metadata response)))

(defun clime-pipe--rejected-response (message)
  "Build a rejected dispatch response for malformed pipe request MESSAGE."
  (clime-make-dispatch-response
   :outcome 'rejected
   :body (concat message "\n")
   :content-type "text/plain; charset=utf-8"
   :error-type 'clime-pipe-malformed-request
   :error-message message
   :adapter-data '(:status 400)))

(defun clime-pipe--policy-rejection (node _request _fmt _start-time)
  "Return pipe adapter policy rejection for NODE, or nil."
  (when-let ((policy (cdr (assq 'pipe (clime-node-adapter-policies node)))))
    (cond
     ((not (clime-pipe--plist-p policy))
      (clime-pipe--rejected-response "Pipe adapter policy must be a plist"))
     (policy
      (clime-pipe--rejected-response
       (format "Unsupported pipe adapter policy on `%s'"
               (clime-node-name node))))
     (t nil))))

(defun clime-pipe-dispatch-frame (app frame &optional default-format)
  "Dispatch readable request FRAME against APP and return a response frame."
  (let* ((id (clime-pipe-frame-id frame))
         (request (clime-pipe-request-from-frame frame))
         (response (clime-dispatch-run-request
                    app request default-format
                    :policy-rejector #'clime-pipe--policy-rejection)))
    (clime-pipe-response-frame id response)))

(defun clime-pipe--partial-id (text)
  "Return best-effort request id parsed from partial frame TEXT."
  (when (string-match ":id[[:space:]\n]+\"\\([^\"]*\\)\"" text)
    (match-string 1 text)))

(defun clime-pipe--non-ws-p (text)
  "Return non-nil when TEXT has any non-whitespace character."
  (string-match-p "[^[:space:]\n]" text))

(defun clime-pipe--read-next (input)
  "Read next object from INPUT.
Return (:frame OBJECT), (:error MESSAGE ID), or (:eof)."
  (let ((start (and (bufferp input)
                    (with-current-buffer input (point)))))
    (condition-case err
        (let ((read-eval nil))
          (list :frame (read input)))
      (end-of-file
       (if (and (bufferp input)
                (let ((remaining (with-current-buffer input
                                   (buffer-substring-no-properties
                                    start (point-max)))))
                  (and (clime-pipe--non-ws-p remaining)
                       remaining)))
           (let ((remaining (with-current-buffer input
                              (buffer-substring-no-properties
                               start (point-max)))))
             (list :error "Truncated pipe request frame"
                   (clime-pipe--partial-id remaining)))
         (list :eof)))
      (error
       (when (bufferp input)
         (with-current-buffer input
           (forward-line 1)))
       (list :error (error-message-string err) nil)))))

(defun clime-pipe--write-frame (output frame)
  "Write FRAME to OUTPUT as one readable sexp line."
  (let ((text (concat (prin1-to-string frame) "\n")))
    (cond
     ((bufferp output)
      (with-current-buffer output
        (goto-char (point-max))
        (insert text)))
     ((processp output)
      (process-send-string output text))
     ((functionp output)
      (funcall output text))
     (t
      (princ text output)))))

(defun clime-pipe-run-worker (app input output &optional default-format)
  "Run warm pipe worker for APP from INPUT to OUTPUT.
INPUT is a readable stream, usually stdin or a buffer.  OUTPUT is a
buffer, process, function, or standard print target.  Returns the number
of response frames written."
  (let ((count 0)
        (done nil))
    (while (not done)
      (pcase (clime-pipe--read-next input)
        (`(:eof)
         (setq done t))
        (`(:error ,message ,id)
         (clime-pipe--write-frame
          output
          (clime-pipe-response-frame
           id (clime-pipe--rejected-response message)))
         (setq count (1+ count)))
        (`(:frame ,frame)
         (let ((response
                (condition-case err
                    (clime-pipe-dispatch-frame app frame default-format)
                  (error
                   (clime-pipe-response-frame
                    (clime-pipe-frame-id frame)
                    (clime-pipe--rejected-response
                     (error-message-string err)))))))
           (clime-pipe--write-frame output response)
           (setq count (1+ count))))))
    count))

(provide 'clime-pipe)
;;; clime-pipe.el ends here
