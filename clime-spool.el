;;; clime-spool.el --- File-spool transport for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Readable sexp request/response transport for sandboxed clients that can
;; share a writable filesystem spool with a warm Clime authority process.  This
;; adapter normalizes spool frames into `clime-dispatch-request' records,
;; dispatches through `clime-dispatch-run-request', and writes printable
;; response frames.  It does not depend on `clime-serve' or web-server.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'clime-core)
(require 'clime-dispatch)

(defvar read-eval)

(defconst clime-spool-request-tag :clime/spool-request
  "Tag at the head of spool request frames.")

(defconst clime-spool-response-tag :clime/spool-response
  "Tag at the head of spool response frames.")

(defconst clime-spool-version 1
  "Readable spool protocol version.")

(defconst clime-spool--subdirs '("requests" "responses" "processing" "quarantine")
  "Subdirectories created under a spool root.")

(defconst clime-spool--safe-id-regexp "\\`[A-Za-z0-9][A-Za-z0-9._-]*\\'"
  "Allowlist regexp for request ids used as filename components.")

(defconst clime-spool--max-id-length 128
  "Maximum length for a spool request id.")

(define-error 'clime-spool-error "Spool transport error")

(defun clime-spool--plist-p (value)
  "Return non-nil when VALUE is a plist."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _val) on value by #'cddr
                always (keywordp key))))

(defun clime-spool-safe-id-p (id)
  "Return non-nil when ID is safe as one filename component."
  (and (stringp id)
       (< 0 (length id))
       (<= (length id) clime-spool--max-id-length)
       (string-match-p clime-spool--safe-id-regexp id)
       (not (member id '("." "..")))))

(defun clime-spool--validate-id (id)
  "Signal unless ID is safe as one filename component."
  (unless (clime-spool-safe-id-p id)
    (signal 'clime-spool-error
            (list "Spool request :id must be a safe filename component")))
  id)

(defun clime-spool--frame-plist (frame expected-tag)
  "Return payload plist from FRAME after checking EXPECTED-TAG and version."
  (unless (and (consp frame)
               (eq (car frame) expected-tag)
               (equal (cadr frame) clime-spool-version))
    (signal 'clime-spool-error
            (list (format "Expected %S version %s frame"
                          expected-tag clime-spool-version))))
  (let ((payload (cddr frame)))
    (unless (clime-spool--plist-p payload)
      (signal 'clime-spool-error
              (list "Spool frame payload must be a plist")))
    payload))

(defun clime-spool-frame-id (frame)
  "Return best-effort safe request id from FRAME, or nil."
  (when (and (consp frame) (clime-spool--plist-p (cddr frame)))
    (let ((id (plist-get (cddr frame) :id)))
      (when (clime-spool-safe-id-p id)
        id))))

(defun clime-spool--partial-id (text)
  "Return best-effort safe request id parsed from partial frame TEXT."
  (when (string-match ":id[[:space:]\n]+\"\\([^\"]*\\)\"" text)
    (let ((id (match-string 1 text)))
      (when (clime-spool-safe-id-p id)
        id))))

(defun clime-spool--validate-wire-value (value field)
  "Signal if VALUE under FIELD is not safe spool wire data."
  (cond
   ((or (functionp value) (processp value) (bufferp value) (markerp value))
    (signal 'clime-spool-error
            (list (format "Spool request %s must not contain callable or live objects"
                          field))))
   ((consp value)
    (clime-spool--validate-wire-value (car value) field)
    (clime-spool--validate-wire-value (cdr value) field))
   ((vectorp value)
    (dotimes (idx (length value))
      (clime-spool--validate-wire-value (aref value idx) field)))))

(defun clime-spool--input-from-frame (input)
  "Convert INPUT plist into a `clime-dispatch-input'."
  (unless (clime-spool--plist-p input)
    (signal 'clime-spool-error (list "Spool input must be a plist")))
  (let ((tail input))
    (while tail
      (unless (memq (car tail) '(:name :value :source :metadata))
        (signal 'clime-spool-error
                (list (format "Unknown spool input key %S" (car tail)))))
      (setq tail (cddr tail))))
  (clime-spool--validate-wire-value (plist-get input :value) ":value")
  (clime-spool--validate-wire-value (plist-get input :metadata) ":input metadata")
  (clime-make-dispatch-input
   :name (plist-get input :name)
   :value (plist-get input :value)
   :source (or (plist-get input :source) 'spool)
   :metadata (plist-get input :metadata)))

(defun clime-spool-request-from-frame (frame)
  "Normalize readable request FRAME into a `clime-dispatch-request'."
  (let* ((payload (clime-spool--frame-plist frame clime-spool-request-tag))
         (id (plist-get payload :id))
         (path (plist-get payload :path))
         (inputs (plist-get payload :inputs))
         (metadata (plist-get payload :metadata))
         (auth (plist-get payload :auth)))
    (clime-spool--validate-id id)
    (unless (and (listp path) (cl-every #'stringp path))
      (signal 'clime-spool-error
              (list "Spool request :path must be a list of strings")))
    (unless (or (null inputs) (listp inputs))
      (signal 'clime-spool-error (list "Spool request :inputs must be a list")))
    (unless (or (null auth) (clime-spool--plist-p auth))
      (signal 'clime-spool-error (list "Spool request :auth must be a plist")))
    (clime-spool--validate-wire-value metadata ":metadata")
    (clime-spool--validate-wire-value auth ":auth")
    (clime-make-dispatch-request
     :surface 'serve
     :adapter 'spool
     :path path
     :format (plist-get payload :format)
     :inputs (mapcar #'clime-spool--input-from-frame inputs)
     :metadata metadata
     :auth-context auth
     :correlation-id id)))

(defun clime-spool-response-frame (id response)
  "Convert dispatch RESPONSE into a readable spool response frame for ID."
  (list clime-spool-response-tag clime-spool-version
        :id id
        :outcome (clime-dispatch-response-outcome response)
        :status (clime-dispatch-response-status response)
        :body (or (clime-dispatch-response-body response) "")
        :content-type (or (clime-dispatch-response-content-type response)
                          "text/plain; charset=utf-8")
        :error-type (clime-dispatch-response-error-type response)
        :error-message (clime-dispatch-response-error-message response)
        :metadata (clime-dispatch-response-metadata response)))

(defun clime-spool--rejected-response (message &optional status error-type)
  "Build a rejected dispatch response for spool rejection MESSAGE."
  (clime-make-dispatch-response
   :outcome 'rejected
   :body (concat message "\n")
   :content-type "text/plain; charset=utf-8"
   :error-type (or error-type 'clime-spool-malformed-request)
   :error-message message
   :adapter-data (list :status (or status 400))))

(defun clime-spool--policy-rejection (node _request _fmt _start-time)
  "Return spool adapter policy rejection for NODE, or nil."
  (when-let ((policy (cdr (assq 'spool (clime-node-adapter-policies node)))))
    (cond
     ((not (clime-spool--plist-p policy))
      (clime-spool--rejected-response "Spool adapter policy must be a plist"))
     (policy
      (clime-spool--rejected-response
       (format "Unsupported spool adapter policy on `%s'"
               (clime-node-name node))))
     (t nil))))

(defun clime-spool--capability-response (request capability)
  "Return an auth rejection for REQUEST when CAPABILITY is not satisfied."
  (when capability
    (let* ((auth (clime-dispatch-request-auth-context request))
           (supplied (and (clime-spool--plist-p auth)
                          (plist-get auth :capability))))
      (unless (equal supplied capability)
        (clime-spool--rejected-response
         "Spool capability rejected"
         401 'clime-spool-unauthorized)))))

(defun clime-spool-dispatch-frame (app frame &optional default-format capability)
  "Dispatch readable request FRAME against APP and return a response frame."
  (let* ((id (clime-spool-frame-id frame))
         (request (clime-spool-request-from-frame frame))
         (response (or (clime-spool--capability-response request capability)
                       (clime-dispatch-run-request
                        app request default-format
                        :policy-rejector #'clime-spool--policy-rejection))))
    (clime-spool-response-frame id response)))

(defun clime-spool--dir (root name)
  "Return subdirectory NAME under ROOT."
  (expand-file-name name (file-name-as-directory (expand-file-name root))))

(defun clime-spool--root-truename (root)
  "Return ROOT truename after rejecting symlink roots."
  (when (file-symlink-p root)
    (signal 'clime-spool-error (list "Spool root must not be a symlink")))
  (file-truename root))

(defun clime-spool--ensure-directory (dir)
  "Create DIR if needed and reject non-directory or symlink entries."
  (cond
   ((file-symlink-p dir)
    (signal 'clime-spool-error
            (list (format "Spool path must not be a symlink: %s" dir))))
   ((file-exists-p dir)
    (unless (file-directory-p dir)
      (signal 'clime-spool-error
              (list (format "Spool path must be a directory: %s" dir)))))
   (t
    (make-directory dir t))))

(defun clime-spool--inside-root-p (root path)
  "Return non-nil when PATH is contained inside ROOT."
  (let* ((root-dir (file-name-as-directory (clime-spool--root-truename root)))
         (path-name (file-truename (or (file-name-directory path) path))))
    (string-prefix-p root-dir (file-name-as-directory path-name))))

(defun clime-spool--assert-inside-root (root path)
  "Signal unless PATH is contained inside ROOT."
  (unless (clime-spool--inside-root-p root path)
    (signal 'clime-spool-error
            (list (format "Spool path escapes root: %s" path))))
  path)

(defun clime-spool-ensure-root (root)
  "Ensure ROOT has the spool directory layout and return ROOT."
  (clime-spool--ensure-directory root)
  (clime-spool--root-truename root)
  (dolist (name clime-spool--subdirs)
    (let ((dir (clime-spool--dir root name)))
      (clime-spool--assert-inside-root root dir)
      (clime-spool--ensure-directory dir)))
  root)

(defun clime-spool--request-path (root id)
  "Return final request path for ID under ROOT."
  (clime-spool--validate-id id)
  (clime-spool--assert-inside-root
   root
   (expand-file-name (concat id ".request")
                     (clime-spool--dir root "requests"))))

(defun clime-spool--processing-path (root id)
  "Return processing request path for ID under ROOT."
  (clime-spool--validate-id id)
  (clime-spool--assert-inside-root
   root
   (expand-file-name (concat id ".request")
                     (clime-spool--dir root "processing"))))

(defun clime-spool--response-path (root id)
  "Return final response path for ID under ROOT."
  (clime-spool--validate-id id)
  (clime-spool--assert-inside-root
   root
   (expand-file-name (concat id ".response")
                     (clime-spool--dir root "responses"))))

(defun clime-spool--quarantine-path (root file)
  "Return deterministic quarantine path for FILE under ROOT."
  (let ((base (file-name-nondirectory file)))
    (clime-spool--assert-inside-root
     root
     (expand-file-name base (clime-spool--dir root "quarantine")))))

(defun clime-spool--tmp-path (target)
  "Return a temporary path in TARGET's directory."
  (expand-file-name
   (format "%s.tmp-%s-%s-%06x"
           (file-name-nondirectory target)
           (system-name)
           (emacs-pid)
           (random #x1000000))
   (file-name-directory target)))

(defun clime-spool--atomic-write-frame (target frame)
  "Write FRAME to TARGET via same-directory temp file plus rename."
  (let ((tmp (clime-spool--tmp-path target)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((print-level nil)
                  (print-length nil))
              (prin1 frame (current-buffer))
              (terpri (current-buffer)))
            (write-region (point-min) (point-max) tmp nil 'silent))
          (rename-file tmp target t)
          target)
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun clime-spool-write-request-file (root id frame)
  "Write request FRAME as ID.request under ROOT atomically."
  (clime-spool-ensure-root root)
  (let ((frame-id (plist-get (cddr frame) :id)))
    (unless (equal id frame-id)
      (signal 'clime-spool-error
              (list "Request filename id must match frame :id"))))
  (clime-spool--atomic-write-frame (clime-spool--request-path root id) frame))

(defun clime-spool-write-response-file (root id frame)
  "Write response FRAME as ID.response under ROOT atomically."
  (clime-spool-ensure-root root)
  (let ((frame-id (plist-get (cddr frame) :id)))
    (unless (equal id frame-id)
      (signal 'clime-spool-error
              (list "Response filename id must match frame :id"))))
  (clime-spool--atomic-write-frame (clime-spool--response-path root id) frame))

(defun clime-spool--read-file-text (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun clime-spool-read-frame-file (file)
  "Read one data frame from FILE with read-time evaluation disabled."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((read-eval nil))
      (read (current-buffer)))))

(defun clime-spool--request-files (root)
  "Return sorted complete request files under ROOT."
  (clime-spool-ensure-root root)
  (sort (directory-files (clime-spool--dir root "requests") t
                         "\\.request\\'")
        #'string<))

(defun clime-spool--id-from-request-file (file)
  "Return request id represented by FILE."
  (let ((base (file-name-nondirectory file)))
    (when (string-match "\\`\\(.+\\)\\.request\\'" base)
      (let ((id (match-string 1 base)))
        (when (clime-spool-safe-id-p id)
          id)))))

(defun clime-spool-claim-request (root request-file)
  "Claim REQUEST-FILE by renaming it into processing.
Return the processing file on success, or nil when another worker already
claimed it."
  (clime-spool-ensure-root root)
  (unless (clime-spool--assert-inside-root root request-file)
    (signal 'clime-spool-error (list "Request file escapes spool root")))
  (let ((id (clime-spool--id-from-request-file request-file)))
    (unless id
      (signal 'clime-spool-error (list "Unsafe request filename")))
    (when (file-symlink-p request-file)
      (let ((quarantine (clime-spool--quarantine-path root request-file)))
        (rename-file request-file quarantine t)
        (signal 'clime-spool-error (list "Spool request file must not be a symlink"))))
    (let ((processing (clime-spool--processing-path root id)))
      (condition-case err
          (progn
            (rename-file request-file processing nil)
            processing)
        (file-missing nil)
        (file-already-exists nil)
        (file-error
         (if (file-exists-p request-file)
             (signal (car err) (cdr err))
           nil))))))

(defun clime-spool--quarantine-file (root file)
  "Move FILE into ROOT's quarantine directory when it still exists."
  (when (or (file-exists-p file) (file-symlink-p file))
    (let ((quarantine (clime-spool--quarantine-path root file)))
      (rename-file file quarantine t)
      quarantine)))

(defun clime-spool--write-error-response (root id message &optional status error-type)
  "Write a structured error response under ROOT when ID is safe."
  (when (clime-spool-safe-id-p id)
    (clime-spool-write-response-file
     root id
     (clime-spool-response-frame
      id (clime-spool--rejected-response message status error-type)))))

(defun clime-spool--handle-processing-file
    (app root processing id default-format capability)
  "Handle claimed PROCESSING file and return processed request id."
  (let ((frame nil)
        (response nil)
        (processed-id id))
    (condition-case err
        (progn
          (setq frame (clime-spool-read-frame-file processing)
                processed-id (or (clime-spool-frame-id frame) processed-id)
                response (clime-spool-dispatch-frame
                          app frame default-format capability))
          (clime-spool-write-response-file root processed-id response))
      (error
       (let* ((text (condition-case nil
                        (clime-spool--read-file-text processing)
                      (error "")))
              (best-id (or (and frame (clime-spool-frame-id frame))
                           (clime-spool--partial-id text)
                           processed-id))
              (message (error-message-string err)))
         (clime-spool--write-error-response
          root best-id message 400 'clime-spool-malformed-request)
         (clime-spool--quarantine-file root processing))))
    (when (file-exists-p processing)
      (delete-file processing))
    processed-id))

(defun clime-spool--process-request-file
    (app root request-file default-format capability)
  "Process REQUEST-FILE and return its request id, or nil if already claimed."
  (let ((id (clime-spool--id-from-request-file request-file)))
    (if (file-symlink-p request-file)
        (progn
          (clime-spool--quarantine-file root request-file)
          id)
      (condition-case _err
          (when-let ((processing (clime-spool-claim-request root request-file)))
            (clime-spool--handle-processing-file
             app root processing id default-format capability))
        (clime-spool-error
         (clime-spool--quarantine-file root request-file)
         id)))))

(cl-defun clime-spool-process-one (app root &optional default-format
                                       &key capability)
  "Process one complete request under ROOT for APP.
Return the processed request id, or nil when no complete request is available."
  (clime-spool-ensure-root root)
  (catch 'processed
    (dolist (request-file (clime-spool--request-files root))
      (when-let ((id (clime-spool--process-request-file
                      app root request-file default-format capability)))
        (throw 'processed id)))
    nil))

(cl-defun clime-spool-submit-request (root frame &key (wait t) (timeout 30)
                                           delete-response (poll-interval 0.01))
  "Submit request FRAME under ROOT.
When WAIT is non-nil, wait up to TIMEOUT seconds for the matching response and
return the response frame.  Otherwise return the request file path."
  (let ((id (plist-get (cddr frame) :id)))
    (clime-spool--validate-id id)
    (let ((request-file (clime-spool-write-request-file root id frame)))
      (if wait
          (let ((response (clime-spool-wait-for-response
                           root id timeout poll-interval)))
            (when delete-response
              (let ((file (clime-spool--response-path root id)))
                (when (file-exists-p file)
                  (delete-file file))))
            response)
        request-file))))

(defun clime-spool-wait-for-response (root id timeout &optional poll-interval)
  "Wait for response ID under ROOT until TIMEOUT seconds elapse."
  (clime-spool-ensure-root root)
  (clime-spool--validate-id id)
  (let ((deadline (+ (float-time) timeout))
        (file (clime-spool--response-path root id))
        (interval (or poll-interval 0.01))
        response)
    (while (and (not response) (< (float-time) deadline))
      (if (file-exists-p file)
          (setq response (clime-spool-read-frame-file file))
        (sleep-for interval)))
    (unless response
      (signal 'clime-spool-error
              (list (format "Timed out waiting for spool response %s" id))))
    (let ((payload (clime-spool--frame-plist response clime-spool-response-tag)))
      (unless (equal (plist-get payload :id) id)
        (signal 'clime-spool-error
                (list "Spool response id does not match request id"))))
    response))

(defun clime-spool--stale-p (file max-age)
  "Return non-nil when FILE is older than MAX-AGE seconds."
  (> (- (float-time) (float-time (file-attribute-modification-time
                                  (file-attributes file))))
     max-age))

(defun clime-spool--delete-stale-matching (root dir regexp max-age)
  "Delete stale files under DIR matching REGEXP and contained in ROOT."
  (dolist (file (directory-files dir t regexp))
    (when (and (not (file-symlink-p file))
               (clime-spool--inside-root-p root file)
               (file-regular-p file)
               (clime-spool--stale-p file max-age))
      (delete-file file))))

(defun clime-spool-cleanup-stale (root max-age)
  "Clean stale tmp/processing files under ROOT older than MAX-AGE seconds."
  (clime-spool-ensure-root root)
  (clime-spool--delete-stale-matching
   root (clime-spool--dir root "requests") "\\.tmp-" max-age)
  (clime-spool--delete-stale-matching
   root (clime-spool--dir root "responses") "\\.tmp-" max-age)
  (clime-spool--delete-stale-matching
   root (clime-spool--dir root "processing") "\\.request\\'" max-age))

(defun clime-spool-process-loop (app root &optional default-format interval
                                     capability stop-predicate)
  "Poll ROOT and process requests for APP until STOP-PREDICATE returns non-nil.
Return the number of processed requests."
  (let ((count 0)
        (sleep-interval (or interval 0.05)))
    (while (not (and stop-predicate (funcall stop-predicate)))
      (if (clime-spool-process-one app root default-format
                                   :capability capability)
          (setq count (1+ count))
        (sleep-for sleep-interval)))
    count))

(provide 'clime-spool)
;;; clime-spool.el ends here
