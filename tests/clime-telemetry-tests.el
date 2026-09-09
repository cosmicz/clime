;;; clime-telemetry-tests.el --- Tests for local invocation telemetry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the privacy-safe local JSONL invocation sink.

;;; Code:

(require 'ert)
(require 'json)
(require 'clime-core)
(require 'clime-run)

(condition-case nil
    (require 'clime-telemetry)
  (error nil))

(defconst clime-test-telemetry--secret "fh2u-sentinel-secret"
  "Sentinel that must never reach default telemetry records.")

(defun clime-test-telemetry--require ()
  "Assert the telemetry API is available."
  (should (featurep 'clime-telemetry))
  (should (fboundp 'clime-telemetry-event-record))
  (should (fboundp 'clime-telemetry-append-jsonl))
  (should (fboundp 'clime-telemetry-invocation-record))
  (should (fboundp 'clime-telemetry-jsonl-sink))
  (should (fboundp 'clime-telemetry-eld-sink))
  (should (fboundp 'clime-telemetry-lifecycle-jsonl-sink))
  (should (fboundp 'clime-telemetry-lifecycle-eld-sink))
  (should (fboundp 'clime-telemetry-append-parent-exit-jsonl)))

(defun clime-test-telemetry--event ()
  "Return an invocation event containing secret-bearing fields."
  (let ((app (clime-make-app :name "telemetry-test")))
    (clime-invocation-event--create
     :app app
     :surface 'cli
     :phase 'runtime-error
     :argv (list "send" "--token" clime-test-telemetry--secret)
     :path '("telemetry-test" "send")
     :display-path '("telemetry-test" "send")
     :params (list 'token clime-test-telemetry--secret)
     :provided-params (list 'token clime-test-telemetry--secret)
     :handler-invoked-p t
     :execution-duration 0.1
     :returned-p t
     :return-value '(:sent t)
     :adapter 'http
     :response-status 500
     :exit-code 1
     :error-type 'error
     :error-message (concat "failed with " clime-test-telemetry--secret)
     :start-time 1784620800.0
     :duration 0.125
     :format (clime-make-output-format
              :name 'json :flags '("--json") :help "JSON"))))

(ert-deftest clime-test-telemetry/lifecycle-sink-persists-safe-correlated-pairs ()
  "Lifecycle JSONL records pair start and terminal records without secrets."
  (clime-test-telemetry--require)
  (let* ((root (make-temp-file "clime-telemetry-lifecycle-" t))
         (file (expand-file-name "events.jsonl" root))
         (start (clime-test-telemetry--event))
         (terminal (clime-test-telemetry--event))
         (sink (clime-telemetry-lifecycle-jsonl-sink file)))
    (unwind-protect
        (progn
          (setf (clime-invocation-event-invocation-id start) "opaque-id-42"
                (clime-invocation-event-phase start) 'started
                (clime-invocation-event-path start) nil
                (clime-invocation-event-display-path start) nil
                (clime-invocation-event-invocation-id terminal) "opaque-id-42")
          (funcall sink start)
          (funcall sink terminal)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (= 2 (length lines)))
              (dolist (line lines)
                (should (string-match-p "opaque-id-42" line))
                (should-not (string-match-p clime-test-telemetry--secret line))
                (should-not (string-match-p (number-to-string (emacs-pid)) line))))))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/lifecycle-sink-correlates-out-of-order-terminals ()
  "Append order may finish B before A while IDs keep each pair unambiguous."
  (clime-test-telemetry--require)
  (let* ((root (make-temp-file "clime-telemetry-order-" t))
         (file (expand-file-name "events.jsonl" root))
         (sink (clime-telemetry-lifecycle-jsonl-sink file))
         (a-start (clime-test-telemetry--event))
         (a-terminal (clime-test-telemetry--event))
         (b-start (clime-test-telemetry--event))
         (b-terminal (clime-test-telemetry--event)))
    (unwind-protect
        (progn
          (dolist (event (list a-start a-terminal b-start b-terminal))
            (setf (clime-invocation-event-invocation-id event)
                  (if (memq event (list a-start a-terminal)) "A" "B")))
          (setf (clime-invocation-event-phase a-start) 'started
                (clime-invocation-event-phase b-start) 'started)
          (mapc sink (list a-start b-start b-terminal a-terminal))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((json-object-type 'alist) (json-key-type 'symbol)
                  (records (mapcar #'json-read-from-string
                                   (split-string (buffer-string) "\n" t))))
              (should (equal '("A" "B" "B" "A")
                             (mapcar (lambda (record)
                                       (alist-get 'invocation_id record))
                                     records)))
              (dolist (id '("A" "B"))
                (should (= 2 (cl-count id records
                                       :key (lambda (record)
                                              (alist-get 'invocation_id record))
                                       :test #'equal))))))
      (delete-directory root t)))))

(ert-deftest clime-test-telemetry/sigkill-leaves-an-unmatched-start ()
  "A killed child cannot self-report a terminal after its durable start."
  (skip-unless (executable-find "sh"))
  (let* ((root (make-temp-file "clime-telemetry-sigkill-" t))
         (file (expand-file-name "events.jsonl" root))
         (start (clime-test-telemetry--event))
         (sink (clime-telemetry-lifecycle-jsonl-sink file)))
    (unwind-protect
        (progn
          (setf (clime-invocation-event-invocation-id start) "killed-child"
                (clime-invocation-event-phase start) 'started)
          (funcall sink start)
          (should-not (equal 0 (call-process "sh" nil nil nil "-c"
                                              "kill -KILL $$")))
          (with-temp-buffer
            (insert-file-contents file)
            (should (= 1 (length (split-string (buffer-string) "\n" t))))))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/append-disables-lockfiles-for-concurrent-writers ()
  "Append-only telemetry never lets lockfiles turn a write loss into a kill."
  (let* ((root (make-temp-file "clime-telemetry-lockfiles-" t))
         (file (expand-file-name "events.jsonl" root))
         (event (clime-test-telemetry--event))
         (seen-lockfiles :unset)
         (real-write-region (symbol-function 'write-region))
         (create-lockfiles t))
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest args)
                     (setq seen-lockfiles create-lockfiles)
                     (apply real-write-region args))))
          (clime-telemetry-append-jsonl event file)
          (should-not seen-lockfiles))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/parent-exit-has-no-application-result ()
  "A supervisor may append its observed signal without inventing app data."
  (clime-test-telemetry--require)
  (skip-unless (executable-find "sh"))
  (let* ((root (make-temp-file "clime-telemetry-parent-exit-" t))
         (file (expand-file-name "events.jsonl" root)))
    (unwind-protect
        (progn
          (should-not (equal 0 (call-process "sh" nil nil nil "-c"
                                              "kill -TERM $$")))
          (clime-telemetry-append-parent-exit-jsonl
           file "parent-observed-42" nil 'SIGTERM)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((json-object-type 'alist) (json-key-type 'symbol))
              (let ((record (json-read-from-string (buffer-string))))
                (should (equal "parent-observed-42"
                               (alist-get 'invocation_id record)))
                (should (equal "process-exit" (alist-get 'phase record)))
                (should (equal "parent" (alist-get 'observer record)))
                (should (equal "SIGTERM" (alist-get 'signal record)))
                (should-not (assq 'returned record))
                (should-not (assq 'return_value record))
                (should-not (assq 'error record))))))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/record-is-metadata-only ()
  "Default records expose lifecycle metadata but no input or message bodies."
  (clime-test-telemetry--require)
  (let* ((record (clime-telemetry-event-record
                  (clime-test-telemetry--event)))
         (encoded (json-encode record)))
    (should (equal "telemetry-test" (alist-get 'app record)))
    (should (equal "cli" (alist-get 'surface record)))
    (should (equal "runtime-error" (alist-get 'phase record)))
    (should (equal '("telemetry-test" "send")
                   (alist-get 'path record)))
    (should (= 1 (alist-get 'exit_code record)))
    (should (= 0.125 (alist-get 'duration record)))
    (should (equal "error" (alist-get 'error_type record)))
    (should (equal "json" (alist-get 'output_format record)))
    (should (equal (format-time-string "%FT%T%z"
                                       (seconds-to-time 1784620800.0))
                   (alist-get 'timestamp record)))
    (dolist (forbidden '(argv params context output return_value error_message))
      (should-not (assq forbidden record)))
    (should-not (string-match-p clime-test-telemetry--secret encoded))))

(ert-deftest clime-test-telemetry/unresolved-serve-path-is-omitted ()
  "Untrusted URL segments from a serve 404 are not persisted."
  (clime-test-telemetry--require)
  (let ((event (clime-test-telemetry--event)))
    (setf (clime-invocation-event-surface event) 'serve
          (clime-invocation-event-phase event) 'not-found
          (clime-invocation-event-display-path event) nil
          (clime-invocation-event-path event)
          (list clime-test-telemetry--secret)
          (clime-invocation-event-format event) nil)
    (let* ((record (clime-telemetry-event-record event))
           (encoded (json-encode record)))
      (should-not (alist-get 'path record))
      (should (equal "text" (alist-get 'output_format record)))
      (should-not (string-match-p clime-test-telemetry--secret encoded)))))

(ert-deftest clime-test-telemetry/unresolved-serve-rejection-path-is-omitted ()
  "A policy rejection must not turn raw URL segments into safe telemetry."
  (clime-test-telemetry--require)
  (let ((event (clime-test-telemetry--event)))
    (setf (clime-invocation-event-surface event) 'serve
          (clime-invocation-event-phase event) 'rejected
          (clime-invocation-event-display-path event) nil
          (clime-invocation-event-path event)
          (list clime-test-telemetry--secret))
    (let* ((record (clime-telemetry-event-record event))
           (encoded (json-encode record)))
      (should-not (alist-get 'path record))
      (should-not (string-match-p clime-test-telemetry--secret encoded)))))

(ert-deftest clime-test-telemetry/append-creates-and-preserves-jsonl ()
  "Appending creates parents and preserves one complete line per event."
  (clime-test-telemetry--require)
  (let* ((root (make-temp-file "clime-telemetry-test-" t))
         (file (expand-file-name "nested/invocations.jsonl" root))
         (event (clime-test-telemetry--event)))
    (unwind-protect
        (progn
          (should-not (file-exists-p (file-name-directory file)))
          (clime-telemetry-append-jsonl event file)
          (clime-telemetry-append-jsonl event file)
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-suffix-p "\n" (buffer-string)))
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (= 2 (length lines)))
              (dolist (line lines)
                (let ((json-object-type 'alist)
                      (json-key-type 'symbol))
                  (should (equal "telemetry-test"
                                 (alist-get 'app
                                            (json-read-from-string line)))))
                (should-not
                 (string-match-p clime-test-telemetry--secret line))))))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/invocation-record-has-fixed-sensitive-schema ()
  "The result-aware record keeps result, error, params, and HTTP independent."
  (clime-test-telemetry--require)
  (let* ((record (clime-telemetry-invocation-record
                  (clime-test-telemetry--event)))
         (params (alist-get 'params record))
         (error-record (alist-get 'error record)))
    (should (equal clime-test-telemetry--secret
                   (alist-get 'token params)))
    (should (eq t (alist-get 'returned record)))
    (should (equal '(:sent t) (alist-get 'return_value record)))
    (should (= 1 (alist-get 'exit_code record)))
    (should (equal "error" (alist-get 'type error-record)))
    (should (string-match-p clime-test-telemetry--secret
                            (alist-get 'message error-record)))
    (should (= 500 (alist-get 'http_status record)))
    (should-not (assq 'output record))
    (should-not (assq 'response_body record))
    (should (assq 'error record))
    (should (assq 'http_status record))))

(ert-deftest clime-test-telemetry/non-http-record-does-not-invent-status ()
  "A non-HTTP adapter always records nil in the fixed http_status field."
  (let ((event (clime-test-telemetry--event)))
    (setf (clime-invocation-event-adapter event) 'pipe
          (clime-invocation-event-response-status event) 200)
    (should (null
             (alist-get 'http_status
                        (clime-telemetry-invocation-record event))))
    (setf (clime-invocation-event-adapter event) 'http
          (clime-invocation-event-response-status event) 302
          (clime-invocation-event-error-type event) nil
          (clime-invocation-event-error-message event) nil
          (clime-invocation-event-returned-p event) nil
          (clime-invocation-event-return-value event) nil)
    (let ((record (clime-telemetry-invocation-record event)))
      (should (= 302 (alist-get 'http_status record)))
      (should (null (alist-get 'error record)))
      (should (eq :json-false (alist-get 'returned record)))
      (should (null (alist-get 'return_value record))))))

(ert-deftest clime-test-telemetry/sinks-append-jsonl-and-readable-eld ()
  "Sink closures resolve destinations and append protected framed records."
  (clime-test-telemetry--require)
  (let* ((root (make-temp-file "clime-telemetry-sinks-" t))
         (json-file (expand-file-name "json/runtime-error.jsonl" root))
         (eld-file (expand-file-name "eld/invocations.eld" root))
         (event (clime-test-telemetry--event))
         (json-sink
          (clime-telemetry-jsonl-sink
           (lambda (candidate)
             (expand-file-name
              (format "json/%s.jsonl"
                      (clime-invocation-event-phase candidate))
              root))))
         (eld-sink
          (clime-telemetry-eld-sink
           (lambda (_candidate) eld-file))))
    (unwind-protect
        (progn
          (setf (clime-invocation-event-provided-params event)
                (append (clime-invocation-event-provided-params event)
                        '(multiline "first\nsecond")))
          ;; Redirect standard output; json-encode uses princ internally.
          (with-output-to-string
            (funcall json-sink event)
            (funcall json-sink event)
            (funcall eld-sink event)
            (funcall eld-sink event))
          (dolist (file (list json-file eld-file))
            (should (file-exists-p file))
            (should (= #o600 (logand #o777 (file-modes file)))))
          (with-temp-buffer
            (insert-file-contents json-file)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (= 2 (length lines)))
              (dolist (line lines)
                (let ((json-object-type 'alist)
                      (json-key-type 'symbol))
                  (should (= 500
                             (alist-get
                              'http_status
                              (json-read-from-string line))))))))
          (with-temp-buffer
            (insert-file-contents eld-file)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (= 2 (length lines)))
              (dolist (line lines)
                (let* ((read-result (read-from-string line))
                       (record (car read-result)))
                  (should (= (length line) (cdr read-result)))
                  (should (= 500 (alist-get 'http_status record))))))))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/unsupported-values-use-field-fallback ()
  "Unreadable values fall back per field without discarding the record."
  (let* ((event (clime-test-telemetry--event))
         (buffer (generate-new-buffer " *clime-telemetry-unreadable*")))
    (unwind-protect
        (progn
          (setf (clime-invocation-event-provided-params event)
                (list 'safe "kept" 'opaque buffer)
                (clime-invocation-event-return-value event) buffer)
          (let* ((record (clime-telemetry-invocation-record event))
                 (params (alist-get 'params record))
                 (param-fallback (alist-get 'opaque params))
                 (return-fallback (alist-get 'return_value record))
                 (json (json-encode record))
                 (eld (prin1-to-string record)))
            (should (equal "kept" (alist-get 'safe params)))
            (should (eq t (alist-get 'serialization_fallback
                                     param-fallback)))
            (should (eq t (alist-get 'serialization_fallback
                                     return-fallback)))
            (should (json-read-from-string json))
            (should (= (length eld) (cdr (read-from-string eld))))))
      (kill-buffer buffer))))

(ert-deftest clime-test-telemetry/append-retries-transient-lock ()
  "A transient lock failure is retried without duplicating the record."
  (clime-test-telemetry--require)
  (let* ((root (make-temp-file "clime-telemetry-lock-test-" t))
         (file (expand-file-name "invocations.jsonl" root))
         (real-write-region (symbol-function 'write-region))
         (attempts 0)
         (clime-telemetry-lock-retry-count 1)
         (clime-telemetry-lock-retry-delay 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest args)
                       (setq attempts (1+ attempts))
                       (if (= attempts 1)
                           (signal 'file-locked (list file "test"))
                         (apply real-write-region args)))))
            (clime-telemetry-append-jsonl
             (clime-test-telemetry--event) file))
          (should (= 2 attempts))
          (with-temp-buffer
            (insert-file-contents file)
            (should (= 1 (length (split-string (buffer-string) "\n" t))))))
      (delete-directory root t))))

(ert-deftest clime-test-telemetry/hook-write-failure-preserves-exit-code ()
  "A sink failure is isolated by the unified invocation hook boundary."
  (clime-test-telemetry--require)
  (let* ((directory-as-file (make-temp-file "clime-telemetry-fail-" t))
         (app (clime-make-app
               :name "telemetry-hook-test"
               :handler (lambda (_ctx) (princ "handler-output") nil)
               :on-invocation
               (clime-telemetry-jsonl-sink directory-as-file))))
    (unwind-protect
        (let (code)
          (should (equal "handler-output"
                         (with-output-to-string
                           (setq code (clime-run app nil)))))
          (should (= 0 code)))
      (delete-directory directory-as-file t))))

(provide 'clime-telemetry-tests)
;;; clime-telemetry-tests.el ends here
