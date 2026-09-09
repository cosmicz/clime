;;; clime-pipe-tests.el --- Tests for pipe/stdin transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the readable pipe/stdin transport over the shared dispatch core.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-dsl)

(defconst clime-test-pipe--serve-feature-before-require (featurep 'clime-serve)
  "Whether `clime-serve' was loaded before requiring `clime-pipe'.")

(defconst clime-test-pipe--web-server-feature-before-require (featurep 'web-server)
  "Whether `web-server' was loaded before requiring `clime-pipe'.")

(condition-case nil
    (require 'clime-pipe)
  (error nil))

(defconst clime-test-pipe--serve-feature-after-require (featurep 'clime-serve)
  "Whether `clime-serve' was loaded by requiring `clime-pipe'.")

(defconst clime-test-pipe--web-server-feature-after-require (featurep 'web-server)
  "Whether `web-server' was loaded by requiring `clime-pipe'.")

(defun clime-test-pipe--require ()
  "Assert the pipe transport API is available."
  (should (featurep 'clime-pipe))
  (should (fboundp 'clime-pipe-request-from-frame))
  (should (fboundp 'clime-pipe-response-frame))
  (should (fboundp 'clime-pipe-dispatch-frame))
  (should (fboundp 'clime-pipe-run-worker)))

(eval '(clime-app clime-test--pipe-app
         :version "1.0"
         (clime-command echo
           (clime-arg name :required t)
           (clime-handler (ctx)
             (format "hello:%s" (clime-ctx-get ctx 'name))))
         (clime-command need
           (clime-arg value :required t)
           (clime-handler (ctx)
             (format "need:%s" (clime-ctx-get ctx 'value))))
         (clime-command boom
           (clime-handler (_ctx)
             (error "boom"))))
      t)

(defun clime-test-pipe--request (id path &optional inputs metadata)
  "Build a pipe request frame with ID, PATH, INPUTS, and METADATA."
  `(:clime/pipe-request 1
    :id ,id
    :path ,path
    :inputs ,inputs
    :metadata ,metadata))

(defun clime-test-pipe--frames-from-string (text)
  "Read all pipe frames from TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (frames done)
      (while (not done)
        (condition-case nil
            (push (read (current-buffer)) frames)
          (end-of-file (setq done t))))
      (nreverse frames))))

(defun clime-test-pipe--run-worker-for-app (app frames)
  "Run pipe worker for APP and FRAMES, returning response frames."
  (with-temp-buffer
    (dolist (frame frames)
      (prin1 frame (current-buffer))
      (terpri (current-buffer)))
    (goto-char (point-min))
    (let ((input (current-buffer))
          (output (generate-new-buffer " *clime-pipe-output*")))
      (unwind-protect
          (progn
            (clime-pipe-run-worker app input output)
            (with-current-buffer output
              (clime-test-pipe--frames-from-string (buffer-string))))
        (kill-buffer output)))))

(defun clime-test-pipe--run-worker (&rest frames)
  "Run pipe worker for FRAMES and return response frames."
  (clime-test-pipe--run-worker-for-app clime-test--pipe-app frames))

(ert-deftest clime-test-pipe/request-frame-normalizes-to-dispatch-request ()
  "Readable pipe request frames become transport-neutral dispatch requests."
  (clime-test-pipe--require)
  (let* ((frame (clime-test-pipe--request
                 "req-1" '("echo")
                 '((:name name :value "Ada" :source pipe))
                 '(:session "s1")))
         (request (clime-pipe-request-from-frame frame)))
    (should (memq 'pipe clime-dispatch-adapters))
    (should (clime-dispatch-request-p request))
    (should (eq (clime-dispatch-request-surface request) 'serve))
    (should (eq (clime-dispatch-request-adapter request) 'pipe))
    (should (equal (clime-dispatch-request-path request) '("echo")))
    (should (equal (clime-dispatch-request-correlation-id request) "req-1"))
    (should (eq clime-test-pipe--serve-feature-after-require
                clime-test-pipe--serve-feature-before-require))
    (should (eq clime-test-pipe--web-server-feature-after-require
                clime-test-pipe--web-server-feature-before-require))))

(ert-deftest clime-test-pipe/worker-success-response-frame ()
  "Worker loop dispatches one request and writes a completed response frame."
  (clime-test-pipe--require)
  (let* ((frames (clime-test-pipe--run-worker
                  (clime-test-pipe--request
                   "req-1" '("echo" "Ada") nil nil)))
         (response (car frames)))
    (should (= 1 (length frames)))
    (should (eq (car response) :clime/pipe-response))
    (should (equal (plist-get (cddr response) :id) "req-1"))
    (should (eq (plist-get (cddr response) :outcome) 'completed))
    (should (equal (plist-get (cddr response) :status) 200))
    (should (string-match-p "hello:Ada" (plist-get (cddr response) :body)))))

(ert-deftest clime-test-pipe/usage-and-runtime-statuses ()
  "Usage and runtime failures map to 400 and 500 style response frames."
  (clime-test-pipe--require)
  (let* ((frames (clime-test-pipe--run-worker
                  (clime-test-pipe--request "usage" '("need") nil nil)
                  (clime-test-pipe--request "runtime" '("boom") nil nil)))
         (usage (nth 0 frames))
         (runtime (nth 1 frames)))
    (should (= 2 (length frames)))
    (should (eq (plist-get (cddr usage) :outcome) 'usage-error))
    (should (equal (plist-get (cddr usage) :status) 400))
    (should (eq (plist-get (cddr runtime) :outcome) 'runtime-error))
    (should (equal (plist-get (cddr runtime) :status) 500))))

(ert-deftest clime-test-pipe/worker-reuses-stream-for-sequential-ids ()
  "A warm worker handles sequential frames and preserves response ids in order."
  (clime-test-pipe--require)
  (let ((frames (clime-test-pipe--run-worker
                 (clime-test-pipe--request "one" '("echo" "Ada") nil nil)
                 (clime-test-pipe--request "two" '("echo" "Grace") nil nil))))
    (should (= 2 (length frames)))
    (should (equal (plist-get (cddr (nth 0 frames)) :id) "one"))
    (should (equal (plist-get (cddr (nth 1 frames)) :id) "two"))
    (should (eq (plist-get (cddr (nth 0 frames)) :outcome) 'completed))
    (should (eq (plist-get (cddr (nth 1 frames)) :outcome) 'completed))))

(ert-deftest clime-test-pipe/malformed-frame-survives-worker-loop ()
  "Malformed frames produce 400 responses and do not stop later requests."
  (clime-test-pipe--require)
  (let* ((frames (clime-test-pipe--run-worker
                  '(:not-a-clime-request 1 :id "bad")
                  (clime-test-pipe--request "ok" '("echo" "Ada") nil nil)))
         (bad (nth 0 frames))
         (ok (nth 1 frames)))
    (should (= 2 (length frames)))
    (should (equal (plist-get (cddr bad) :id) "bad"))
    (should (eq (plist-get (cddr bad) :outcome) 'rejected))
    (should (equal (plist-get (cddr bad) :status) 400))
    (should (eq (plist-get (cddr ok) :outcome) 'completed))))

(ert-deftest clime-test-pipe/rejects-callables-in-values-and-metadata ()
  "Callable payloads are rejected as structured 400 responses."
  (clime-test-pipe--require)
  (let* ((byte-code (byte-compile (lambda () "nope")))
         (frames (clime-test-pipe--run-worker
                  (clime-test-pipe--request
                   "value-bytecode" '("echo")
                   `((:name name :value ,byte-code :source pipe))
                   nil)
                  (clime-test-pipe--request
                   "metadata-bytecode" '("echo" "Ada")
                   nil
                   (list :callback byte-code))
                  (clime-test-pipe--request
                   "record-data" '("echo" "Ada")
                   nil
                   (list :record (record 'clime-pipe-test "data"))))))
    (should (= 3 (length frames)))
    (should (eq (plist-get (cddr (nth 0 frames)) :outcome) 'rejected))
    (should (equal (plist-get (cddr (nth 0 frames)) :status) 400))
    (should (eq (plist-get (cddr (nth 1 frames)) :outcome) 'rejected))
    (should (equal (plist-get (cddr (nth 1 frames)) :status) 400))
    ;; Records are inert data; dispatch accepts them unless a slot is callable.
    (should (eq (plist-get (cddr (nth 2 frames)) :outcome) 'completed))))

(defvar clime-test-pipe--read-eval-marker nil
  "Marker used to verify pipe reads do not evaluate #. forms.")

(ert-deftest clime-test-pipe/read-eval-syntax-is-not-executed ()
  "Read-time eval syntax is rejected without executing arbitrary Elisp."
  (clime-test-pipe--require)
  (let ((clime-test-pipe--read-eval-marker nil))
    (with-temp-buffer
      (insert "#.(setq clime-test-pipe--read-eval-marker t)\n")
      (goto-char (point-min))
      (let ((input (current-buffer))
            (output (generate-new-buffer " *clime-pipe-output*")))
        (unwind-protect
            (progn
              (clime-pipe-run-worker clime-test--pipe-app input output)
              (with-current-buffer output
                (let* ((frames (clime-test-pipe--frames-from-string
                                (buffer-string)))
                       (response (car frames)))
                  (should-not clime-test-pipe--read-eval-marker)
                  (should (= 1 (length frames)))
                  (should (eq (plist-get (cddr response) :outcome) 'rejected))
                  (should (equal (plist-get (cddr response) :status) 400)))))
          (kill-buffer output))))))

(ert-deftest clime-test-pipe/explicit-pipe-policy-rejects-through-hook ()
  "Explicit unsupported pipe policies reject through the dispatch policy hook."
  (clime-test-pipe--require)
  (let ((calls 0))
    (let* ((app (clime-make-app
                 :name "pipe-policy"
                 :children
                 (list (cons "guard"
                             (clime-make-command
                              :name "guard"
                              :adapter-policies '((pipe . (:unsupported t)))
                              :handler (lambda (_ctx)
                                         (setq calls (1+ calls))
                                         "guard"))))))
           (frames (clime-test-pipe--run-worker-for-app
                    app (list (clime-test-pipe--request
                               "policy" '("guard") nil nil))))
           (response (car frames)))
      (should (= 1 (length frames)))
      (should (= calls 0))
      (should (eq (plist-get (cddr response) :outcome) 'rejected))
      (should (equal (plist-get (cddr response) :status) 400))
      (should (string-match-p "Unsupported pipe adapter policy"
                              (plist-get (cddr response) :error-message))))))

(ert-deftest clime-test-pipe/truncated-final-frame-yields-structured-error ()
  "A partial final frame produces a rejected response instead of an uncaught crash."
  (clime-test-pipe--require)
  (with-temp-buffer
    (insert "(:clime/pipe-request 1 :id \"bad\" :path (\"echo\"")
    (goto-char (point-min))
    (let ((input (current-buffer))
          (output (generate-new-buffer " *clime-pipe-output*")))
      (unwind-protect
          (progn
            (clime-pipe-run-worker clime-test--pipe-app input output)
            (with-current-buffer output
              (let* ((frames (clime-test-pipe--frames-from-string
                              (buffer-string)))
                     (response (car frames)))
                (should (= 1 (length frames)))
                (should (equal (plist-get (cddr response) :id) "bad"))
                (should (eq (plist-get (cddr response) :outcome) 'rejected))
                (should (equal (plist-get (cddr response) :status) 400)))))
        (kill-buffer output)))))

(provide 'clime-pipe-tests)
;;; clime-pipe-tests.el ends here
