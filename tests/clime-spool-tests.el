;;; clime-spool-tests.el --- Tests for file-spool transport  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the readable file-spool transport over the shared dispatch core.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-dsl)

(defconst clime-test-spool--serve-feature-before-require (featurep 'clime-serve)
  "Whether `clime-serve' was loaded before requiring `clime-spool'.")

(defconst clime-test-spool--web-server-feature-before-require (featurep 'web-server)
  "Whether `web-server' was loaded before requiring `clime-spool'.")

(condition-case nil
    (require 'clime-spool)
  (error nil))

(defconst clime-test-spool--serve-feature-after-require (featurep 'clime-serve)
  "Whether `clime-serve' was loaded by requiring `clime-spool'.")

(defconst clime-test-spool--web-server-feature-after-require (featurep 'web-server)
  "Whether `web-server' was loaded by requiring `clime-spool'.")

(defun clime-test-spool--require ()
  "Assert the spool transport API is available."
  (should (featurep 'clime-spool))
  (should (fboundp 'clime-spool-request-from-frame))
  (should (fboundp 'clime-spool-response-frame))
  (should (fboundp 'clime-spool-submit-request))
  (should (fboundp 'clime-spool-process-one))
  (should (fboundp 'clime-spool-cleanup-stale)))

(eval '(clime-app clime-test--spool-app
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

(defmacro clime-test-spool--with-root (root &rest body)
  "Bind ROOT to a fresh temporary spool directory while running BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,root (make-temp-file "clime-spool-test-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,root)
         (delete-directory ,root t)))))

(defun clime-test-spool--request (id path &optional inputs metadata auth)
  "Build a spool request frame with ID, PATH, INPUTS, METADATA, and AUTH."
  `(:clime/spool-request 1
    :id ,id
    :path ,path
    :inputs ,inputs
    :metadata ,metadata
    :auth ,auth))

(defun clime-test-spool--read-frame (file)
  "Read one frame from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((read-eval nil))
      (read (current-buffer)))))

(defun clime-test-spool--response-file (root id)
  "Return response file path for ID under ROOT."
  (expand-file-name (concat id ".response")
                    (expand-file-name "responses" root)))

(defun clime-test-spool--request-file (root id)
  "Return request file path for ID under ROOT."
  (expand-file-name (concat id ".request")
                    (expand-file-name "requests" root)))

(defun clime-test-spool--write-request (root frame)
  "Write FRAME as a complete request file under ROOT."
  (clime-spool-write-request-file root (plist-get (cddr frame) :id) frame))

(defun clime-test-spool--process (root &optional capability)
  "Process one request under ROOT using optional CAPABILITY."
  (clime-spool-process-one clime-test--spool-app root nil
                           :capability capability))

(ert-deftest clime-test-spool/request-frame-normalizes-to-dispatch-request ()
  "Spool request frames become transport-neutral dispatch requests."
  (clime-test-spool--require)
  (let* ((frame (clime-test-spool--request
                 "req-1" '("echo")
                 '((:name name :value "Ada" :source spool))
                 '(:client "sandbox-1")
                 '(:capability "secret")))
         (request (clime-spool-request-from-frame frame)))
    (should (memq 'spool clime-dispatch-adapters))
    (should (clime-dispatch-request-p request))
    (should (eq (clime-dispatch-request-surface request) 'serve))
    (should (eq (clime-dispatch-request-adapter request) 'spool))
    (should (equal (clime-dispatch-request-path request) '("echo")))
    (should (equal (clime-dispatch-request-correlation-id request) "req-1"))
    (should (equal (clime-dispatch-request-auth-context request)
                   '(:capability "secret")))
    (should (eq clime-test-spool--serve-feature-after-require
                clime-test-spool--serve-feature-before-require))
    (should (eq clime-test-spool--web-server-feature-after-require
                clime-test-spool--web-server-feature-before-require))))

(ert-deftest clime-test-spool/client-writes-request-atomically ()
  "Client request writes leave a complete final file and no tmp request."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (let ((frame (clime-test-spool--request "req-1" '("echo" "Ada"))))
      (clime-spool-submit-request root frame :wait nil)
      (should (file-exists-p (clime-test-spool--request-file root "req-1")))
      (should-not (directory-files (expand-file-name "requests" root)
                                   nil "\\.tmp-"))
      (should (equal (clime-test-spool--read-frame
                      (clime-test-spool--request-file root "req-1"))
                     frame)))))

(ert-deftest clime-test-spool/process-one-dispatches-and-writes-response ()
  "Authority claims one complete request and writes a correlated response."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (clime-test-spool--write-request
     root (clime-test-spool--request "req-1" '("echo" "Ada")))
    (should (equal (clime-test-spool--process root) "req-1"))
    (should-not (file-exists-p (clime-test-spool--request-file root "req-1")))
    (let ((response (clime-test-spool--read-frame
                     (clime-test-spool--response-file root "req-1"))))
      (should (eq (car response) :clime/spool-response))
      (should (equal (plist-get (cddr response) :id) "req-1"))
      (should (eq (plist-get (cddr response) :outcome) 'completed))
      (should (equal (plist-get (cddr response) :status) 200))
      (should (string-match-p "hello:Ada"
                              (plist-get (cddr response) :body))))))

(ert-deftest clime-test-spool/warm-authority-processes-sequential-requests ()
  "Sequential scans reuse the same warm app and preserve response ids."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (clime-test-spool--write-request
     root (clime-test-spool--request "one" '("echo" "Ada")))
    (clime-test-spool--write-request
     root (clime-test-spool--request "two" '("echo" "Grace")))
    (should (equal (sort (list (clime-test-spool--process root)
                               (clime-test-spool--process root))
                         #'string<)
                   '("one" "two")))
    (should (file-exists-p (clime-test-spool--response-file root "one")))
    (should (file-exists-p (clime-test-spool--response-file root "two")))))

(ert-deftest clime-test-spool/capability-rejects-before-route-lookup ()
  "Configured capability rejects missing auth before route walking."
  (clime-test-spool--require)
  (let ((invocations 0))
    (let* ((app (clime-make-app
                 :name "spool-auth"
                 :on-invocation (lambda (_event)
                                  (setq invocations (1+ invocations)))
                 :children
                 (list (cons "known"
                             (clime-make-command
                              :name "known"
                              :handler (lambda (_ctx) "known")))))))
      (clime-test-spool--with-root root
        (clime-test-spool--write-request
         root (clime-test-spool--request "auth" '("missing-route")
                                         nil nil '(:capability "wrong")))
        (should (equal (clime-spool-process-one app root nil
                                                :capability "secret")
                       "auth"))
        (let ((response (clime-test-spool--read-frame
                         (clime-test-spool--response-file root "auth"))))
          (should (= invocations 0))
          (should (eq (plist-get (cddr response) :outcome) 'rejected))
          (should (equal (plist-get (cddr response) :status) 401)))))))

(ert-deftest clime-test-spool/rejects-callables-in-values-metadata-and-auth ()
  "Callable payloads are rejected as structured 400 responses."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (let ((byte-code (byte-compile (lambda () "nope"))))
      (clime-test-spool--write-request
       root
       (clime-test-spool--request
        "value-callable" '("echo")
        `((:name name :value ((nested [,byte-code])) :source spool))))
      (clime-test-spool--write-request
       root
       (clime-test-spool--request
        "metadata-callable" '("echo" "Ada")
        nil (list :callbacks (vector byte-code))))
      (clime-test-spool--write-request
       root
       (clime-test-spool--request
        "auth-callable" '("echo" "Ada")
        nil nil (list :capability "secret" :callback byte-code))))
    (dotimes (_ 3)
      (should (clime-test-spool--process root)))
    (dolist (id '("value-callable" "metadata-callable" "auth-callable"))
      (let ((response (clime-test-spool--read-frame
                       (clime-test-spool--response-file root id))))
        (should (eq (plist-get (cddr response) :outcome) 'rejected))
        (should (equal (plist-get (cddr response) :status) 400))))))

(ert-deftest clime-test-spool/malformed-frame-is-quarantined-and-loop-continues ()
  "Malformed request files are quarantined and later requests still run."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (clime-spool-ensure-root root)
    (with-temp-file (clime-test-spool--request-file root "bad")
      (insert "(:clime/spool-request 999 :id \"bad\" :path (\"echo\"))\n"))
    (clime-test-spool--write-request
     root (clime-test-spool--request "ok" '("echo" "Ada")))
    (should (equal (clime-test-spool--process root) "bad"))
    (should (file-exists-p (expand-file-name "bad.request"
                                             (expand-file-name "quarantine" root))))
    (should (file-exists-p (clime-test-spool--response-file root "bad")))
    (should (equal (clime-test-spool--process root) "ok"))
    (let ((response (clime-test-spool--read-frame
                     (clime-test-spool--response-file root "ok"))))
      (should (eq (plist-get (cddr response) :outcome) 'completed)))))

(defvar clime-test-spool--read-eval-marker nil
  "Marker used to verify spool reads do not evaluate #. forms.")

(ert-deftest clime-test-spool/read-eval-syntax-is-not-executed ()
  "Read-time eval syntax is rejected without executing arbitrary Elisp."
  (clime-test-spool--require)
  (let ((clime-test-spool--read-eval-marker nil))
    (clime-test-spool--with-root root
      (clime-spool-ensure-root root)
      (with-temp-file (clime-test-spool--request-file root "eval")
        (insert "(:clime/spool-request 1 :id \"eval\" :path (\"echo\") ")
        (insert ":metadata #.(setq clime-test-spool--read-eval-marker t))\n"))
      (should (equal (clime-test-spool--process root) "eval"))
      (let ((response (clime-test-spool--read-frame
                       (clime-test-spool--response-file root "eval"))))
        (should-not clime-test-spool--read-eval-marker)
        (should (eq (plist-get (cddr response) :outcome) 'rejected))
        (should (equal (plist-get (cddr response) :status) 400))))))

(ert-deftest clime-test-spool/unsafe-request-filename-is-quarantined ()
  "Unsafe request filename entries are quarantined without killing the loop."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (clime-spool-ensure-root root)
    (let ((unsafe (expand-file-name ".request"
                                    (expand-file-name "requests" root))))
      (with-temp-file unsafe
        (insert "(:clime/spool-request 1 :id \".\" :path (\"echo\"))\n"))
      (should-not (clime-test-spool--process root))
      (should (file-exists-p (expand-file-name
                              ".request"
                              (expand-file-name "quarantine" root)))))))

(ert-deftest clime-test-spool/symlink-request-is-quarantined-not-followed ()
  "Authority rejects symlink request entries before claim/read."
  (clime-test-spool--require)
  (skip-unless (fboundp 'make-symbolic-link))
  (clime-test-spool--with-root root
    (clime-spool-ensure-root root)
    (let ((outside (make-temp-file "clime-spool-outside-")))
      (unwind-protect
          (progn
            (with-temp-file outside
              (prin1 (clime-test-spool--request "link" '("echo" "Ada"))
                     (current-buffer)))
            (make-symbolic-link outside
                                (clime-test-spool--request-file root "link"))
            (should (equal (clime-test-spool--process root) "link"))
            (should (file-exists-p (expand-file-name
                                    "link.request"
                                    (expand-file-name "quarantine" root))))
            (should-not (file-exists-p
                         (clime-test-spool--response-file root "link"))))
        (when (file-exists-p outside)
          (delete-file outside))))))

(ert-deftest clime-test-spool/claim-by-rename-has-single-winner ()
  "Two claims of the same request yield one winner and one nil loser."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (clime-test-spool--write-request
     root (clime-test-spool--request "race" '("echo" "Ada")))
    (let ((request-file (clime-test-spool--request-file root "race")))
      (should (clime-spool-claim-request root request-file))
      (should-not (clime-spool-claim-request root request-file)))))

(ert-deftest clime-test-spool/cleanup-stays-inside-spool-root ()
  "Stale tmp and processing cleanup does not delete outside-root files."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (clime-spool-ensure-root root)
    (let ((outside (make-temp-file "clime-spool-keep-"))
          (tmp (expand-file-name "stale.request.tmp-test"
                                 (expand-file-name "requests" root)))
          (processing (expand-file-name "stale.request"
                                        (expand-file-name "processing" root))))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "tmp"))
            (with-temp-file processing (insert "processing"))
            (set-file-times tmp '(0 0))
            (set-file-times processing '(0 0))
            (clime-spool-cleanup-stale root 1)
            (should-not (file-exists-p tmp))
            (should-not (file-exists-p processing))
            (should (file-exists-p outside)))
        (when (file-exists-p outside)
          (delete-file outside))))))

(ert-deftest clime-test-spool/client-submit-waits-for-response ()
  "Client can submit and wait using only filesystem reads and writes."
  (clime-test-spool--require)
  (clime-test-spool--with-root root
    (let ((frame (clime-test-spool--request "req-1" '("echo" "Ada"))))
      (clime-spool-submit-request root frame :wait nil)
      (clime-test-spool--process root)
      (let ((response (clime-spool-wait-for-response root "req-1" 0.2)))
        (should (eq (plist-get (cddr response) :outcome) 'completed))
        (should (equal (plist-get (cddr response) :id) "req-1"))))))

(ert-deftest clime-test-spool/explicit-spool-policy-rejects-through-hook ()
  "Explicit unsupported spool policies reject through the dispatch policy hook."
  (clime-test-spool--require)
  (let ((calls 0))
    (let* ((app (clime-make-app
                 :name "spool-policy"
                 :children
                 (list (cons "guard"
                             (clime-make-command
                              :name "guard"
                              :adapter-policies '((spool . (:unsupported t)))
                              :handler (lambda (_ctx)
                                         (setq calls (1+ calls))
                                         "guard")))))))
      (clime-test-spool--with-root root
        (clime-test-spool--write-request
         root (clime-test-spool--request "policy" '("guard")))
        (should (equal (clime-spool-process-one app root nil) "policy"))
        (let ((response (clime-test-spool--read-frame
                         (clime-test-spool--response-file root "policy"))))
          (should (= calls 0))
          (should (eq (plist-get (cddr response) :outcome) 'rejected))
          (should (equal (plist-get (cddr response) :status) 400))
          (should (string-match-p "Unsupported spool adapter policy"
                                  (plist-get (cddr response)
                                             :error-message))))))))

(provide 'clime-spool-tests)
;;; clime-spool-tests.el ends here
