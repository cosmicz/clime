;;; clime-run-tests.el --- Tests for clime-run  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dispatch/runner layer.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-dsl)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Test Fixtures ──────────────────────────────────────────────────────

(defvar clime-test--run-output nil
  "Captures output from clime-run during tests.")

(defmacro clime-test-with-run-output (&rest body)
  "Execute BODY capturing princ output to `clime-test--run-output'."
  (declare (indent 0) (debug t))
  `(let ((clime-test--run-output ""))
     (cl-letf (((symbol-function 'princ)
                (lambda (obj &optional _stream)
                  (setq clime-test--run-output
                        (concat clime-test--run-output
                                (if (stringp obj) obj (format "%s" obj)))))))
       ,@body)))

(eval '(clime-app clime-test--run-app
         :version "1.0"
         (clime-option verbose ("-v" "--verbose") :count t)
         (clime-command echo
           :help "Echo a message"
           (clime-arg msg :help "Message to echo")
           (clime-handler (ctx)
             (clime-ctx-get ctx 'msg)))
         (clime-command fail
           :help "Always fails"
           (clime-handler (ctx)
             (error "Something went wrong")))
         (clime-command quiet
           :help "Returns nil"
           (clime-handler (ctx)
             nil)))
      t)

;;; ─── Success Cases ──────────────────────────────────────────────────────

(ert-deftest clime-test-run/success-returns-0 ()
  "Successful command returns exit code 0."
  (let ((code (clime-run clime-test--run-app '("echo" "hello"))))
    (should (= code 0))))

(ert-deftest clime-test-run/handler-receives-context ()
  "Handler receives a clime-context with parsed params."
  (clime-test-with-run-output
    (clime-run clime-test--run-app '("echo" "hello"))
    (should (equal clime-test--run-output "hello"))))

(ert-deftest clime-test-run/nil-return-no-output ()
  "Handler returning nil produces no output."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("quiet"))))
      (should (= code 0))
      (should (equal clime-test--run-output "")))))

(ert-deftest clime-test-run/root-options-in-context ()
  "Root options are available in handler context."
  (let* ((captured-ctx nil)
         (opt (clime-make-option :name 'verbose :flags '("-v") :count t))
         (cmd (clime-make-command :name "test" :handler
                                  (lambda (ctx) (setq captured-ctx ctx) nil)))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "test" cmd)))))
    (clime-run app '("-v" "test"))
    (should (= (clime-ctx-get captured-ctx 'verbose) 1))))

;;; ─── Error Handling ─────────────────────────────────────────────────────

(ert-deftest clime-test-run/usage-error-returns-2 ()
  "Usage error returns exit code 2."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("echo"))))  ;; missing arg
      (should (= code 2)))))

(ert-deftest clime-test-run/usage-error-prints-message ()
  "Usage error prints error message to stderr (message)."
  (let ((msgs (clime-test-with-messages
                (clime-run clime-test--run-app '("echo")))))
    (should (cl-some (lambda (m) (string-match-p "Error:" m)) msgs))))

(ert-deftest clime-test-run/unknown-option-returns-2 ()
  "Unknown option returns exit code 2."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("echo" "--bogus" "hi"))))
      (should (= code 2)))))

(ert-deftest clime-test-run/runtime-error-returns-1 ()
  "Runtime error in handler returns exit code 1."
  (let ((debug-on-error nil))
    (clime-test-with-run-output
      (let ((code (clime-run clime-test--run-app '("fail"))))
        (should (= code 1))))))

(ert-deftest clime-test-run/runtime-error-prints-message ()
  "Runtime error prints error message to stderr (message)."
  (let ((debug-on-error nil))
    (let ((msgs (clime-test-with-messages
                  (clime-run clime-test--run-app '("fail")))))
      (should (cl-some (lambda (m) (string-match-p "Error:" m)) msgs)))))

;;; ─── Help / Version ─────────────────────────────────────────────────────

(ert-deftest clime-test-run/help-returns-0 ()
  "--help returns exit code 0."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("--help"))))
      (should (= code 0)))))

(ert-deftest clime-test-run/version-returns-0 ()
  "--version returns exit code 0."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("--version"))))
      (should (= code 0))
      (should (string-match-p "1\\.0" clime-test--run-output)))))

(ert-deftest clime-test-run/help-at-command-returns-0 ()
  "--help at command level returns exit code 0."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("echo" "--help"))))
      (should (= code 0)))))

;;; ─── Debug Mode ────────────────────────────────────────────────────────

(ert-deftest clime-test-run/debug-mode-propagates-error ()
  "When debug-on-error is non-nil, runtime errors re-signal."
  (let ((debug-on-error t))
    (should-error (clime-run clime-test--run-app '("fail"))
                  :type 'error)))

;;; ─── Error Formatter Slot ──────────────────────────────────────────────

(ert-deftest clime-test-run/error-formatter-slot ()
  "Rebinding clime-format-error changes error output."
  (let* ((captured '())
         (clime-format-error (lambda (msg) (push msg captured))))
    (clime-run clime-test--run-app '("echo"))  ;; missing arg
    (should (cl-some (lambda (m) (string-match-p "Missing" m)) captured))))

;;; ─── Output Protocol ───────────────────────────────────────────────────

(ert-deftest clime-test-run/non-string-return-princ ()
  "Handler returning a non-string value gets princ'd."
  (let* ((cmd (clime-make-command :name "num" :handler (lambda (_ctx) 42)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "num" cmd)))))
    (clime-test-with-run-output
      (let ((code (clime-run app '("num"))))
        (should (= code 0))
        (should (equal clime-test--run-output "42"))))))

;;; ─── Help Output Content ──────────────────────────────────────────────

(ert-deftest clime-test-run/help-prints-usage ()
  "--help prints Usage line to stdout."
  (clime-test-with-run-output
    (clime-run clime-test--run-app '("--help"))
    (should (string-match-p "Usage:" clime-test--run-output))))

(ert-deftest clime-test-run/version-prints-app-name ()
  "--version output includes app name."
  (clime-test-with-run-output
    (clime-run clime-test--run-app '("--version"))
    (should (string-match-p "clime-test--run-app" clime-test--run-output))))

(ert-deftest clime-test-run/no-args-shows-help ()
  "App with children and no args triggers help (exit 0)."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '())))
      (should (= code 0))
      (should (string-match-p "Usage:" clime-test--run-output)))))

;;; ─── Group Dispatch ───────────────────────────────────────────────────

(ert-deftest clime-test-run/group-dispatch ()
  "Dispatch through a group to a leaf command."
  (let* ((cmd (clime-make-command :name "add" :handler
                                  (lambda (ctx) (clime-ctx-get ctx 'id))))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "dep" grp)))))
    (setf (clime-command-args cmd)
          (list (clime-make-arg :name 'id)))
    (clime-test-with-run-output
      (let ((code (clime-run app '("dep" "add" "foo"))))
        (should (= code 0))
        (should (equal clime-test--run-output "foo"))))))

;;; ─── Error Formatter Edge Cases ───────────────────────────────────────

(ert-deftest clime-test-run/error-formatter-runtime ()
  "Error formatter slot works for runtime errors too."
  (let* ((debug-on-error nil)
         (captured nil)
         (clime-format-error (lambda (msg) (setq captured msg))))
    (clime-run clime-test--run-app '("fail"))
    (should (stringp captured))
    (should (string-match-p "went wrong" captured))))

(ert-deftest clime-test-run/debug-mode-does-not-affect-usage-error ()
  "debug-on-error does NOT propagate usage errors (they are expected)."
  (let ((debug-on-error t))
    (let ((code (clime-run clime-test--run-app '("echo"))))
      (should (= code 2)))))

;;; ─── Group Invoke Dispatch ───────────────────────────────────────────

(ert-deftest clime-test-run/group-invoke-dispatch ()
  "Group with :handler handler dispatches through clime-run."
  (let* ((cmd (clime-make-command :name "detail" :handler
                                  (lambda (_ctx) "detailed")))
         (grp (clime-make-group :name "status"
                                :handler (lambda (_ctx) "overview")
                                :children (list (cons "detail" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "status" grp)))))
    (clime-test-with-run-output
      (let ((code (clime-run app '("status"))))
        (should (= code 0))
        (should (equal clime-test--run-output "overview"))))))

(ert-deftest clime-test-run/group-invoke-subcommand-still-works ()
  "Group with :handler still dispatches to subcommand when given."
  (let* ((cmd (clime-make-command :name "detail" :handler
                                  (lambda (_ctx) "detailed")))
         (grp (clime-make-group :name "status"
                                :handler (lambda (_ctx) "overview")
                                :children (list (cons "detail" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "status" grp)))))
    (clime-test-with-run-output
      (let ((code (clime-run app '("status" "detail"))))
        (should (= code 0))
        (should (equal clime-test--run-output "detailed"))))))

;;; ─── Run Batch ──────────────────────────────────────────────────────

(ert-deftest clime-test-run/run-batch-strips-leading-dash-dash ()
  "clime-run-batch strips leading \"--\" from command-line-args-left."
  (let* ((exit-code nil)
         (cmd (clime-make-command :name "echo"
                                  :handler (lambda (ctx) (clime-ctx-get ctx 'msg))
                                  :args (list (clime-make-arg :name 'msg))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "echo" cmd))))
         (command-line-args-left '("--" "echo" "hi")))
    (cl-letf (((symbol-function 'kill-emacs)
               (lambda (code) (setq exit-code code))))
      (clime-test-with-run-output
        (clime-run-batch app)
        (should (= exit-code 0))
        (should (equal clime-test--run-output "hi"))
        ;; command-line-args-left should be cleared
        (should (null command-line-args-left))))))

;;; ─── Help Before Subcommand ─────────────────────────────────────────

(ert-deftest clime-test-run/help-before-subcommand ()
  "--help install shows install help, same as install --help."
  (clime-test-with-run-output
    (let ((code (clime-run clime-test--run-app '("--help" "echo"))))
      (should (= code 0))
      (should (string-match-p "Echo a message" clime-test--run-output)))))

(ert-deftest clime-test-run/help-before-group-subcommand ()
  "--help group cmd descends into group then command."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore
                                  :help "Add a dep"))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "dep" grp)))))
    (clime-test-with-run-output
      (let ((code (clime-run app '("--help" "dep" "add"))))
        (should (= code 0))
        (should (string-match-p "Add a dep" clime-test--run-output))
        (should (string-match-p "Usage: t dep add" clime-test--run-output))))))

;;; ─── Missing Subcommand Shows Help ──────────────────────────────────

(ert-deftest clime-test-run/missing-subcommand-shows-help ()
  "Group without handler shows help when no subcommand given."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "dep" grp)))))
    (clime-test-with-run-output
      (let ((code (clime-run app '("dep"))))
        (should (= code 0))
        (should (string-match-p "Usage: t dep" clime-test--run-output))
        (should (string-match-p "Commands:" clime-test--run-output))))))

;;; ─── Usage Error Hint ──────────────────────────────────────────────

(ert-deftest clime-test-run/usage-error-shows-help-hint ()
  "Usage errors include a 'Try ... --help' hint."
  (let ((msgs (clime-test-with-messages
                (clime-run clime-test--run-app '("echo")))))
    (should (cl-some (lambda (m) (string-match-p "Try.*--help" m)) msgs))))

(ert-deftest clime-test-run/usage-error-hint-includes-path ()
  "Usage error hint includes the correct command path."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id))))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "dep" grp)))))
    (let ((msgs (clime-test-with-messages
                  (clime-run app '("dep" "add")))))
      (should (cl-some (lambda (m) (string-match-p "t dep add --help" m)) msgs)))))

(provide 'clime-run-tests)
;;; clime-run-tests.el ends here
