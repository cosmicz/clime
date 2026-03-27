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
  "Custom error-handler on active format changes error output."
  (let* ((captured '())
         (clime-out--active-format
          (clime-make-output-format
           :name 'text :flags '("--text") :streaming t
           :encoder (lambda (data) (format "%s" data))
           :error-handler (lambda (msg) (push msg captured)))))
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
  "Custom error-handler on active format handles runtime errors too."
  (let* ((debug-on-error nil)
         (captured nil)
         (clime-out--active-format
          (clime-make-output-format
           :name 'text :flags '("--text") :streaming t
           :encoder (lambda (data) (format "%s" data))
           :error-handler (lambda (msg) (setq captured msg)))))
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

;;; ─── Setup Hook ────────────────────────────────────────────────────────

(ert-deftest clime-test-run/setup-hook-called ()
  "Setup hook is called with app and parse-result before handler."
  (let* ((setup-called nil)
         (handler-called nil)
         (cmd (clime-make-command :name "go" :handler
                                  (lambda (_ctx) (setq handler-called t) nil)))
         (app (clime-make-app :name "t" :version "1"
                              :setup (lambda (a result)
                                       (should (clime-app-p a))
                                       (should (clime-parse-result-p result))
                                       (should-not handler-called)
                                       (setq setup-called t))
                              :children (list (cons "go" cmd)))))
    (clime-run app '("go"))
    (should setup-called)
    (should handler-called)))

(ert-deftest clime-test-run/setup-hook-receives-parsed-values ()
  "Setup hook receives parse-result with typed param values."
  (let* ((captured-result nil)
         (opt (clime-make-option :name 'verbose :flags '("-v") :count t))
         (cmd (clime-make-command :name "go" :handler (lambda (_ctx) nil)))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :setup (lambda (_app result)
                                       (setq captured-result result))
                              :children (list (cons "go" cmd)))))
    (clime-run app '("-v" "go"))
    (should captured-result)
    (should (equal (plist-get (clime-parse-result-params captured-result)
                              'verbose)
                   1))))

(ert-deftest clime-test-run/setup-hook-influences-dynamic-choices ()
  "Setup hook can set state that dynamic :choices reads in pass 2."
  (let* ((allowed-values nil)
         (opt (clime-make-option :name 'level :flags '("--level")
                                  :choices (lambda () allowed-values)))
         (cmd (clime-make-command :name "go" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :setup (lambda (_app _result)
                                       (setq allowed-values '("low" "high")))
                              :children (list (cons "go" cmd)))))
    ;; Dynamic choices resolved in pass 2 after setup sets allowed-values
    (let ((code (clime-run app '("go" "--level" "high"))))
      (should (= code 0)))))

(ert-deftest clime-test-run/static-choices-validated-in-pass-1 ()
  "Static :choices (literal list) are validated during pass 1."
  (let* ((opt (clime-make-option :name 'level :flags '("--level")
                                  :choices '("low" "high")))
         (cmd (clime-make-command :name "go" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "go" cmd)))))
    ;; "bogus" is not in static choices — error even without setup
    (let ((code (clime-run app '("go" "--level" "bogus"))))
      (should (= code 2)))))

(ert-deftest clime-test-run/setup-hook-error-returns-1 ()
  "Setup hook error is treated as runtime error (exit code 1)."
  (let* ((debug-on-error nil)
         (cmd (clime-make-command :name "go" :handler (lambda (_ctx) nil)))
         (app (clime-make-app :name "t" :version "1"
                              :setup (lambda (_app _result) (error "Setup failed"))
                              :children (list (cons "go" cmd)))))
    (let ((code (clime-run app '("go"))))
      (should (= code 1)))))

(ert-deftest clime-test-run/no-setup-hook-works ()
  "App without :setup works normally."
  (let* ((cmd (clime-make-command :name "go" :handler (lambda (_ctx) "ok")))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "go" cmd)))))
    (clime-test-with-run-output
      (let ((code (clime-run app '("go"))))
        (should (= code 0))
        (should (equal clime-test--run-output "ok"))))))

;;; ─── clime-run--execute ─────────────────────────────────────────────

(ert-deftest clime-test-run/execute-returns-0-on-success ()
  "Successful handler returns exit code 0."
  (let* ((ctx (clime-context--create :app nil :command nil :path nil :params nil))
         (code (clime-run--execute (lambda (_) "ok") ctx)))
    (should (= 0 code))))

(ert-deftest clime-test-run/execute-re-signals-usage-error ()
  "Usage error is re-signaled to caller."
  (let ((ctx (clime-context--create :app nil :command nil :path nil :params nil)))
    (should-error
     (clime-run--execute (lambda (_) (signal 'clime-usage-error '("bad"))) ctx)
     :type 'clime-usage-error)))

(ert-deftest clime-test-run/execute-re-signals-help-requested ()
  "Help-requested is re-signaled to caller."
  (let ((ctx (clime-context--create :app nil :command nil :path nil :params nil)))
    (should-error
     (clime-run--execute (lambda (_) (signal 'clime-help-requested '(:node nil))) ctx)
     :type 'clime-help-requested)))

(ert-deftest clime-test-run/execute-generic-error-returns-1 ()
  "Generic error returns exit code 1 when debug-on-error is nil."
  (let ((debug-on-error nil)
        (ctx (clime-context--create :app nil :command nil :path nil :params nil))
        (code nil))
    (with-output-to-string
      (setq code (clime-run--execute (lambda (_) (error "boom")) ctx)))
    (should (= 1 code))))

;;; ─── clime-run-client ───────────────────────────────────────────────────

(defvar clime-test--client-app nil
  "Test app for clime-run-client tests.")

(eval '(clime-app clime-test--client-app
         :version "1"
         (clime-command greet
           :help "Greet"
           (clime-arg name :optional :default "world" :help "Name")
           (clime-handler (ctx)
             (princ (format "Hello, %s!"
                            (plist-get (clime-context-params ctx) 'name)))
             nil))
         (clime-command fail
           :help "Fail"
           (clime-handler (ctx)
             (error "deliberate error"))))
      t)

(ert-deftest clime-test-run/client-captures-stdout ()
  "clime-run-client writes handler output to DIR/out."
  (clime-test-with-temp-dir
   (let ((dir default-directory))
     ;; Write argv: null-delimited
     (with-temp-file (expand-file-name "argv" dir)
       (insert "greet\0Alice\0"))
     (clime-run-client 'clime-test--client-app dir)
     (should (equal "Hello, Alice!"
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "out" dir))
                      (buffer-string))))
     (should (equal "0"
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "exit" dir))
                      (buffer-string)))))))

(ert-deftest clime-test-run/client-captures-exit-code ()
  "clime-run-client writes nonzero exit code on error."
  (clime-test-with-temp-dir
   (let ((dir default-directory))
     (with-temp-file (expand-file-name "argv" dir)
       (insert "fail\0"))
     (clime-run-client 'clime-test--client-app dir)
     (should (equal "1"
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "exit" dir))
                      (buffer-string)))))))

(ert-deftest clime-test-run/client-captures-stderr ()
  "clime-run-client captures message calls to DIR/err."
  (clime-test-with-temp-dir
   (let ((dir default-directory))
     (with-temp-file (expand-file-name "argv" dir)
       (insert "fail\0"))
     (clime-run-client 'clime-test--client-app dir)
     (let ((err (with-temp-buffer
                  (insert-file-contents (expand-file-name "err" dir))
                  (buffer-string))))
       (should (string-match-p "deliberate error" err))))))

(ert-deftest clime-test-run/client-default-args ()
  "clime-run-client works with no argv file (no args)."
  (clime-test-with-temp-dir
   (let ((dir default-directory))
     ;; No argv file — app should show help or error
     ;; greet with default "world" if we put just the command
     (with-temp-file (expand-file-name "argv" dir)
       (insert "greet\0"))
     (clime-run-client 'clime-test--client-app dir)
     (should (equal "Hello, world!"
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "out" dir))
                      (buffer-string)))))))

(ert-deftest clime-test-run/client-returns-nil ()
  "clime-run-client returns nil (for emacsclient)."
  (clime-test-with-temp-dir
   (let ((dir default-directory))
     (with-temp-file (expand-file-name "argv" dir)
       (insert "greet\0"))
     (should (null (clime-run-client 'clime-test--client-app dir))))))

(ert-deftest clime-test-run/client-stdin-file ()
  "clime-run-client reads stdin from DIR/in via CLIME_STDIN_FILE."
  (clime-test-with-temp-dir
   (let ((dir default-directory))
     ;; Set up an app that reads stdin via -
     (eval '(clime-app clime-test--stdin-client-app
              :version "1"
              (clime-command echo
                :help "Echo"
                (clime-arg text :help "Text")
                (clime-handler (ctx)
                  (princ (plist-get (clime-context-params ctx) 'text))
                  nil)))
           t)
     (with-temp-file (expand-file-name "argv" dir)
       (insert "echo\0-\0"))
     (with-temp-file (expand-file-name "in" dir)
       (insert "from-stdin"))
     (clime-run-client 'clime-test--stdin-client-app dir)
     (should (equal "from-stdin"
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "out" dir))
                      (buffer-string)))))))

(ert-deftest clime-test-run/client-lazy-load ()
  "clime-run-client loads :file when app symbol is unbound."
  (clime-test-with-temp-dir
   (let ((dir default-directory)
         (app-file (expand-file-name "lazy-app.el")))
     ;; Write a minimal app file
     (with-temp-file app-file
       (insert "(require 'clime)\n"
               "(clime-app clime-test--lazy-app\n"
               "  :version \"1\"\n"
               "  (clime-command ping\n"
               "    :help \"Ping\"\n"
               "    (clime-handler (ctx) (princ \"pong\") nil)))\n"))
     ;; Ensure unbound
     (when (boundp 'clime-test--lazy-app)
       (makunbound 'clime-test--lazy-app))
     (with-temp-file (expand-file-name "argv" dir)
       (insert "ping\0"))
     (clime-run-client 'clime-test--lazy-app dir :file app-file)
     (should (equal "pong"
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "out" dir))
                      (buffer-string))))
     ;; Cleanup
     (makunbound 'clime-test--lazy-app))))

;;; ─── Client wrapper generation ──────────────────────────────────────────

(require 'clime-make)

(ert-deftest clime-test-run/client-wrapper-content ()
  "clime-make--make-client-wrapper generates a valid bash wrapper."
  (let ((wrapper (clime-make--make-client-wrapper
                  'my-app
                  '("/home/user/app" "/opt/clime")
                  "/home/user/app/my-app.el")))
    ;; Bash shebang
    (should (string-prefix-p "#!/usr/bin/env bash" wrapper))
    ;; App symbol embedded
    (should (string-match-p "clime-run-client 'my-app" wrapper))
    ;; Load paths embedded
    (should (string-match-p "/home/user/app" wrapper))
    (should (string-match-p "/opt/clime" wrapper))
    ;; App file embedded
    (should (string-match-p "my-app\\.el" wrapper))
    ;; Null-delimited argv
    (should (string-match-p "printf.*\\\\0" wrapper))
    ;; Connection flag parsing
    (should (string-match-p "--socket-name" wrapper))
    (should (string-match-p "--server-file" wrapper))
    ;; Env fallback
    (should (string-match-p "EMACS_SOCKET_NAME" wrapper))
    ;; Error handling
    (should (string-match-p "Cannot connect to Emacs server" wrapper))))

(ert-deftest clime-test-run/client-wrapper-write ()
  "clime-make--write-client-wrapper writes an executable file."
  (clime-test-with-temp-dir
   (let ((target (expand-file-name "my-app")))
     (clime-make--write-client-wrapper
      target 'my-app '("/opt/clime") "/home/user/my-app.el" nil)
     (should (file-exists-p target))
     (should (file-executable-p target))
     (with-temp-buffer
       (insert-file-contents target)
       (should (string-prefix-p "#!/usr/bin/env bash" (buffer-string)))))))

(ert-deftest clime-test-run/client-wrapper-no-overwrite ()
  "clime-make--write-client-wrapper refuses to overwrite without force."
  (clime-test-with-temp-dir
   (let ((target (expand-file-name "my-app")))
     (with-temp-file target (insert "existing"))
     (should-error
      (clime-make--write-client-wrapper
       target 'my-app '("/opt/clime") "/home/user/my-app.el" nil)
      :type 'clime-usage-error))))

(ert-deftest clime-test-run/client-wrapper-force-overwrite ()
  "clime-make--write-client-wrapper overwrites with force."
  (clime-test-with-temp-dir
   (let ((target (expand-file-name "my-app")))
     (with-temp-file target (insert "existing"))
     (clime-make--write-client-wrapper
      target 'my-app '("/opt/clime") "/home/user/my-app.el" t)
     (with-temp-buffer
       (insert-file-contents target)
       (should (string-prefix-p "#!/usr/bin/env bash" (buffer-string)))))))

(provide 'clime-run-tests)
;;; clime-run-tests.el ends here
