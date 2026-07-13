;;; clime-dotenv-tests.el --- Tests for .env file loading  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dotenv loader: parser grammar, file loading,
;; precedence rules (CLI > real env > .env > default), scoped
;; mutation of `process-environment', and integration with the
;; existing :env / :env-prefix machinery.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-dotenv)

;;; ─── Helpers ───────────────────────────────────────────────────────────

(defmacro clime-dotenv-test-with-file (path-var contents &rest body)
  "Write CONTENTS to a temp file, bind PATH-VAR to its path, run BODY.
File is deleted after BODY."
  (declare (indent 2))
  `(let ((,path-var (make-temp-file "clime-dotenv-test-" nil ".env")))
     (unwind-protect
         (progn
           (with-temp-file ,path-var (insert ,contents))
           ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var)))))

(defmacro clime-dotenv-test-with-env (bindings &rest body)
  "Set env BINDINGS for BODY, restore afterwards.
BINDINGS is a list of (VAR VALUE-OR-NIL).  Value nil unsets the var."
  (declare (indent 1))
  (let ((saved (gensym "saved")))
    `(let ((,saved (mapcar (lambda (b) (cons (car b) (getenv (car b))))
                           ',bindings)))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (b)
                         `(setenv ,(car b) ,(cadr b)))
                       bindings)
             ,@body)
         (dolist (pair ,saved)
           (setenv (car pair) (cdr pair)))))))

(defun clime-run-capture-param (app argv name)
  "Run APP with ARGV and return the value of param NAME from the handler.
The handler is replaced with a closure that stashes the params; if the
underlying command resolved to a different node, that node's handler is
ignored.  Returns nil if NAME is not in the captured params."
  (let* ((captured nil)
         (clime-out--active-format clime-out--active-format))
    (cl-labels ((replace-handlers (node)
                  (when (clime-node-p node)
                    (setf (clime-node-handler node)
                          (lambda (ctx)
                            (setq captured (clime-context-params ctx))
                            nil))
                    (when (clime-group-p node)
                      (dolist (entry (clime-group-children node))
                        (replace-handlers (cdr entry)))))))
      (replace-handlers app))
    (clime-run app argv)
    (plist-get captured name)))

;;; ─── Parser grammar ─────────────────────────────────────────────────────

(ert-deftest clime-test-dotenv/parse-bare-value ()
  "Bare KEY=value pairs are parsed."
  (should (equal (clime-dotenv-parse "FOO=bar\nBAZ=qux\n")
                 '(("FOO" . "bar") ("BAZ" . "qux")))))

(ert-deftest clime-test-dotenv/parse-blank-lines-and-comments ()
  "Blank lines and full-line # comments are ignored."
  (should (equal (clime-dotenv-parse "# header\n\nFOO=bar\n\n# tail\n")
                 '(("FOO" . "bar")))))

(ert-deftest clime-test-dotenv/parse-trailing-comment-bare ()
  "Trailing # comments on bare values are stripped."
  (should (equal (clime-dotenv-parse "FOO=bar # note\n")
                 '(("FOO" . "bar")))))

(ert-deftest clime-test-dotenv/parse-bare-trims-whitespace ()
  "Bare values have surrounding whitespace trimmed (both sides)."
  (should (equal (clime-dotenv-parse "FOO=bar   \n")
                 '(("FOO" . "bar"))))
  (should (equal (clime-dotenv-parse "FOO=  bar\n")
                 '(("FOO" . "bar"))))
  (should (equal (clime-dotenv-parse "FOO=  bar  \n")
                 '(("FOO" . "bar")))))

(ert-deftest clime-test-dotenv/parse-quoted-preserves-leading-ws ()
  "Quoted values preserve internal leading/trailing whitespace."
  (should (equal (clime-dotenv-parse "FOO=\"  spaced  \"\n")
                 '(("FOO" . "  spaced  ")))))

(ert-deftest clime-test-dotenv/parse-whitespace-around-equals ()
  "Whitespace around `=' is tolerated on both sides."
  (should (equal (clime-dotenv-parse "FOO = bar\n")
                 '(("FOO" . "bar")))))

(ert-deftest clime-test-dotenv/parse-equals-in-value ()
  "Only the first `=' splits key from value; further `=' are literal."
  (should (equal (clime-dotenv-parse "FOO=a=b=c\n")
                 '(("FOO" . "a=b=c")))))

(ert-deftest clime-test-dotenv/parse-duplicate-key-last-wins ()
  "Within a single file, the last occurrence of a key wins."
  (should (equal (clime-dotenv-parse "K=first\nK=second\n")
                 '(("K" . "second")))))

(ert-deftest clime-test-dotenv/parse-crlf-line-endings ()
  "CRLF line endings produce clean values (no trailing \\r)."
  (should (equal (clime-dotenv-parse "FOO=bar\r\nBAZ=qux\r\n")
                 '(("FOO" . "bar") ("BAZ" . "qux")))))

(ert-deftest clime-test-dotenv/parse-hash-without-leading-ws-not-comment ()
  "`#' inside a bare value (no preceding whitespace) is literal."
  (should (equal (clime-dotenv-parse "FOO=ab#cd\n")
                 '(("FOO" . "ab#cd")))))

(ert-deftest clime-test-dotenv/parse-no-trailing-newline ()
  "Last line without trailing newline still parses."
  (should (equal (clime-dotenv-parse "FOO=bar")
                 '(("FOO" . "bar")))))

(ert-deftest clime-test-dotenv/parse-double-quoted-preserves-spaces ()
  "Double-quoted values keep internal whitespace and #."
  (should (equal (clime-dotenv-parse "FOO=\"a b # c\"\n")
                 '(("FOO" . "a b # c")))))

(ert-deftest clime-test-dotenv/parse-double-quoted-escapes ()
  "Double quotes expand \\n, \\r, \\t escapes."
  (should (equal (clime-dotenv-parse "FOO=\"line1\\nline2\\ttab\"\n")
                 '(("FOO" . "line1\nline2\ttab")))))

(ert-deftest clime-test-dotenv/parse-single-quoted-literal ()
  "Single-quoted values are literal — no escape expansion."
  (should (equal (clime-dotenv-parse "FOO='line1\\nline2'\n")
                 '(("FOO" . "line1\\nline2")))))

(ert-deftest clime-test-dotenv/parse-export-prefix ()
  "Leading `export ' is tolerated and dropped."
  (should (equal (clime-dotenv-parse "export FOO=bar\n")
                 '(("FOO" . "bar")))))

(ert-deftest clime-test-dotenv/parse-empty-value ()
  "KEY= produces empty string value."
  (should (equal (clime-dotenv-parse "FOO=\n")
                 '(("FOO" . "")))))

(ert-deftest clime-test-dotenv/parse-malformed-key-signals ()
  "Key not matching [A-Za-z_][A-Za-z0-9_]* signals usage error."
  (should-error (clime-dotenv-parse "1FOO=bar\n")
                :type 'clime-usage-error))

(ert-deftest clime-test-dotenv/parse-unterminated-double-quote-signals ()
  "Unterminated double quote signals usage error."
  (should-error (clime-dotenv-parse "FOO=\"unterminated\n")
                :type 'clime-usage-error))

(ert-deftest clime-test-dotenv/parse-unterminated-single-quote-signals ()
  "Unterminated single quote signals usage error."
  (should-error (clime-dotenv-parse "FOO='unterminated\n")
                :type 'clime-usage-error))

(ert-deftest clime-test-dotenv/parse-no-equals-signals ()
  "Line without `=' signals usage error."
  (should-error (clime-dotenv-parse "FOO bar\n")
                :type 'clime-usage-error))

;;; ─── File loading ───────────────────────────────────────────────────────

(ert-deftest clime-test-dotenv/load-missing-file-returns-nil ()
  "Loading a non-existent file returns nil (silently)."
  (should-not (clime-dotenv-load "/nonexistent/path/.env.never")))

(ert-deftest clime-test-dotenv/load-existing-file ()
  "Loading an existing file returns the parsed alist."
  (clime-dotenv-test-with-file path "FOO=bar\nBAZ=qux\n"
    (should (equal (clime-dotenv-load path)
                   '(("FOO" . "bar") ("BAZ" . "qux"))))))

;;; ─── Real env wins over .env (precedence) ──────────────────────────────

(ert-deftest clime-test-dotenv/real-env-wins-over-dotenv ()
  "Real process env wins over .env values."
  (clime-dotenv-test-with-env (("FOO" "from-real-env"))
    (clime-dotenv-test-with-file path "FOO=from-dotenv\n"
      (let* ((opt (clime-make-option :name 'foo :flags '("--foo") :env "FOO"))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd)))))
        (should (equal (clime-run-capture-param app '("cmd") 'foo)
                       "from-real-env"))))))

;;; ─── CLI wins over .env ────────────────────────────────────────────────

(ert-deftest clime-test-dotenv/cli-wins-over-dotenv ()
  "CLI value wins over .env value."
  (clime-dotenv-test-with-env (("FOO" nil))
    (clime-dotenv-test-with-file path "FOO=from-dotenv\n"
      (let* ((opt (clime-make-option :name 'foo :flags '("--foo") :env "FOO"))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd)))))
        (should (equal (clime-run-capture-param
                        app '("cmd" "--foo" "from-cli") 'foo)
                       "from-cli"))))))

;;; ─── .env reaches options via :env-prefix ──────────────────────────────

(ert-deftest clime-test-dotenv/value-reaches-via-env-prefix ()
  ".env value is consumed by options using :env t with :env-prefix."
  (clime-dotenv-test-with-env (("MYAPP_PORT" nil))
    (clime-dotenv-test-with-file path "MYAPP_PORT=9090\n"
      (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                     :type 'integer :env t))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :env-prefix "MYAPP"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd)))))
        (should (equal (clime-run-capture-param app '("cmd") 'port) 9090))))))

(ert-deftest clime-test-dotenv/value-reaches-via-explicit-env ()
  ".env value is consumed by options using explicit :env \"NAME\"."
  (clime-dotenv-test-with-env (("CUSTOM_VAR" nil))
    (clime-dotenv-test-with-file path "CUSTOM_VAR=hello\n"
      (let* ((opt (clime-make-option :name 'greeting :flags '("--greeting")
                                     :env "CUSTOM_VAR"))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd)))))
        (should (equal (clime-run-capture-param app '("cmd") 'greeting)
                       "hello"))))))

;;; ─── Boolean coercion of .env value ────────────────────────────────────

(ert-deftest clime-test-dotenv/multiple-separator-from-dotenv ()
  ":multiple :separator option splits .env value into a list."
  (clime-dotenv-test-with-env (("MYAPP_TAGS" nil))
    (clime-dotenv-test-with-file path "MYAPP_TAGS=dev,ci,prod\n"
      (let* ((opt (clime-make-option :name 'tags :flags '("--tags")
                                     :multiple t :separator ","
                                     :env t))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :env-prefix "MYAPP"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd)))))
        (should (equal (clime-run-capture-param app '("cmd") 'tags)
                       '("dev" "ci" "prod")))))))

(ert-deftest clime-test-dotenv/boolean-coercion ()
  ".env value `true' coerces to t for boolean option."
  (clime-dotenv-test-with-env (("MYAPP_DRY_RUN" nil))
    (clime-dotenv-test-with-file path "MYAPP_DRY_RUN=true\n"
      (let* ((opt (clime-make-option :name 'dry-run :flags '("--dry-run")
                                     :nargs 0 :env t))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :env-prefix "MYAPP"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd)))))
        (should (eq (clime-run-capture-param app '("cmd") 'dry-run) t))))))

;;; ─── Multiple .env files: earlier wins per key ─────────────────────────

(ert-deftest clime-test-dotenv/multiple-files-earlier-wins ()
  "When :dotenv is a list, earlier files win per key; later files
contribute keys not present in earlier files."
  (clime-dotenv-test-with-env (("FOO" nil) ("BAR" nil))
    (clime-dotenv-test-with-file path-a "FOO=from-a\n"
      (clime-dotenv-test-with-file path-b "FOO=from-b\nBAR=only-in-b\n"
        (let* ((opt-foo (clime-make-option :name 'foo :flags '("--foo")
                                           :env "FOO"))
               (opt-bar (clime-make-option :name 'bar :flags '("--bar")
                                           :env "BAR"))
               (cmd (clime-make-command :name "cmd" :handler #'ignore
                                        :options (list opt-foo opt-bar)))
               (app (clime-make-app :name "t" :version "1"
                                    :dotenv (list path-a path-b)
                                    :children (list (cons "cmd" cmd)))))
          (should (equal (clime-run-capture-param app '("cmd") 'foo) "from-a"))
          (should (equal (clime-run-capture-param app '("cmd") 'bar)
                         "only-in-b")))))))

;;; ─── Missing file in list does not error ───────────────────────────────

(ert-deftest clime-test-dotenv/list-with-missing-file ()
  ":dotenv list silently skips missing files."
  (clime-dotenv-test-with-env (("FOO" nil))
    (clime-dotenv-test-with-file path "FOO=from-real\n"
      (let* ((opt (clime-make-option :name 'foo :flags '("--foo") :env "FOO"))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv (list "/nonexistent/.env.x" path)
                                  :children (list (cons "cmd" cmd)))))
        (should (equal (clime-run-capture-param app '("cmd") 'foo)
                       "from-real"))))))

;;; ─── :dotenv t → loads .env from default-directory ─────────────────────

(ert-deftest clime-test-dotenv/value-t-uses-cwd-default ()
  ":dotenv t loads `.env' from `default-directory'."
  (clime-dotenv-test-with-env (("FOO" nil))
    (let ((tmpdir (make-temp-file "clime-dotenv-cwd-" t)))
      (unwind-protect
          (let ((default-directory (file-name-as-directory tmpdir)))
            (with-temp-file (expand-file-name ".env" tmpdir)
              (insert "FOO=from-cwd-dotenv\n"))
            (let* ((opt (clime-make-option :name 'foo :flags '("--foo")
                                           :env "FOO"))
                   (cmd (clime-make-command :name "cmd" :handler #'ignore
                                            :options (list opt)))
                   (app (clime-make-app :name "t" :version "1"
                                        :dotenv t
                                        :children (list (cons "cmd" cmd)))))
              (should (equal (clime-run-capture-param app '("cmd") 'foo)
                             "from-cwd-dotenv"))))
        (delete-directory tmpdir t)))))

;;; ─── No leakage of process-environment ─────────────────────────────────

(ert-deftest clime-test-dotenv/no-leak-after-run ()
  "After `clime-run' returns, `process-environment' is unchanged."
  (clime-dotenv-test-with-env (("DOTENV_LEAK_KEY" nil))
    (clime-dotenv-test-with-file path "DOTENV_LEAK_KEY=leaked\n"
      (let* ((opt (clime-make-option :name 'foo :flags '("--foo")
                                     :env "DOTENV_LEAK_KEY"))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv path
                                  :children (list (cons "cmd" cmd))))
             (env-before (copy-sequence process-environment)))
        (clime-run app '("cmd"))
        (should-not (getenv "DOTENV_LEAK_KEY"))
        (should (equal process-environment env-before))))))

;;; ─── Malformed file surfaces line number ───────────────────────────────

(ert-deftest clime-test-dotenv/load-missing-equals-includes-location ()
  "Missing `=' surfaces with file + line via load."
  (clime-dotenv-test-with-file path "OK=value\nBAD LINE\n"
    (let ((err (should-error (clime-dotenv-load path)
                             :type 'clime-usage-error)))
      (let ((msg (cadr err)))
        (should (string-match-p (regexp-quote path) msg))
        (should (string-match-p "line 2" msg))))))

(ert-deftest clime-test-dotenv/load-bad-key-includes-location ()
  "Bad-shape KEY surfaces with file + line via load."
  (clime-dotenv-test-with-file path "OK=value\n9BAD=oops\n"
    (let ((err (should-error (clime-dotenv-load path)
                             :type 'clime-usage-error)))
      (let ((msg (cadr err)))
        (should (string-match-p (regexp-quote path) msg))
        (should (string-match-p "line 2" msg))))))

(ert-deftest clime-test-dotenv/load-unterminated-quote-includes-location ()
  "Unterminated quote surfaces with file + line via load."
  (clime-dotenv-test-with-file path "OK=value\nFOO=\"never closes\n"
    (let ((err (should-error (clime-dotenv-load path)
                             :type 'clime-usage-error)))
      (let ((msg (cadr err)))
        (should (string-match-p (regexp-quote path) msg))
        (should (string-match-p "line 2" msg))))))

;;; ─── DSL preserves :dotenv ─────────────────────────────────────────────

(ert-deftest clime-test-dotenv/dsl-preserves-dotenv-slot ()
  "(clime-app NAME :dotenv ...) round-trips through `clime-app-dotenv'."
  (eval '(clime-app clime-test--dotenv-dsl-app
           :version "1.0"
           :dotenv "/tmp/clime-dotenv-dsl-test.env"
           (clime-command noop
             (clime-handler (ctx) "ok")))
        t)
  (should (equal (clime-app-dotenv clime-test--dotenv-dsl-app)
                 "/tmp/clime-dotenv-dsl-test.env")))

(ert-deftest clime-test-dotenv/dsl-accepts-list-of-paths ()
  ":dotenv accepts a list of paths through the DSL."
  (eval '(clime-app clime-test--dotenv-dsl-list-app
           :version "1.0"
           :dotenv '("/tmp/a.env" "/tmp/b.env")
           (clime-command noop
             (clime-handler (ctx) "ok")))
        t)
  (should (equal (clime-app-dotenv clime-test--dotenv-dsl-list-app)
                 '("/tmp/a.env" "/tmp/b.env"))))

;;; ─── Surface coverage: serve and invoke honor :dotenv ──────────────────

(ert-deftest clime-test-dotenv/serve-dispatch-honors-dotenv ()
  "Serve dispatch reads .env-supplied values via :env."
  (require 'clime-serve)
  (clime-dotenv-test-with-env (("SERVE_DOTENV_VAR" nil))
    (clime-dotenv-test-with-file path "SERVE_DOTENV_VAR=from-dotenv\n"
      (let* ((captured nil)
             (opt (clime-make-option :name 'val :flags '("--val")
                                     :env "SERVE_DOTENV_VAR"))
             (cmd (clime-make-command
                   :name "echo" :options (list opt)
                   :handler (lambda (ctx)
                              (setq captured
                                    (clime-ctx-get ctx 'val)))))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv path
                                  :children (list (cons "echo" cmd)))))
        (clime-serve--dispatch app '("echo") nil)
        (should (equal captured "from-dotenv"))))))

(ert-deftest clime-test-dotenv/invoke-seeding-honors-dotenv ()
  "Invoke seeding picks up .env-supplied env values."
  (require 'clime-invoke)
  (clime-dotenv-test-with-env (("INVOKE_DOTENV_VAR" nil))
    (clime-dotenv-test-with-file path "INVOKE_DOTENV_VAR=seeded\n"
      (let* ((opt (clime-make-option :name 'val :flags '("--val")
                                     :env "INVOKE_DOTENV_VAR"))
             (cmd (clime-make-command
                   :name "echo" :options (list opt) :handler #'ignore))
             (app (clime-make-app :name "t" :version "1"
                                  :dotenv path
                                  :children (list (cons "echo" cmd))))
             (clime-invoke--values nil))
        (clime-dotenv-with-app-env app
          (clime-invoke--seed-values cmd nil app))
        (should (equal (clime-values-value clime-invoke--values 'val)
                       "seeded"))))))

(provide 'clime-dotenv-tests)
;;; clime-dotenv-tests.el ends here
