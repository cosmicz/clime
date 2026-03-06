;;; clime-help-tests.el --- Tests for clime-help  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the help formatter.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-help)
(require 'clime-test-helpers)

;;; ─── Test Fixtures ────────────────────────────────────────────────────

(defvar clime-test--help-cmd
  (clime-make-command :name "show"
                      :help "Show a resource"
                      :handler #'ignore
                      :options (list (clime-make-option :name 'json
                                                        :flags '("--json")
                                                        :nargs 0
                                                        :help "Output as JSON"))
                      :args (list (clime-make-arg :name 'id
                                                  :help "Resource ID")))
  "Test command with options and args.")

(defvar clime-test--help-app
  (let* ((cmd-show clime-test--help-cmd)
         (cmd-list (clime-make-command :name "list"
                                       :help "List resources"
                                       :handler #'ignore))
         (cmd-hidden (clime-make-command :name "debug"
                                         :help "Debug internals"
                                         :handler #'ignore
                                         :hidden t))
         (app (clime-make-app :name "myapp"
                              :version "1.2.3"
                              :help "A test application"
                              :options (list (clime-make-option
                                             :name 'verbose
                                             :flags '("--verbose" "-v")
                                             :count t
                                             :help "Increase verbosity"))
                              :children (list (cons "show" cmd-show)
                                              (cons "list" cmd-list)
                                              (cons "debug" cmd-hidden)))))
    app)
  "Test app for help tests.")

;;; ─── Usage Line ───────────────────────────────────────────────────────

(ert-deftest clime-test-help/usage-command ()
  "Command help shows usage with args."
  (let ((help (clime-format-help clime-test--help-cmd '("myapp" "show"))))
    (should (string-match-p "Usage: myapp show" help))
    (should (string-match-p "<id>" help))))

(ert-deftest clime-test-help/usage-app ()
  "App help shows usage with COMMAND."
  (let ((help (clime-format-help clime-test--help-app '("myapp"))))
    (should (string-match-p "Usage: myapp" help))
    (should (string-match-p "COMMAND" help))))

;;; ─── Description ──────────────────────────────────────────────────────

(ert-deftest clime-test-help/description ()
  "Help includes the description text."
  (let ((help (clime-format-help clime-test--help-app '("myapp"))))
    (should (string-match-p "A test application" help))))

(ert-deftest clime-test-help/command-description ()
  "Command help includes command description."
  (let ((help (clime-format-help clime-test--help-cmd '("myapp" "show"))))
    (should (string-match-p "Show a resource" help))))

;;; ─── Arguments Section ────────────────────────────────────────────────

(ert-deftest clime-test-help/arguments ()
  "Command help lists positional arguments."
  (let ((help (clime-format-help clime-test--help-cmd '("myapp" "show"))))
    (should (string-match-p "Arguments:" help))
    (should (string-match-p "<id>" help))
    (should (string-match-p "Resource ID" help))))

;;; ─── Options Section ──────────────────────────────────────────────────

(ert-deftest clime-test-help/options ()
  "Help lists options with flags and help text."
  (let ((help (clime-format-help clime-test--help-app '("myapp"))))
    (should (string-match-p "Options:" help))
    (should (string-match-p "-v, --verbose" help))
    (should (string-match-p "Increase verbosity" help))))

(ert-deftest clime-test-help/command-options ()
  "Command help lists its own options."
  (let ((help (clime-format-help clime-test--help-cmd '("myapp" "show"))))
    (should (string-match-p "--json" help))
    (should (string-match-p "Output as JSON" help))))

(ert-deftest clime-test-help/hidden-option-omitted ()
  "Hidden options are not shown in help."
  (let* ((opt-visible (clime-make-option :name 'out :flags '("--out")
                                          :help "Output file"))
         (opt-hidden (clime-make-option :name 'secret :flags '("--secret")
                                        :help "Secret flag" :hidden t))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt-visible opt-hidden)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "--out" help))
    (should-not (string-match-p "--secret" help))))

;;; ─── Option Flag Formatting ────────────────────────────────────────────

(ert-deftest clime-test-help/short-flag-first ()
  "Short flags appear before long flags."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                  :count t :help "Verbosity"))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "-v, --verbose" help))))

(ert-deftest clime-test-help/value-placeholder ()
  "Value-taking options show VALUE after flags."
  (let* ((opt (clime-make-option :name 'output :flags '("-o" "--output")
                                  :help "Output file"))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "--output VALUE" help))))

(ert-deftest clime-test-help/no-value-for-boolean ()
  "Boolean options do not show VALUE."
  (let* ((opt (clime-make-option :name 'json :flags '("--json")
                                  :nargs 0 :help "JSON output"))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "--json " help))
    (should-not (string-match-p "--json VALUE" help))))

(ert-deftest clime-test-help/repeat-indicator-count ()
  "Count options show ... indicator."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v")
                                  :count t :help "Verbosity"))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "-v \\.\\.\\." help))))

(ert-deftest clime-test-help/repeat-indicator-multiple ()
  "Multiple options show ... indicator."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag")
                                  :multiple t :help "Add tag"))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "--tag VALUE \\.\\.\\." help))))

;;; ─── Option Groups ────────────────────────────────────────────────────

(ert-deftest clime-test-help/option-groups ()
  "Options with :group label are clustered under group header."
  (let* ((opt-a (clime-make-option :name 'verbose :flags '("--verbose")
                                    :count t :help "Verbosity"))
         (opt-b (clime-make-option :name 'to :flags '("--to")
                                    :help "Target" :group "Workflow"))
         (opt-c (clime-make-option :name 'from :flags '("--from")
                                    :help "Source" :group "Workflow"))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt-a opt-b opt-c)))
         (help (clime-format-help cmd '("app" "x"))))
    (should (string-match-p "Options:" help))
    (should (string-match-p "Workflow:" help))
    (should (string-match-p "--to" help))
    (should (string-match-p "--from" help))))

;;; ─── Commands Section ─────────────────────────────────────────────────

(ert-deftest clime-test-help/commands ()
  "App help lists child commands."
  (let ((help (clime-format-help clime-test--help-app '("myapp"))))
    (should (string-match-p "Commands:" help))
    (should (string-match-p "show" help))
    (should (string-match-p "Show a resource" help))
    (should (string-match-p "list" help))))

(ert-deftest clime-test-help/hidden-command-omitted ()
  "Hidden commands are not shown in help."
  (let ((help (clime-format-help clime-test--help-app '("myapp"))))
    (should-not (string-match-p "debug" help))))

;;; ─── Version ──────────────────────────────────────────────────────────

(ert-deftest clime-test-help/version ()
  "Version string includes app name and version."
  (let ((ver (clime-format-version clime-test--help-app)))
    (should (string-match-p "myapp" ver))
    (should (string-match-p "1\\.2\\.3" ver))))

;;; ─── Integration with clime-run ───────────────────────────────────────

(ert-deftest clime-test-help/run-integration ()
  "--help through clime-run produces real help output."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                  :help "Show things"))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (output ""))
    (cl-letf (((symbol-function 'princ)
               (lambda (obj &optional _stream)
                 (setq output (concat output
                                      (if (stringp obj) obj
                                        (format "%s" obj)))))))
      (clime-run app '("show" "--help")))
    (should (string-match-p "Usage: t show" output))
    (should (string-match-p "Show things" output))))

(provide 'clime-help-tests)
;;; clime-help-tests.el ends here
