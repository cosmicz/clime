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

;;; ─── Group With Invoke + Children ────────────────────────────────────

(ert-deftest clime-test-help/group-with-invoke-shows-commands ()
  "Group with both :handler and children shows Commands section in help."
  (let* ((cmd (clime-make-command :name "detail" :handler #'ignore
                                  :help "Show details"))
         (grp (clime-make-group :name "status"
                                :help "Status overview"
                                :handler (lambda (_ctx) nil)
                                :children (list (cons "detail" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "status" grp))))
         (help (clime-format-help grp '("t" "status"))))
    (should (string-match-p "Commands:" help))
    (should (string-match-p "detail" help))
    (should (string-match-p "Show details" help))))

;;; ─── Multiple Arguments ─────────────────────────────────────────────

(ert-deftest clime-test-help/multiple-args ()
  "Command with multiple args shows all in Arguments section."
  (let* ((cmd (clime-make-command :name "copy" :handler #'ignore
                                  :help "Copy files"
                                  :args (list (clime-make-arg :name 'src
                                                              :help "Source file")
                                              (clime-make-arg :name 'dst
                                                              :help "Destination"))))
         (help (clime-format-help cmd '("app" "copy"))))
    (should (string-match-p "Arguments:" help))
    (should (string-match-p "<src>" help))
    (should (string-match-p "Source file" help))
    (should (string-match-p "<dst>" help))
    (should (string-match-p "Destination" help))))

;;; ─── Multiline Help ──────────────────────────────────────────────────

(ert-deftest clime-test-help/multiline-help-first-line-in-listing ()
  "Commands listing shows only the first line of multiline help."
  (let* ((cmd (clime-make-command :name "deploy"
                                  :help "Deploy the application\nUses rsync under the hood."
                                  :handler #'ignore))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "deploy" cmd))))
         (help (clime-format-help app '("t"))))
    ;; First line appears in the Commands listing
    (should (string-match-p "Deploy the application" help))
    ;; Second line does NOT appear in the listing
    (should-not (string-match-p "rsync" help))))

(ert-deftest clime-test-help/multiline-help-full-in-detail ()
  "Detailed command help shows the full multiline help text."
  (let* ((cmd (clime-make-command :name "deploy"
                                  :help "Deploy the application\nUses rsync under the hood."
                                  :handler #'ignore))
         (help (clime-format-help cmd '("t" "deploy"))))
    (should (string-match-p "Deploy the application" help))
    (should (string-match-p "rsync" help))))

;;; ─── Epilog ──────────────────────────────────────────────────────────

(ert-deftest clime-test-help/epilog-appears-after-sections ()
  "Epilog text appears at the end of help output."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                  :help "Show things"
                                  :epilog "Examples:\n  app show 123"))
         (help (clime-format-help cmd '("app" "show"))))
    (should (string-match-p "Examples:" help))
    (should (string-match-p "app show 123" help))
    ;; Epilog should be after the description
    (should (< (string-match "Show things" help)
               (string-match "Examples:" help)))))

(ert-deftest clime-test-help/no-epilog-unchanged ()
  "Help output without epilog is unchanged."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                  :help "Show things"))
         (help (clime-format-help cmd '("app" "show"))))
    (should-not (string-match-p "Examples:" help))))

(ert-deftest clime-test-help/app-epilog ()
  "App-level epilog appears in app help."
  (let* ((app (clime-make-app :name "t" :version "1"
                              :help "Test app"
                              :epilog "See https://example.com for docs."))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "https://example.com" help))))

;;; ─── Choices in Help ─────────────────────────────────────────────────

(ert-deftest clime-test-help/choices-shown-in-help ()
  "Option with :choices shows allowed values in help text."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices '("json" "table" "csv")
                                  :help "Output format"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "show"))))
    (should (string-match-p "choices: json, table, csv" help))))

(ert-deftest clime-test-help/choices-arg-shown-in-help ()
  "Positional arg with :choices shows allowed values in help text."
  (let* ((arg (clime-make-arg :name 'action
                               :choices '("start" "stop")
                               :help "Service action"))
         (cmd (clime-make-command :name "svc" :handler #'ignore
                                  :args (list arg)))
         (help (clime-format-help cmd '("app" "svc"))))
    (should (string-match-p "choices: start, stop" help))))

(ert-deftest clime-test-help/default-group-inlined ()
  "Default group's children appear at parent level in help."
  (let* ((add-cmd (clime-make-command :name "add" :handler #'ignore
                                       :help "Add a thing"))
         (rm-cmd (clime-make-command :name "remove" :handler #'ignore
                                      :help "Remove a thing"))
         (grp (clime-make-group :name "repo" :inline t
                                :help "Manage repos"
                                :children (list (cons "add" add-cmd)
                                                (cons "remove" rm-cmd))))
         (info-cmd (clime-make-command :name "info" :handler #'ignore
                                        :help "Show info"))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "repo" grp)
                                              (cons "info" info-cmd))))
         (help (clime-format-help app '("myapp"))))
    ;; Default group's children should appear at top level
    (should (string-match-p "add" help))
    (should (string-match-p "remove" help))
    (should (string-match-p "info" help))
    ;; The group name itself should not appear as a command
    (should-not (string-match-p "repo" help))))

(ert-deftest clime-test-help/inline-group-hidden-child ()
  "Hidden children in an inline group stay hidden in parent help."
  (let* ((add-cmd (clime-make-command :name "add" :handler #'ignore
                                       :help "Add a thing"))
         (secret (clime-make-command :name "secret" :handler #'ignore
                                      :help "Secret cmd" :hidden t))
         (grp (clime-make-group :name "repo" :inline t
                                :children (list (cons "add" add-cmd)
                                                (cons "secret" secret))))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "repo" grp))))
         (help (clime-format-help app '("myapp"))))
    (should (string-match-p "add" help))
    (should-not (string-match-p "secret" help))))

;;; ─── Ancestor Options in Help ────────────────────────────────────────

(ert-deftest clime-test-help/ancestor-options-shown ()
  "Help for a child command shows inherited ancestor options."
  (let* ((grp-opt (clime-make-option :name 'env :flags '("--env")
                                      :help "Target environment"))
         (cmd (clime-make-command :name "start" :handler #'ignore
                                  :help "Start the service"))
         (grp (clime-make-group :name "deploy"
                                :options (list grp-opt)
                                :children (list (cons "start" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "deploy" grp)))))
    ;; Need parent refs set for ancestor walk
    (clime--set-parent-refs app)
    (let ((help (clime-format-help cmd '("t" "deploy" "start"))))
      (should (string-match-p "--env" help))
      (should (string-match-p "Target environment" help)))))

(ert-deftest clime-test-help/ancestor-options-separate-section ()
  "Ancestor options appear under a distinct heading, not mixed with local."
  (let* ((grp-opt (clime-make-option :name 'env :flags '("--env")
                                      :help "Target environment"))
         (cmd-opt (clime-make-option :name 'port :flags '("--port")
                                      :help "Port number"))
         (cmd (clime-make-command :name "start" :handler #'ignore
                                  :options (list cmd-opt)))
         (grp (clime-make-group :name "deploy"
                                :options (list grp-opt)
                                :children (list (cons "start" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "deploy" grp)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help cmd '("t" "deploy" "start"))))
      ;; Local options in Options section
      (should (string-match-p "--port" help))
      ;; Ancestor options in a separate section
      (should (string-match-p "Global Options:" help))
      (should (string-match-p "--env" help)))))

(ert-deftest clime-test-help/no-ancestor-section-when-empty ()
  "No Global Options section when the command has no ancestors with options."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list (clime-make-option
                                                  :name 'json :flags '("--json")
                                                  :nargs 0 :help "JSON output"))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help cmd '("t" "show"))))
      (should-not (string-match-p "Global Options:" help)))))

(ert-deftest clime-test-help/choices-function-shown-in-help ()
  "Option with :choices as a function resolves and shows values in help."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices (lambda () '("json" "table"))
                                  :help "Output format"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "show"))))
    (should (string-match-p "choices: json, table" help))))

(provide 'clime-help-tests)
;;; clime-help-tests.el ends here
