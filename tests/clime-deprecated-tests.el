;;; clime-deprecated-tests.el --- Tests for deprecation warnings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for :deprecated slot on options, args, commands, and groups.
;; Covers help rendering, runtime warnings, and DSL integration.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-help)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Struct Slots ───────────────────────────────────────────────────

(ert-deftest clime-test-deprecated/option-slot-nil-by-default ()
  "Options have :deprecated nil by default."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo"))))
    (should-not (clime-option-deprecated opt))))

(ert-deftest clime-test-deprecated/option-slot-string ()
  ":deprecated accepts a string."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo")
                                :deprecated "Use --bar instead")))
    (should (equal (clime-option-deprecated opt) "Use --bar instead"))))

(ert-deftest clime-test-deprecated/option-slot-boolean ()
  ":deprecated accepts t."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :deprecated t)))
    (should (eq (clime-option-deprecated opt) t))))

(ert-deftest clime-test-deprecated/command-slot ()
  "Commands inherit :deprecated from clime-node."
  (let ((cmd (clime-make-command :name "old" :handler #'ignore :deprecated "Use new")))
    (should (equal (clime-node-deprecated cmd) "Use new"))))

(ert-deftest clime-test-deprecated/arg-slot ()
  "Args have :deprecated slot."
  (let ((arg (clime-make-arg :name 'target :deprecated t)))
    (should (eq (clime-arg-deprecated arg) t))))

;;; ─── Help Rendering ─────────────────────────────────────────────────

(ert-deftest clime-test-deprecated/help-option-string ()
  "Deprecated option with message shows (deprecated: MSG) in help."
  (let* ((opt (clime-make-option :name 'old :flags '("--old")
                                  :help "Legacy option"
                                  :deprecated "Use --new instead"))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "test"))))
    (should (string-match-p "(deprecated: Use --new instead)" help))))

(ert-deftest clime-test-deprecated/help-option-boolean ()
  "Deprecated option with t shows (deprecated) in help."
  (let* ((opt (clime-make-option :name 'old :flags '("--old")
                                  :help "Legacy option"
                                  :deprecated t))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "test"))))
    (should (string-match-p "(deprecated)" help))))

(ert-deftest clime-test-deprecated/help-command-string ()
  "Deprecated command with message shows (deprecated: MSG) in help."
  (let* ((cmd (clime-make-command :name "migrate"
                                   :handler #'ignore
                                   :help "Old migration"
                                   :deprecated "Use upgrade instead"))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "migrate" cmd))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "(deprecated: Use upgrade instead)" help))))

(ert-deftest clime-test-deprecated/help-command-boolean ()
  "Deprecated command with t shows (deprecated) in help."
  (let* ((cmd (clime-make-command :name "migrate"
                                   :handler #'ignore
                                   :help "Old migration"
                                   :deprecated t))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "migrate" cmd))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "(deprecated)" help))))

(ert-deftest clime-test-deprecated/help-no-deprecated ()
  "Non-deprecated option has no (deprecated) in help."
  (let* ((opt (clime-make-option :name 'foo :flags '("--foo") :help "Normal"))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "test"))))
    (should-not (string-match-p "deprecated" help))))

;;; ─── Runtime Warnings (Options) ─────────────────────────────────────

(ert-deftest clime-test-deprecated/option-warning-string ()
  "Using a deprecated option emits a warning with the message."
  (let* ((opt (clime-make-option :name 'old :flags '("--old") :nargs 0
                                  :deprecated "Use --new"))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (msgs (clime-test-with-messages
                 (with-output-to-string
                   (clime-run app '("run" "--old"))))))
    (should (cl-some (lambda (m) (string-match-p "Warning: --old is deprecated. Use --new" m))
                     msgs))))

(ert-deftest clime-test-deprecated/option-warning-boolean ()
  "Using a deprecated option with t emits a generic warning."
  (let* ((opt (clime-make-option :name 'old :flags '("--old") :nargs 0
                                  :deprecated t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (msgs (clime-test-with-messages
                 (with-output-to-string
                   (clime-run app '("run" "--old"))))))
    (should (cl-some (lambda (m) (string-match-p "Warning: --old is deprecated$" m))
                     msgs))))

(ert-deftest clime-test-deprecated/option-still-works ()
  "Deprecated option is still consumed and its value available."
  (let* ((opt (clime-make-option :name 'old :flags '("--old")
                                  :deprecated t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (plist-get (clime-context-params ctx) 'old))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-test-with-messages
                     (clime-run app '("run" "--old" "val"))))))
    (should (equal output "val"))))

;;; ─── Runtime Warnings (Commands) ────────────────────────────────────

(ert-deftest clime-test-deprecated/command-warning-string ()
  "Dispatching a deprecated command emits a warning with the message."
  (let* ((cmd (clime-make-command :name "migrate"
                                   :handler (lambda (_ctx) nil)
                                   :deprecated "Use upgrade"))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "migrate" cmd))))
         (msgs (clime-test-with-messages
                 (with-output-to-string
                   (clime-run app '("migrate"))))))
    (should (cl-some (lambda (m) (string-search "Warning: migrate is deprecated" m))
                     msgs))))

(ert-deftest clime-test-deprecated/command-warning-boolean ()
  "Dispatching a deprecated command with t emits a generic warning."
  (let* ((cmd (clime-make-command :name "migrate"
                                   :handler (lambda (_ctx) nil)
                                   :deprecated t))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "migrate" cmd))))
         (msgs (clime-test-with-messages
                 (with-output-to-string
                   (clime-run app '("migrate"))))))
    (should (cl-some (lambda (m) (string-search "Warning: migrate is deprecated" m))
                     msgs))))

(ert-deftest clime-test-deprecated/command-still-runs ()
  "Deprecated command handler still executes."
  (let* ((cmd (clime-make-command :name "migrate"
                                   :handler (lambda (_ctx) "migrated")
                                   :deprecated t))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "migrate" cmd))))
         (output (with-output-to-string
                   (clime-test-with-messages
                     (clime-run app '("migrate"))))))
    (should (equal output "migrated"))))

;;; ─── Hidden + Deprecated ────────────────────────────────────────────

(ert-deftest clime-test-deprecated/hidden-deprecated-option-not-in-help ()
  "Hidden deprecated option does not appear in help."
  (let* ((opt (clime-make-option :name 'old :flags '("--old") :nargs 0
                                  :hidden t :deprecated "Use --new"))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "test"))))
    (should-not (string-match-p "--old" help))))

(ert-deftest clime-test-deprecated/hidden-deprecated-option-still-warns ()
  "Hidden deprecated option still emits a warning when used."
  (let* ((opt (clime-make-option :name 'old :flags '("--old") :nargs 0
                                  :hidden t :deprecated "Use --new"))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (msgs (clime-test-with-messages
                 (with-output-to-string
                   (clime-run app '("run" "--old"))))))
    (should (cl-some (lambda (m) (string-match-p "Warning: --old is deprecated" m))
                     msgs))))

;;; ─── DSL Integration ────────────────────────────────────────────────

(ert-deftest clime-test-deprecated/dsl-option ()
  ":deprecated on option in DSL sets the slot."
  (eval
   '(clime-app clime-test--depr-opt-app
      :version "1"
      (clime-option old ("--old") :deprecated "Use --new" :bool t)
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--depr-opt-app))
         (opt (clime-node-find-option app "--old")))
    (should (equal (clime-option-deprecated opt) "Use --new"))))

(ert-deftest clime-test-deprecated/dsl-command ()
  ":deprecated on command in DSL sets the slot."
  (eval
   '(clime-app clime-test--depr-cmd-app
      :version "1"
      (clime-command migrate
        :deprecated "Use upgrade"
        :help "Old command"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--depr-cmd-app))
         (cmd (cdr (assoc "migrate" (clime-group-children app)))))
    (should (equal (clime-node-deprecated cmd) "Use upgrade"))))

(ert-deftest clime-test-deprecated/dsl-group ()
  ":deprecated on group in DSL sets the slot."
  (eval
   '(clime-app clime-test--depr-grp-app
      :version "1"
      (clime-group legacy
        :deprecated t
        :help "Old commands"
        (clime-command old-cmd
          :help "An old command"
          (clime-handler (_ctx) nil))))
   t)
  (let* ((app (symbol-value 'clime-test--depr-grp-app))
         (grp (cdr (assoc "legacy" (clime-group-children app)))))
    (should (eq (clime-node-deprecated grp) t))))

(provide 'clime-deprecated-tests)
;;; clime-deprecated-tests.el ends here
