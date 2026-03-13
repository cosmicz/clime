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
  "Options with :category label are clustered under group header."
  (let* ((opt-a (clime-make-option :name 'verbose :flags '("--verbose")
                                    :count t :help "Verbosity"))
         (opt-b (clime-make-option :name 'to :flags '("--to")
                                    :help "Target" :category "Workflow"))
         (opt-c (clime-make-option :name 'from :flags '("--from")
                                    :help "Source" :category "Workflow"))
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

;;; ─── Category Specs ──────────────────────────────────────────────────
;;
;; Spec numbering matches acceptance criteria on clime-162.

;; 6. No categories → "Options:" / "Commands:" (backward compat)
(ert-deftest clime-test-help/cat-06-no-categories-backward-compat ()
  "Without :category, help renders Options: and Commands: as before."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")
                                  :count t :help "Verbosity"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :help "Show details"))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "show" cmd))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Options:" help))
    (should (string-match-p "^Commands:" help))))

;; 7. Commands with :category → grouped headings
(ert-deftest clime-test-help/cat-07-command-categories ()
  "Commands with :category are grouped under category headings."
  (let* ((cmd-a (clime-make-command :name "search" :handler #'ignore
                                     :help "Search beads" :category "Filter"))
         (cmd-b (clime-make-command :name "list" :handler #'ignore
                                     :help "List beads" :category "Filter"))
         (cmd-c (clime-make-command :name "claim" :handler #'ignore
                                     :help "Claim a bead" :category "Lifecycle"))
         (cmd-d (clime-make-command :name "show" :handler #'ignore
                                     :help "Show details"))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "search" cmd-a)
                                              (cons "list" cmd-b)
                                              (cons "claim" cmd-c)
                                              (cons "show" cmd-d))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Filter:" help))
    (should (string-match-p "^Lifecycle:" help))
    (should (string-match-p "^Commands:" help))
    ;; Uncategorized "show" under Commands:, not under a category
    (should (string-match-p "Commands:.*show" (replace-regexp-in-string "\n" " " help)))))

;; 8. Options with :category → grouped headings
(ert-deftest clime-test-help/cat-08-option-categories ()
  "Options with :category are grouped under category headings."
  (let* ((opt-a (clime-make-option :name 'json :flags '("--json")
                                    :nargs 0 :help "JSON output" :category "Output"))
         (opt-b (clime-make-option :name 'verbose :flags '("-v")
                                    :count t :help "Verbosity"))
         (cmd (clime-make-command :name "show" :handler #'ignore :help "Show"))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt-b opt-a)
                              :children (list (cons "show" cmd))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Options:" help))
    (should (string-match-p "^Output:" help))
    ;; --json under Output, -v under Options
    (let ((output-pos (string-match "^Output:" help))
          (opts-pos (string-match "^Options:" help)))
      (should (string-match-p "--json" (substring help output-pos)))
      (should (string-match-p "-v" (substring help opts-pos))))))

;; 9. Mixed options+commands same category → unified section
(ert-deftest clime-test-help/cat-09-mixed-options-and-commands ()
  "Options and commands with same :category share a section."
  (let* ((opt (clime-make-option :name 'status :flags '("--status")
                                  :help "Filter by status" :category "Filter"))
         (cmd (clime-make-command :name "search" :handler #'ignore
                                   :help "Search beads" :category "Filter"))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "search" cmd))))
         (help (clime-format-help app '("t"))))
    ;; Single Filter section with both
    (should (string-match-p "^Filter:" help))
    (should (string-match-p "--status" help))
    (should (string-match-p "search" help))
    ;; No separate Options: or Commands:
    (should-not (string-match-p "^Options:" help))
    (should-not (string-match-p "^Commands:" help))))

;; 10. Inline group with :category → children inherit
(ert-deftest clime-test-help/cat-10-inline-group-children-inherit ()
  "Inline group with :category renders its children under that category."
  (let* ((cmd-a (clime-make-command :name "search" :handler #'ignore
                                     :help "Search beads"))
         (cmd-b (clime-make-command :name "list" :handler #'ignore
                                     :help "List beads"))
         (grp (clime-make-group :name "filter" :inline t :category "Filter"
                                :children (list (cons "search" cmd-a)
                                                (cons "list" cmd-b))))
         (cmd-c (clime-make-command :name "show" :handler #'ignore
                                     :help "Show details"))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "filter" grp)
                                              (cons "show" cmd-c))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Filter:" help))
    (should (string-match-p "search" help))
    (should (string-match-p "list" help))
    (should (string-match-p "^Commands:" help))
    (should (string-match-p "show" help))))

;; 11. Inline group option inherits group category
(ert-deftest clime-test-help/cat-11-inline-group-option-inherits ()
  "Option on inline group with :category renders under that category."
  (let* ((opt (clime-make-option :name 'token :flags '("--token")
                                  :help "Admin token"))
         (cmd (clime-make-command :name "status" :handler #'ignore
                                   :help "Show status"))
         (grp (clime-make-group :name "admin" :inline t :category "Admin"
                                :options (list opt)
                                :children (list (cons "status" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin" grp))))
         (help (clime-format-help app '("t"))))
    ;; Both option and command under Admin:
    (should (string-match-p "^Admin:" help))
    (should (string-match-p "--token" help))
    (should (string-match-p "status" help))))

;; 12. Inline group option with own :category → composes with group
(ert-deftest clime-test-help/cat-12-inline-option-composes ()
  "Option with own :category on categorized inline group composes path."
  (let* ((opt (clime-make-option :name 'token :flags '("--token")
                                  :help "Auth token" :category "Auth"))
         (cmd (clime-make-command :name "status" :handler #'ignore
                                   :help "Show status"))
         (grp (clime-make-group :name "admin" :inline t :category "Admin"
                                :options (list opt)
                                :children (list (cons "status" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin" grp))))
         (help (clime-format-help app '("t"))))
    ;; Top-level: Admin heading
    (should (string-match-p "^Admin:" help))
    (should (string-match-p "status" help))
    ;; Option sub-category indented with scope annotation
    (should (string-match-p "^  Auth (status):" help))
    (should (string-match-p "--token" help))))

;; 13. Nested inline groups → paths compose
(ert-deftest clime-test-help/cat-13-nested-inline-compose ()
  "Nested inline groups compose category paths."
  (let* ((get-cmd (clime-make-command :name "get" :handler #'ignore
                                       :help "Get config value"))
         (set-cmd (clime-make-command :name "set" :handler #'ignore
                                       :help "Set config value"))
         (config-grp (clime-make-group :name "config" :inline t
                                        :category "Config"
                                        :children (list (cons "get" get-cmd)
                                                        (cons "set" set-cmd))))
         (status-cmd (clime-make-command :name "status" :handler #'ignore
                                          :help "Show status"))
         (admin-grp (clime-make-group :name "admin" :inline t
                                       :category "Admin"
                                       :children (list (cons "config" config-grp)
                                                       (cons "status" status-cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin" admin-grp))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^  Config:" help))
    (should (string-match-p "^Admin:" help))
    (should (string-match-p "get" help))
    (should (string-match-p "set" help))
    (should (string-match-p "status" help))))

;; 14. Uncategorized inline group → transparent for children
(ert-deftest clime-test-help/cat-14-uncategorized-inline-transparent ()
  "Uncategorized inline group children fall through to Commands:."
  (let* ((cmd-a (clime-make-command :name "search" :handler #'ignore
                                     :help "Search"))
         (grp (clime-make-group :name "stuff" :inline t
                                :children (list (cons "search" cmd-a))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "stuff" grp))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Commands:" help))
    (should (string-match-p "search" help))))

;; 15. Uncategorized inline group, categorized children → child's category
(ert-deftest clime-test-help/cat-15-uncategorized-group-categorized-children ()
  "Categorized children of uncategorized inline group keep their own category."
  (let* ((cmd-a (clime-make-command :name "search" :handler #'ignore
                                     :help "Search" :category "Filter"))
         (cmd-b (clime-make-command :name "deploy" :handler #'ignore
                                     :help "Deploy" :category "Deploy"))
         (grp (clime-make-group :name "stuff" :inline t
                                :children (list (cons "search" cmd-a)
                                                (cons "deploy" cmd-b))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "stuff" grp))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Filter:" help))
    (should (string-match-p "^Deploy:" help))
    ;; No Commands: — both have categories
    (should-not (string-match-p "^Commands:" help))))

;; 16. Uncategorized inline group, option with no category → skipped
(ert-deftest clime-test-help/cat-16-uncategorized-inline-option-skipped ()
  "Option on uncategorized inline group with no own category is skipped."
  (let* ((opt (clime-make-option :name 'token :flags '("--token")
                                  :help "A token"))
         (cmd (clime-make-command :name "run" :handler #'ignore :help "Run"))
         (grp (clime-make-group :name "stuff" :inline t
                                :options (list opt)
                                :children (list (cons "run" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "stuff" grp))))
         (help (clime-format-help app '("t"))))
    ;; Option is skipped — no category home
    (should-not (string-match-p "--token" help))
    ;; Command still appears under Commands:
    (should (string-match-p "^Commands:" help))
    (should (string-match-p "run" help))))

;; 17. Uncategorized inline group, option with own :category → renders
(ert-deftest clime-test-help/cat-17-uncategorized-group-categorized-option ()
  "Option with :category on uncategorized inline group renders under that category."
  (let* ((opt (clime-make-option :name 'token :flags '("--token")
                                  :help "Auth token" :category "Auth"))
         (cmd (clime-make-command :name "run" :handler #'ignore :help "Run"))
         (grp (clime-make-group :name "stuff" :inline t
                                :options (list opt)
                                :children (list (cons "run" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "stuff" grp))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Auth:" help))
    (should (string-match-p "--token" help))
    (should (string-match-p "^Commands:" help))))

;; 18. Same category from different branches → merged
(ert-deftest clime-test-help/cat-18-same-category-different-branches ()
  "Same category path from separate inline groups merges into one section."
  (let* ((cmd-a (clime-make-command :name "search" :handler #'ignore
                                     :help "Search beads"))
         (grp-a (clime-make-group :name "filter1" :inline t :category "Filter"
                                  :children (list (cons "search" cmd-a))))
         (cmd-b (clime-make-command :name "list" :handler #'ignore
                                     :help "List beads"))
         (grp-b (clime-make-group :name "filter2" :inline t :category "Filter"
                                  :children (list (cons "list" cmd-b))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "filter1" grp-a)
                                              (cons "filter2" grp-b))))
         (help (clime-format-help app '("t"))))
    ;; Single Filter: section with both commands
    (should (string-match-p "^Filter:" help))
    (should (string-match-p "search" help))
    (should (string-match-p "list" help))
    ;; Only one Filter: heading
    (should (= 1 (let ((count 0) (start 0))
                   (while (string-match "^Filter:" help start)
                     (cl-incf count)
                     (setq start (match-end 0)))
                   count)))))

;; 18b. Same composed category from different nested branches → merged
(ert-deftest clime-test-help/cat-18b-same-composed-path-merges ()
  "Same composed category path from different nested branches merges."
  (let* ((get-cmd (clime-make-command :name "get" :handler #'ignore
                                       :help "Get value"))
         (config-a (clime-make-group :name "config1" :inline t :category "Config"
                                     :children (list (cons "get" get-cmd))))
         (admin-a (clime-make-group :name "admin1" :inline t :category "Admin"
                                    :children (list (cons "config1" config-a))))
         (set-cmd (clime-make-command :name "set" :handler #'ignore
                                       :help "Set value"))
         (config-b (clime-make-group :name "config2" :inline t :category "Config"
                                     :children (list (cons "set" set-cmd))))
         (admin-b (clime-make-group :name "admin2" :inline t :category "Admin"
                                    :children (list (cons "config2" config-b))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin1" admin-a)
                                              (cons "admin2" admin-b))))
         (help (clime-format-help app '("t"))))
    ;; Both get and set under single "Config:" sub-heading within "Admin:"
    (should (string-match-p "^Admin:" help))
    (should (string-match-p "^  Config:" help))
    (should (string-match-p "get" help))
    (should (string-match-p "set" help))
    ;; Only one Config: sub-heading
    (should (= 1 (let ((count 0) (start 0))
                   (while (string-match "^  Config:" help start)
                     (cl-incf count)
                     (setq start (match-end 0)))
                   count)))))

;; 19. Interleaved sections in first-occurrence order
(ert-deftest clime-test-help/cat-19-interleaved-order ()
  "Category sections appear in first-occurrence order, interleaved."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")
                                  :count t :help "Verbosity"))
         (cmd-a (clime-make-command :name "search" :handler #'ignore
                                     :help "Search" :category "Filter"))
         (cmd-b (clime-make-command :name "show" :handler #'ignore
                                     :help "Show"))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "search" cmd-a)
                                              (cons "show" cmd-b))))
         (help (clime-format-help app '("t"))))
    ;; Options before Filter before Commands
    (let ((opts-pos (string-match "^Options:" help))
          (filter-pos (string-match "^Filter:" help))
          (cmds-pos (string-match "^Commands:" help)))
      (should opts-pos)
      (should filter-pos)
      (should cmds-pos)
      (should (< opts-pos filter-pos))
      (should (< filter-pos cmds-pos)))))

;; 20. Hidden items excluded
(ert-deftest clime-test-help/cat-20-hidden-excluded ()
  "Hidden options and commands are excluded from categorized sections."
  (let* ((opt (clime-make-option :name 'debug :flags '("--debug")
                                  :nargs 0 :help "Debug" :hidden t
                                  :category "Dev"))
         (cmd-a (clime-make-command :name "internal" :handler #'ignore
                                     :help "Internal" :hidden t
                                     :category "Dev"))
         (cmd-b (clime-make-command :name "show" :handler #'ignore
                                     :help "Show" :category "Dev"))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "internal" cmd-a)
                                              (cons "show" cmd-b))))
         (help (clime-format-help app '("t"))))
    ;; Only non-hidden "show" appears
    (should (string-match-p "show" help))
    (should-not (string-match-p "internal" help))
    (should-not (string-match-p "--debug" help))))

;; 21. Inline group with :help renders section description
(ert-deftest clime-test-help/cat-21-group-help-as-description ()
  "Inline group with :category and :help renders description under heading."
  (let* ((cmd (clime-make-command :name "search" :handler #'ignore
                                   :help "Search items"))
         (grp (clime-make-group :name "filter" :inline t
                                :category "Filter"
                                :help "Commands for filtering results"
                                :children (list (cons "search" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "filter" grp))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Filter:" help))
    (should (string-match-p "Commands for filtering results" help))
    (should (string-match-p "search" help))))

;; 22. Three-level nested inline groups
(ert-deftest clime-test-help/cat-22-three-level-inline-nesting ()
  "Three levels of nested inline groups compose category paths."
  (let* ((cmd (clime-make-command :name "get" :handler #'ignore
                                   :help "Get value"))
         (l3 (clime-make-group :name "keys" :inline t :category "Keys"
                                :children (list (cons "get" cmd))))
         (l2 (clime-make-group :name "config" :inline t :category "Config"
                                :children (list (cons "keys" l3))))
         (l1 (clime-make-group :name "admin" :inline t :category "Admin"
                                :children (list (cons "config" l2))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin" l1))))
         (help (clime-format-help app '("t"))))
    (should (string-match-p "^Admin:" help))
    (should (string-match-p "^  Config / Keys:" help))
    (should (string-match-p "get" help))))

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

;;; ─── Nested Category Sections ───────────────────────────────────────────

(ert-deftest clime-test-help/nested-categories-merged ()
  "Options with sub-categories and commands with parent category merge
into one section with indented sub-headings."
  (let* ((sect-opt (clime-make-option :name 'design :flags '("--design")
                                       :help "Set Design section"
                                       :category "Sections"))
         (note-opt (clime-make-option :name 'note :flags '("--note")
                                      :help "Append a note"
                                      :category "Notes"))
         (mutate (clime-make-group :name "mutate" :inline t
                                   :category "Lifecycle"
                                   :options (list sect-opt note-opt)
                                   :children (list
                                              (cons "update"
                                                    (clime-make-command
                                                     :name "update"
                                                     :handler #'ignore
                                                     :help "Update fields"))
                                              (cons "close"
                                                    (clime-make-command
                                                     :name "close"
                                                     :handler #'ignore
                                                     :help "Close bead")))))
         (claim (clime-make-command :name "claim" :handler #'ignore
                                    :help "Claim a bead"
                                    :category "Lifecycle"))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "mutate" mutate)
                                              (cons "claim" claim)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help app '("t"))))
      ;; Single "Lifecycle:" heading (not "Lifecycle / Sections:")
      (should (string-match-p "^Lifecycle:" help))
      (should-not (string-match-p "Lifecycle / " help))
      ;; Commands at standard indent under Lifecycle
      (should (string-match-p "^  update " help))
      (should (string-match-p "^  claim " help))
      ;; Sub-headings indented, with scope annotation
      (should (string-match-p "^  Sections (update, close):" help))
      (should (string-match-p "^  Notes (update, close):" help))
      ;; Options double-indented under sub-headings
      (should (string-match-p "^    --design " help))
      (should (string-match-p "^    --note " help)))))

(ert-deftest clime-test-help/flat-category-no-nesting ()
  "Category with no sub-categories renders flat (no sub-headings)."
  (let* ((cmd-a (clime-make-command :name "init" :handler #'ignore
                                     :help "Initialize" :category "Admin"))
         (cmd-b (clime-make-command :name "setup" :handler #'ignore
                                    :help "Setup wizard" :category "Admin"))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "init" cmd-a)
                                              (cons "setup" cmd-b)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help app '("t"))))
      ;; Single flat "Admin:" section
      (should (string-match-p "^Admin:" help))
      ;; Commands at standard indent
      (should (string-match-p "^  init " help))
      (should (string-match-p "^  setup " help)))))

(ert-deftest clime-test-help/uncategorized-still-standard ()
  "Uncategorized options go to 'Options:', commands to 'Commands:'."
  (let* ((opt (clime-make-option :name 'json :flags '("--json")
                                  :nargs 0 :help "JSON output"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :help "Show details"))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "show" cmd)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help app '("t"))))
      (should (string-match-p "^Options:" help))
      (should (string-match-p "^Commands:" help)))))

;;; ─── Command Footer ─────────────────────────────────────────────────

(ert-deftest clime-test-help/footer-shown-for-group ()
  "Branch nodes with children show a command help footer."
  (let ((help (clime-format-help clime-test--help-app '("myapp"))))
    (should (string-match-p
             "Run \"myapp COMMAND --help\" for more information on a command\\."
             help))))

(ert-deftest clime-test-help/footer-not-shown-for-leaf ()
  "Leaf commands do not show a command help footer."
  (let ((help (clime-format-help clime-test--help-cmd '("myapp" "show"))))
    (should-not (string-match-p "for more information on a command" help))))

(ert-deftest clime-test-help/footer-nested-group-path ()
  "Nested group footer uses full path."
  (let* ((cmd (clime-make-command :name "keys" :handler #'ignore
                                   :help "Manage keys"))
         (grp (clime-make-group :name "config" :help "Config management"
                                 :children (list (cons "keys" cmd))))
         (app (clime-make-app :name "myapp" :version "1"
                               :children (list (cons "config" grp)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help grp '("myapp" "config"))))
      (should (string-match-p
               "Run \"myapp config COMMAND --help\" for more information"
               help)))))

(ert-deftest clime-test-help/footer-after-epilog ()
  "Footer appears after epilog text."
  (let* ((cmd (clime-make-command :name "sub" :handler #'ignore
                                   :help "A sub"))
         (app (clime-make-app :name "myapp" :version "1"
                               :epilog "See https://example.com"
                               :children (list (cons "sub" cmd)))))
    (clime--set-parent-refs app)
    (let ((help (clime-format-help app '("myapp"))))
      (should (string-match-p "See https://example\\.com" help))
      (should (string-match-p "for more information on a command" help))
      ;; Footer comes after epilog
      (let ((epilog-pos (string-match "See https://example\\.com" help))
            (footer-pos (string-match "for more information" help)))
        (should (> footer-pos epilog-pos))))))

;;; ─── Text Wrapping ─────────────────────────────────────────────────────

(ert-deftest clime-test-help/wrap-text-basic ()
  "clime-help--wrap-text wraps at word boundaries."
  (let ((result (clime-help--wrap-text "one two three four five" 12)))
    (should (equal result "one two\nthree four\nfive"))))

(ert-deftest clime-test-help/wrap-text-preserves-newlines ()
  "clime-help--wrap-text preserves existing line breaks."
  (let ((result (clime-help--wrap-text "line one\nline two has words" 15)))
    (should (equal result "line one\nline two has\nwords"))))

(ert-deftest clime-test-help/wrap-text-with-prefix ()
  "clime-help--wrap-text prepends prefix to continuation lines.
Width applies to content; prefix is extra visual indentation."
  (let ((result (clime-help--wrap-text "one two three four" 10 "    ")))
    ;; "one two" (7) fits, "one two three" (13) doesn't → wrap
    (should (equal result "one two\n    three four"))))

(ert-deftest clime-test-help/wrap-text-long-word ()
  "Words longer than width are not broken."
  (let ((result (clime-help--wrap-text "short verylongwordhere end" 10)))
    (should (string-match-p "verylongwordhere" result))))

(ert-deftest clime-test-help/wrap-text-fits ()
  "Text that fits within width is not modified."
  (let ((result (clime-help--wrap-text "short text" 80)))
    (should (equal result "short text"))))

(ert-deftest clime-test-help/terminal-width-default ()
  "Default terminal width is 80 when COLUMNS is unset."
  (let ((clime-help-width nil)
        (process-environment (cl-remove-if
                              (lambda (e) (string-prefix-p "COLUMNS=" e))
                              process-environment)))
    (should (= 80 (clime-help--terminal-width)))))

(ert-deftest clime-test-help/terminal-width-from-var ()
  "clime-help-width overrides auto-detection."
  (let ((clime-help-width 60))
    (should (= 60 (clime-help--terminal-width)))))

(ert-deftest clime-test-help/terminal-width-from-columns ()
  "COLUMNS env var is used for auto-detection."
  (let ((clime-help-width nil)
        (process-environment (cons "COLUMNS=120" process-environment)))
    (should (= 120 (clime-help--terminal-width)))))

(ert-deftest clime-test-help/terminal-width-minimum ()
  "Width is clamped to minimum 40."
  (let ((clime-help-width 20))
    (should (= 40 (clime-help--terminal-width)))))

(ert-deftest clime-test-help/table-wraps-help-column ()
  "Table help text wraps when it would exceed terminal width."
  (let* ((clime-help-width 50)
         (opt (clime-make-option :name 'output
                                 :flags '("--output" "-o")
                                 :help "Set the output format for all commands in the system"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :help "Show"
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "show"))))
    ;; No line in the options section should exceed 50 chars
    (let ((lines (split-string help "\n")))
      (dolist (line lines)
        (should (<= (length line) 50))))))

(ert-deftest clime-test-help/description-wraps ()
  "Description paragraph wraps at terminal width."
  (let* ((clime-help-width 40)
         (cmd (clime-make-command
               :name "show" :handler #'ignore
               :help "This is a long description that should be wrapped at the terminal width boundary")))
    (let ((help (clime-format-help cmd '("app" "show"))))
      (dolist (line (split-string help "\n"))
        (should (<= (length line) 40))))))

(ert-deftest clime-test-help/epilog-wraps ()
  "Epilog text wraps at terminal width."
  (let* ((clime-help-width 40)
         (cmd (clime-make-command :name "sub" :handler #'ignore :help "Sub"))
         (app (clime-make-app :name "app" :version "1"
                               :epilog "This is an epilog with enough words to require wrapping at narrow widths"
                               :children (list (cons "sub" cmd)))))
    (clime--set-parent-refs app)
    (let* ((help (clime-format-help app '("app")))
           (lines (split-string help "\n")))
      ;; Check epilog lines are wrapped (skip usage, footer, and empty lines)
      (let ((epilog-lines (cl-remove-if
                           (lambda (l) (or (string-prefix-p "Usage:" l)
                                           (string-prefix-p "Run \"" l)
                                           (string-empty-p l)))
                           lines)))
        (dolist (line epilog-lines)
          (should (<= (length line) 40)))))))

(ert-deftest clime-test-help/usage-not-wrapped ()
  "Usage line is not wrapped even when it exceeds terminal width."
  (let* ((clime-help-width 40)
         (cmd (clime-make-command
               :name "deploy" :handler #'ignore
               :help "Deploy"
               :args (list (clime-make-arg :name 'target)
                           (clime-make-arg :name 'environment)))))
    (let* ((help (clime-format-help cmd '("myapp" "infra" "deploy")))
           (usage-line (car (split-string help "\n"))))
      ;; Usage line should be intact, not wrapped
      (should (string-match-p "^Usage: myapp infra deploy" usage-line)))))

(provide 'clime-help-tests)
;;; clime-help-tests.el ends here
