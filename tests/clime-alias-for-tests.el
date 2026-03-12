;;; clime-alias-for-tests.el --- Tests for clime-alias-for  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the `clime-alias-for' DSL form and alias resolution.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-dsl)
(require 'clime-test-helpers)

;;; ─── Test Fixtures ──────────────────────────────────────────────────────

(defun clime-test--alias-for-app ()
  "Build an app with a group and an alias-for to a nested command."
  (let* ((cmd-arg (clime-make-arg :name 'name :help "Agent name"))
         (cmd-opt (clime-make-option :name 'prompt :flags '("--prompt")
                                     :help "Initial prompt"))
         (cmd-start (clime-make-command :name "start"
                                        :help "Start an agent"
                                        :handler #'ignore
                                        :args (list cmd-arg)
                                        :options (list cmd-opt)))
         (grp (clime-make-group :name "agents"
                                :help "Manage agents"
                                :children (list (cons "start" cmd-start))))
         (alias (clime-command--create
                 :name "start"
                 :help "Start an agent (shortcut)"
                 :category "Shortcuts"
                 :alias-for '("agents" "start"))))
    (clime-make-app :name "myapp"
                    :children (list (cons "agents" grp)
                                    (cons "start" alias)))))

;;; ─── Resolution ─────────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/resolves-args-and-options ()
  "Alias copies args, options, and handler from the target command."
  (let* ((app (clime-test--alias-for-app))
         (_ (clime--resolve-aliases app))
         (alias (cdr (assoc "start" (clime-group-children app)))))
    (should (clime-command-p alias))
    ;; Args copied from target
    (should (= 1 (length (clime-node-args alias))))
    (should (eq 'name (clime-arg-name (car (clime-node-args alias)))))
    ;; Options copied from target
    (should (= 1 (length (clime-node-options alias))))
    (should (eq 'prompt (clime-option-name (car (clime-node-options alias)))))
    ;; Handler copied from target
    (should (functionp (clime-node-handler alias)))))

(ert-deftest clime-test-alias-for/preserves-own-help ()
  "Alias keeps its own :help when provided."
  (let* ((app (clime-test--alias-for-app))
         (_ (clime--resolve-aliases app))
         (alias (cdr (assoc "start" (clime-group-children app)))))
    (should (equal "Start an agent (shortcut)" (clime-node-help alias)))))

(ert-deftest clime-test-alias-for/preserves-own-category ()
  "Alias keeps its own :category."
  (let* ((app (clime-test--alias-for-app))
         (_ (clime--resolve-aliases app))
         (alias (cdr (assoc "start" (clime-group-children app)))))
    (should (equal "Shortcuts" (clime-node-category alias)))))

(ert-deftest clime-test-alias-for/inherits-help-when-nil ()
  "Alias inherits target's :help when its own is nil."
  (let* ((cmd (clime-make-command :name "stop"
                                   :help "Stop an agent"
                                   :handler #'ignore))
         (grp (clime-make-group :name "agents"
                                :children (list (cons "stop" cmd))))
         (alias (clime-command--create
                 :name "stop"
                 :alias-for '("agents" "stop")))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "agents" grp)
                                              (cons "stop" alias)))))
    (clime--resolve-aliases app)
    (let ((sc (cdr (assoc "stop" (clime-group-children app)))))
      (should (equal "Stop an agent" (clime-node-help sc))))))

(ert-deftest clime-test-alias-for/clears-target-after-resolve ()
  "After resolution, alias-for is nil (resolved marker)."
  (let* ((app (clime-test--alias-for-app))
         (_ (clime--resolve-aliases app))
         (alias (cdr (assoc "start" (clime-group-children app)))))
    (should-not (clime-command-alias-for alias))))

(ert-deftest clime-test-alias-for/idempotent ()
  "Calling resolve-aliases twice is safe."
  (let* ((app (clime-test--alias-for-app)))
    (clime--resolve-aliases app)
    (clime--resolve-aliases app)
    (let ((alias (cdr (assoc "start" (clime-group-children app)))))
      (should (functionp (clime-node-handler alias))))))

;;; ─── Error Cases ────────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/error-target-not-found ()
  "Error when alias target path does not resolve."
  (let* ((alias (clime-command--create
                 :name "start"
                 :alias-for '("nonexistent" "start")))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "start" alias)))))
    (should-error (clime--resolve-aliases app) :type 'error)))

(ert-deftest clime-test-alias-for/error-target-is-group ()
  "Error when alias target resolves to a group, not a command."
  (let* ((grp (clime-make-group :name "agents"
                                :children nil))
         (alias (clime-command--create
                 :name "agents-sc"
                 :alias-for '("agents")))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "agents" grp)
                                              (cons "agents-sc" alias)))))
    (should-error (clime--resolve-aliases app) :type 'error)))

(ert-deftest clime-test-alias-for/error-circular ()
  "Error on circular alias chain."
  (let* ((sc-a (clime-command--create
                :name "a"
                :alias-for '("b")))
         (sc-b (clime-command--create
                :name "b"
                :alias-for '("a")))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "a" sc-a)
                                              (cons "b" sc-b)))))
    (should-error (clime--resolve-aliases app) :type 'error)))

;;; ─── Transitive Resolution ─────────────────────────────────────────────

(ert-deftest clime-test-alias-for/transitive ()
  "Alias to an alias resolves transitively."
  (let* ((cmd (clime-make-command :name "start"
                                   :help "Start"
                                   :handler #'ignore
                                   :args (list (clime-make-arg :name 'name))))
         (grp (clime-make-group :name "agents"
                                :children (list (cons "start" cmd))))
         (sc1 (clime-command--create
               :name "go"
               :alias-for '("agents" "start")))
         (sc2 (clime-command--create
               :name "g"
               :alias-for '("go")))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "agents" grp)
                                              (cons "go" sc1)
                                              (cons "g" sc2)))))
    (clime--resolve-aliases app)
    (let ((g (cdr (assoc "g" (clime-group-children app)))))
      (should (= 1 (length (clime-node-args g))))
      (should (functionp (clime-node-handler g))))))

;;; ─── Parsing ────────────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/parse-alias-args ()
  "Alias parses args and options like the target command."
  (let* ((app (clime-test--alias-for-app))
         (result (clime-parse app '("start" "myagent" "--prompt" "hello"))))
    ;; Should resolve to the alias command and parse its args
    (should (clime-command-p (clime-parse-result-node result)))
    (should (equal "myagent" (plist-get (clime-parse-result-params result) 'name)))
    (should (equal "hello" (plist-get (clime-parse-result-params result) 'prompt)))))

(ert-deftest clime-test-alias-for/parse-target-still-works ()
  "Original nested command still works alongside its alias."
  (let* ((app (clime-test--alias-for-app))
         (result (clime-parse app '("agents" "start" "myagent" "--prompt" "hello"))))
    (should (clime-command-p (clime-parse-result-node result)))
    (should (equal "myagent" (plist-get (clime-parse-result-params result) 'name)))
    (should (equal "hello" (plist-get (clime-parse-result-params result) 'prompt)))))

;;; ─── DSL ────────────────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/dsl-basic ()
  "clime-alias-for DSL form creates a working alias."
  (eval '(clime-app clime-test--af-dsl-basic
           :version "1"
           (clime-group agents
             :help "Manage agents"
             (clime-command start
               :help "Start an agent"
               (clime-arg name :help "Agent name")
               (clime-option prompt ("--prompt") :help "Initial prompt")
               (clime-handler (ctx) nil)))
           (clime-group shortcuts :inline t :category "Shortcuts"
             (clime-alias-for start (agents start)
               :help "Start (shortcut)")))
        t)
  (let* ((result (clime-parse clime-test--af-dsl-basic
                              '("start" "myagent" "--prompt" "hi")))
         (params (clime-parse-result-params result)))
    (should (equal "myagent" (plist-get params 'name)))
    (should (equal "hi" (plist-get params 'prompt)))))

(ert-deftest clime-test-alias-for/dsl-with-aliases ()
  "clime-alias-for DSL form supports :aliases."
  (eval '(clime-app clime-test--af-dsl-aliases
           :version "1"
           (clime-group agents
             :help "Manage"
             (clime-command start
               :help "Start"
               (clime-arg name :help "Name")
               (clime-handler (ctx) nil)))
           (clime-alias-for go (agents start)
             :aliases (g)
             :help "Start (shortcut)"))
        t)
  ;; Alias "g" should resolve to the alias-for command
  (let* ((result (clime-parse clime-test--af-dsl-aliases '("g" "myagent")))
         (params (clime-parse-result-params result)))
    (should (equal "myagent" (plist-get params 'name)))))

(ert-deftest clime-test-alias-for/dsl-hidden ()
  "clime-alias-for DSL form supports :hidden."
  (eval '(clime-app clime-test--af-dsl-hidden
           :version "1"
           (clime-command greet
             :help "Greet"
             (clime-handler (ctx) nil))
           (clime-alias-for hi (greet) :hidden t))
        t)
  (let ((sc (cdr (assoc "hi" (clime-group-children clime-test--af-dsl-hidden)))))
    (should (clime-node-hidden sc))))

;;; ─── :defaults ─────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/defaults-struct-slot ()
  "alias-defaults slot exists and defaults to nil."
  (let ((cmd (clime-command--create :name "x" :handler #'ignore)))
    (should-not (clime-command-alias-defaults cmd))))

(ert-deftest clime-test-alias-for/defaults-patches-option-default ()
  "alias :defaults overrides the :default on copied options."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-defaults '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (clime--resolve-aliases app)
    (let* ((resolved (cdr (assoc "show-csv" (clime-group-children app))))
           (opt (car (clime-node-options resolved))))
      (should (equal "csv" (clime-option-default opt))))))

(ert-deftest clime-test-alias-for/defaults-visible-in-parse ()
  "alias :defaults value is used when option not provided on CLI."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-defaults '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (let* ((result (clime-parse app '("show-csv")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/defaults-overridable-by-cli ()
  "User can override alias :defaults on the CLI."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-defaults '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (let* ((result (clime-parse app '("show-csv" "--format" "json")))
           (params (clime-parse-result-params result)))
      (should (equal "json" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/defaults-does-not-mutate-target ()
  "alias :defaults does not change the original command's options."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-defaults '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (clime--resolve-aliases app)
    ;; Original target keeps its default
    (let ((orig-opt (car (clime-node-options cmd))))
      (should (equal "text" (clime-option-default orig-opt))))))

(ert-deftest clime-test-alias-for/defaults-error-unknown-option ()
  "Error when :defaults names a non-existent option."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "bad"
                 :alias-for '("report" "show")
                 :alias-defaults '((nonexistent . "x"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "bad" alias)))))
    (should-error (clime--resolve-aliases app) :type 'error)))

;;; ─── :vals ─────────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/vals-struct-slot ()
  "alias-vals slot exists and defaults to nil."
  (let ((cmd (clime-command--create :name "x" :handler #'ignore)))
    (should-not (clime-command-alias-vals cmd))))

(ert-deftest clime-test-alias-for/vals-removes-option-from-list ()
  "alias :vals removes the option from the command's options list."
  (let* ((opt-a (clime-make-option :name 'format :flags '("--format")))
         (opt-b (clime-make-option :name 'verbose :flags '("--verbose") :nargs 0))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt-a opt-b)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (clime--resolve-aliases app)
    (let ((resolved (cdr (assoc "show-csv" (clime-group-children app)))))
      ;; Only --verbose remains
      (should (= 1 (length (clime-node-options resolved))))
      (should (eq 'verbose (clime-option-name (car (clime-node-options resolved))))))))

(ert-deftest clime-test-alias-for/vals-injected-into-params ()
  "alias :vals value appears in parsed params."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (let* ((result (clime-parse app '("show-csv")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/vals-not-overridable ()
  "alias :vals option is removed from CLI — passing the flag is an error."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (should-error (clime-parse app '("show-csv" "--format" "json"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-alias-for/vals-error-unknown-option ()
  "Error when :vals names a non-existent option."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "bad"
                 :alias-for '("report" "show")
                 :alias-vals '((nonexistent . "x"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "bad" alias)))))
    (should-error (clime--resolve-aliases app) :type 'error)))

;;; ─── DSL :defaults / :vals ─────────────────────────────────────────

(ert-deftest clime-test-alias-for/dsl-defaults ()
  "clime-alias-for DSL form supports :defaults."
  (eval '(clime-app clime-test--af-dsl-defaults
           :version "1"
           (clime-group report
             :help "Reports"
             (clime-command show
               :help "Show report"
               (clime-option format ("--format") :default "text"
                 :help "Output format")
               (clime-handler (ctx)
                 (clime-ctx-get ctx 'format))))
           (clime-alias-for show-csv (report show)
             :help "Show as CSV"
             :defaults '((format . "csv"))))
        t)
  (let* ((result (clime-parse clime-test--af-dsl-defaults '("show-csv")))
         (params (clime-parse-result-params result)))
    (should (equal "csv" (plist-get params 'format)))))

(ert-deftest clime-test-alias-for/dsl-vals ()
  "clime-alias-for DSL form supports :vals."
  (eval '(clime-app clime-test--af-dsl-vals
           :version "1"
           (clime-group report
             :help "Reports"
             (clime-command show
               :help "Show report"
               (clime-option format ("--format") :default "text"
                 :help "Output format")
               (clime-handler (ctx)
                 (clime-ctx-get ctx 'format))))
           (clime-alias-for show-csv (report show)
             :help "Show as CSV"
             :vals '((format . "csv"))))
        t)
  ;; Locked value injected
  (let* ((result (clime-parse clime-test--af-dsl-vals '("show-csv")))
         (params (clime-parse-result-params result)))
    (should (equal "csv" (plist-get params 'format))))
  ;; Option removed — can't pass it
  (should-error (clime-parse clime-test--af-dsl-vals '("show-csv" "--format" "json"))
                :type 'clime-usage-error))

(ert-deftest clime-test-alias-for/defaults-satisfies-required ()
  "alias :defaults satisfies a :required option."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :required t))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-defaults '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    ;; Should not error — default satisfies required
    (let* ((result (clime-parse app '("show-csv")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/vals-satisfies-required ()
  "alias :vals satisfies a :required option."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :required t))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-command--create
                 :name "show-csv"
                 :alias-for '("report" "show")
                 :alias-vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    ;; Should not error — val is injected before required check
    (let* ((result (clime-parse app '("show-csv")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

(provide 'clime-alias-for-tests)
;;; clime-alias-for-tests.el ends here
