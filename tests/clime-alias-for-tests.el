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
         (alias (clime-alias--create
                 :name "start"
                 :help "Start an agent (shortcut)"
                 :category "Shortcuts"
                 :target '("agents" "start"))))
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
         (alias (clime-alias--create
                 :name "stop"
                 :target '("agents" "stop")))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "agents" grp)
                                              (cons "stop" alias)))))
    (clime--resolve-aliases app)
    (let ((sc (cdr (assoc "stop" (clime-group-children app)))))
      (should (equal "Stop an agent" (clime-node-help sc))))))

(ert-deftest clime-test-alias-for/resolved-at-construction ()
  "Aliases are resolved at construction time — no clime-alias nodes remain."
  (let* ((app (clime-test--alias-for-app))
         (child (cdr (assoc "start" (clime-group-children app)))))
    (should (clime-command-p child))
    (should-not (clime-alias-p child))
    (should (equal "Start an agent (shortcut)" (clime-node-help child)))))

(ert-deftest clime-test-alias-for/replaced-with-command-after-resolve ()
  "After resolution, alias is replaced with a real command in children."
  (let* ((app (clime-test--alias-for-app))
         (_ (clime--resolve-aliases app))
         (resolved (cdr (assoc "start" (clime-group-children app)))))
    (should (clime-command-p resolved))
    (should-not (clime-alias-p resolved))))

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
  (let ((alias (clime-alias--create
                :name "start"
                :target '("nonexistent" "start"))))
    (should-error (clime-make-app :name "myapp"
                                  :children (list (cons "start" alias)))
                  :type 'error)))

(ert-deftest clime-test-alias-for/error-target-is-group ()
  "Error when alias target resolves to a group, not a command."
  (let ((grp (clime-make-group :name "agents"
                               :children nil))
        (alias (clime-alias--create
                :name "agents-sc"
                :target '("agents"))))
    (should-error (clime-make-app :name "myapp"
                                  :children (list (cons "agents" grp)
                                                  (cons "agents-sc" alias)))
                  :type 'error)))

(ert-deftest clime-test-alias-for/error-circular ()
  "Error on circular alias chain."
  (let ((sc-a (clime-alias--create
               :name "a"
               :target '("b")))
        (sc-b (clime-alias--create
               :name "b"
               :target '("a"))))
    (should-error (clime-make-app :name "myapp"
                                  :children (list (cons "a" sc-a)
                                                  (cons "b" sc-b)))
                  :type 'error)))

;;; ─── Transitive Resolution ─────────────────────────────────────────────

(ert-deftest clime-test-alias-for/transitive ()
  "Alias to an alias resolves transitively."
  (let* ((cmd (clime-make-command :name "start"
                                   :help "Start"
                                   :handler #'ignore
                                   :args (list (clime-make-arg :name 'name))))
         (grp (clime-make-group :name "agents"
                                :children (list (cons "start" cmd))))
         (sc1 (clime-alias--create
               :name "go"
               :target '("agents" "start")))
         (sc2 (clime-alias--create
               :name "g"
               :target '("go")))
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
  "Alias struct defaults slot exists and defaults to nil."
  (let ((alias (clime-alias--create :target '("x"))))
    (should-not (clime-alias-defaults alias))))

(ert-deftest clime-test-alias-for/defaults-patches-option-default ()
  "alias :defaults overrides the :default on copied options."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :defaults '((format . "csv"))))
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
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :defaults '((format . "csv"))))
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
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :defaults '((format . "csv"))))
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
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :defaults '((format . "csv"))))
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
         (alias (clime-alias--create
                 :name "bad"
                 :target '("report" "show")
                 :defaults '((nonexistent . "x")))))
    (should-error (clime-make-app :name "myapp"
                                  :children (list (cons "report" grp)
                                                  (cons "bad" alias)))
                  :type 'error)))

(ert-deftest clime-test-alias-for/defaults-does-not-lock ()
  "alias :defaults sets :default but NOT :locked."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :defaults '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (clime--resolve-aliases app)
    (let* ((resolved (cdr (assoc "show-csv" (clime-group-children app))))
           (fmt-opt (cl-find-if (lambda (o) (eq (clime-option-name o) 'format))
                                (clime-node-options resolved))))
      (should (equal "csv" (clime-option-default fmt-opt)))
      (should-not (clime-option-locked fmt-opt)))))

;;; ─── :vals ─────────────────────────────────────────────────────────

(ert-deftest clime-test-alias-for/vals-struct-slot ()
  "Alias struct vals slot exists and defaults to nil."
  (let ((alias (clime-alias--create :target '("x"))))
    (should-not (clime-alias-vals alias))))

(ert-deftest clime-test-alias-for/vals-locks-option ()
  "alias :vals sets :locked on the option and value in :default."
  (let* ((opt-a (clime-make-option :name 'format :flags '("--format")))
         (opt-b (clime-make-option :name 'verbose :flags '("--verbose") :nargs 0))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt-a opt-b)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (clime--resolve-aliases app)
    (let* ((resolved (cdr (assoc "show-csv" (clime-group-children app))))
           (fmt-opt (cl-find-if (lambda (o) (eq (clime-option-name o) 'format))
                                (clime-node-options resolved))))
      ;; Both options remain
      (should (= 2 (length (clime-node-options resolved))))
      ;; format is locked
      (should fmt-opt)
      (should (clime-option-locked fmt-opt))
      (should (equal "csv" (clime-param-value fmt-opt)))
      (should (eq 'app (clime-param-source fmt-opt)))
      ;; verbose is not locked
      (should-not (clime-option-locked
                   (cl-find-if (lambda (o) (eq (clime-option-name o) 'verbose))
                               (clime-node-options resolved)))))))

(ert-deftest clime-test-alias-for/vals-stored-as-locked-vals ()
  "alias :vals are stored as locked-vals on the resolved command."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (clime--resolve-aliases app)
    (let ((resolved (cdr (assoc "show-csv" (clime-group-children app)))))
      (should (equal '((format . "csv")) (clime-node-locked-vals resolved))))))

(ert-deftest clime-test-alias-for/vals-injected-into-params ()
  "alias :vals value appears in parsed params."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    (let* ((result (clime-parse app '("show-csv")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/vals-not-overridable ()
  "alias :vals option is locked — passing the flag from CLI is an error."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                   :options (list opt)))
         (grp (clime-make-group :name "report"
                                :children (list (cons "show" cmd))))
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :vals '((format . "csv"))))
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
         (alias (clime-alias--create
                 :name "bad"
                 :target '("report" "show")
                 :vals '((nonexistent . "x")))))
    (should-error (clime-make-app :name "myapp"
                                  :children (list (cons "report" grp)
                                                  (cons "bad" alias)))
                  :type 'error)))

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
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :defaults '((format . "csv"))))
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
         (alias (clime-alias--create
                 :name "show-csv"
                 :target '("report" "show")
                 :vals '((format . "csv"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)
                                              (cons "show-csv" alias)))))
    ;; Should not error — val is injected before required check
    (let* ((result (clime-parse app '("show-csv")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

;;; ─── Parent Ref After Resolution ───────────────────────────────────────

(ert-deftest clime-test-alias-for/resolved-alias-has-parent ()
  "Resolved alias has non-nil parent pointing to its containing group."
  (let* ((app (clime-make-app
               :name "test"
               :children
               `(("show" . ,(clime-make-command :name "show"
                              :handler #'ignore))
                 ("s" . ,(clime-alias--create :name "s"
                           :target '("show")))))))
    (clime--set-parent-refs app)
    (clime--resolve-aliases app)
    (let ((resolved (cdr (assoc "s" (clime-group-children app)))))
      (should (clime-command-p resolved))
      (should (eq (clime-node-parent resolved) app)))))

(ert-deftest clime-test-alias-for/resolved-alias-ancestor-walk ()
  "Ancestor walk from resolved alias reaches root."
  (let* ((app (clime-make-app
               :name "test"
               :children
               `(("grp" . ,(clime-make-group
                            :name "grp"
                            :children
                            `(("show" . ,(clime-make-command :name "show"
                                           :handler #'ignore))
                              ("s" . ,(clime-alias--create :name "s"
                                        :target '("grp" "show"))))))))))
    (clime--set-parent-refs app)
    (clime--resolve-aliases app)
    (let* ((grp (cdr (assoc "grp" (clime-group-children app))))
           (resolved (cdr (assoc "s" (clime-group-children grp)))))
      (should (eq (clime-node-parent resolved) grp))
      (should (equal (mapcar #'clime-node-name (clime-node-ancestors resolved))
                     '("grp" "test"))))))

(ert-deftest clime-test-alias-for/resolved-alias-inherits-ancestor-options ()
  "Resolved alias sees ancestor options during parse."
  (let* ((app (clime-make-app
               :name "test"
               :options (list (clime-make-option :name 'verbose
                                :flags '("--verbose") :nargs 0))
               :children
               `(("show" . ,(clime-make-command :name "show"
                              :handler #'ignore))
                 ("s" . ,(clime-alias--create :name "s"
                           :target '("show")))))))
    (let* ((result (clime-parse app '("s" "--verbose")))
           (params (clime-parse-result-params result)))
      (should (eq t (plist-get params 'verbose))))))

;;; ─── Ancestor locked-vals inheritance ────────────────────────────────

(ert-deftest clime-test-alias-for/ancestor-locked-vals-inherited ()
  "locked-vals on ancestor group are injected into child command params."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore))
         (grp (clime-make-group :name "report"
                                :locked-vals '((format . "csv"))
                                :children (list (cons "show" cmd))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)))))
    (let* ((result (clime-parse app '("report" "show")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/leaf-locked-vals-override-ancestor ()
  "Leaf locked-vals take priority over ancestor locked-vals for same key."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                    :locked-vals '((format . "json"))))
         (grp (clime-make-group :name "report"
                                :locked-vals '((format . "csv"))
                                :children (list (cons "show" cmd))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)))))
    (let* ((result (clime-parse app '("report" "show")))
           (params (clime-parse-result-params result)))
      (should (equal "json" (plist-get params 'format))))))

(ert-deftest clime-test-alias-for/ancestor-locked-vals-merge-with-leaf ()
  "Ancestor and leaf locked-vals for different keys both appear in params."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                    :locked-vals '((limit . 10))))
         (grp (clime-make-group :name "report"
                                :locked-vals '((format . "csv"))
                                :children (list (cons "show" cmd))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "report" grp)))))
    (let* ((result (clime-parse app '("report" "show")))
           (params (clime-parse-result-params result)))
      (should (equal "csv" (plist-get params 'format)))
      (should (equal 10 (plist-get params 'limit))))))

;;; ─── :vals / :defaults with mutex groups ─────────────────────────────

(ert-deftest clime-test-alias-for/vals-with-mutex-option ()
  "alias :vals targeting an option inside clime-mutex resolves without error."
  (eval '(clime-app clime-test--af-vals-mutex
           :version "1"
           (clime-command query
             :help "Run a query"
             (clime-mutex query-mode
               (clime-opt todo ("--todo" "-t") :help "TODO keyword")
               (clime-opt sexp ("--sexp") :help "S-expression query"))
             (clime-handler (ctx)
               (clime-let ctx (todo sexp)
                 (or todo sexp))))
           (clime-group shortcuts :inline :category "Shortcuts"
             (clime-alias-for waiting (query)
               :help "Show WAITING items"
               :vals '((todo . "WAITING")))))
        t)
  ;; Alias resolves — no error about unknown option
  (let* ((shortcuts (cdr (assoc "shortcuts"
                                 (clime-group-children
                                  clime-test--af-vals-mutex))))
         (resolved (cdr (assoc "waiting"
                                (clime-group-children shortcuts))))
         (all-opt-names (mapcar #'clime-option-name
                                (clime-node-all-options resolved))))
    ;; todo locked by :vals but still in options list, sexp remains
    (should (memq 'todo all-opt-names))
    (should (clime-option-locked
             (cl-find-if (lambda (o) (eq (clime-option-name o) 'todo))
                          (clime-node-all-options resolved))))
    (should (memq 'sexp all-opt-names))
    ;; locked-vals stored
    (should (equal '((todo . "WAITING")) (clime-node-locked-vals resolved))))
  ;; Parse works — locked val injected
  (let* ((result (clime-parse clime-test--af-vals-mutex '("waiting")))
         (params (clime-parse-result-params result)))
    (should (equal "WAITING" (plist-get params 'todo)))))

(ert-deftest clime-test-alias-for/defaults-with-mutex-option ()
  "alias :defaults targeting an option inside clime-mutex resolves without error."
  (eval '(clime-app clime-test--af-defaults-mutex
           :version "1"
           (clime-command query
             :help "Run a query"
             (clime-mutex query-mode
               (clime-opt todo ("--todo" "-t") :help "TODO keyword")
               (clime-opt sexp ("--sexp") :help "S-expression query"))
             (clime-handler (ctx)
               (clime-let ctx (todo)
                 todo)))
           (clime-alias-for pending (query)
             :help "Show TODO items"
             :defaults '((todo . "TODO"))))
        t)
  ;; Default patched on the mutex-nested option
  (let* ((result (clime-parse clime-test--af-defaults-mutex '("pending")))
         (params (clime-parse-result-params result)))
    (should (equal "TODO" (plist-get params 'todo))))
  ;; Can still override
  (let* ((result (clime-parse clime-test--af-defaults-mutex
                              '("pending" "--todo" "DONE")))
         (params (clime-parse-result-params result)))
    (should (equal "DONE" (plist-get params 'todo)))))

(ert-deftest clime-test-alias-for/vals-mutex-does-not-mutate-target ()
  "alias :vals on mutex option does not affect the original command."
  (eval '(clime-app clime-test--af-vals-mutex-nomut
           :version "1"
           (clime-command query
             :help "Query"
             (clime-mutex qm
               (clime-opt todo ("--todo") :help "Keyword")
               (clime-opt sexp ("--sexp") :help "Sexp"))
             (clime-handler (ctx) nil))
           (clime-alias-for waiting (query)
             :vals '((todo . "WAITING"))))
        t)
  ;; Original query command still has todo in its mutex
  (let* ((query (cdr (assoc "query"
                             (clime-group-children
                              clime-test--af-vals-mutex-nomut))))
         (all-opt-names (mapcar #'clime-option-name
                                (clime-node-all-options query))))
    (should (memq 'todo all-opt-names))
    (should (memq 'sexp all-opt-names))))

;;; ─── Group Lock Propagation (clime-di0) ─────────────────────────────

(ert-deftest clime-test-alias-for/vals-mutex-locks-siblings ()
  "alias :vals locking a mutex member locks all siblings in the group."
  (let* ((exclusive (clime-check-exclusive 'qm '(todo sexp)))
         (opt-todo (clime-make-option :name 'todo :flags '("--todo")))
         (opt-sexp (clime-make-option :name 'sexp :flags '("--sexp")))
         (mutex (clime-make-group :name "qm" :inline t
                                  :options (list opt-todo opt-sexp)
                                  :conform (list exclusive)))
         (cmd (clime-make-command :name "query" :handler #'ignore
                                  :children (list (cons "qm" mutex))))
         (grp (clime-make-group :name "root"
                                :children (list (cons "query" cmd))))
         (alias (clime-alias--create
                 :name "waiting"
                 :target '("root" "query")
                 :vals '((todo . "WAITING"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "root" grp)
                                              (cons "waiting" alias)))))
    (let* ((resolved (cdr (assoc "waiting" (clime-group-children app))))
           (sexp-opt (cl-find-if (lambda (o) (eq (clime-option-name o) 'sexp))
                                 (clime-node-all-options resolved))))
      ;; sexp must be locked as a sibling of locked todo
      (should (clime-option-locked sexp-opt))
      ;; sexp has no value — it was excluded, not set
      (should-not (clime-option-default sexp-opt)))))

(ert-deftest clime-test-alias-for/vals-mutex-all-locked-no-extra ()
  "When :vals locks all mutex members, no extra propagation needed."
  (let* ((exclusive (clime-check-exclusive 'qm '(todo sexp)))
         (opt-todo (clime-make-option :name 'todo :flags '("--todo")))
         (opt-sexp (clime-make-option :name 'sexp :flags '("--sexp")))
         (mutex (clime-make-group :name "qm" :inline t
                                  :options (list opt-todo opt-sexp)
                                  :conform (list exclusive)))
         (cmd (clime-make-command :name "query" :handler #'ignore
                                  :children (list (cons "qm" mutex))))
         (grp (clime-make-group :name "root"
                                :children (list (cons "query" cmd))))
         (alias (clime-alias--create
                 :name "both-locked"
                 :target '("root" "query")
                 :vals '((todo . "WAITING") (sexp . "(active)"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "root" grp)
                                              (cons "both-locked" alias)))))
    (let* ((resolved (cdr (assoc "both-locked" (clime-group-children app)))))
      ;; Both locked with their values
      (let ((todo (cl-find-if (lambda (o) (eq (clime-option-name o) 'todo))
                              (clime-node-all-options resolved)))
            (sexp (cl-find-if (lambda (o) (eq (clime-option-name o) 'sexp))
                              (clime-node-all-options resolved))))
        (should (clime-option-locked todo))
        (should (equal "WAITING" (clime-param-value todo)))
        (should (clime-option-locked sexp))
        (should (equal "(active)" (clime-param-value sexp)))))))

(ert-deftest clime-test-alias-for/vals-zip-locks-siblings ()
  "alias :vals locking a zip member locks all siblings in the group."
  (let* ((paired (clime-check-paired 'zg '(skip reason)))
         (opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (zip (clime-make-group :name "zg" :inline t
                                :options (list opt-skip opt-reason)
                                :conform (list paired)))
         (cmd (clime-make-command :name "process" :handler #'ignore
                                  :children (list (cons "zg" zip))))
         (grp (clime-make-group :name "root"
                                :children (list (cons "process" cmd))))
         (alias (clime-alias--create
                 :name "skip-all"
                 :target '("root" "process")
                 :vals '((skip . ("a" "b")))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "root" grp)
                                              (cons "skip-all" alias)))))
    (let* ((resolved (cdr (assoc "skip-all" (clime-group-children app))))
           (reason-opt (cl-find-if (lambda (o) (eq (clime-option-name o) 'reason))
                                   (clime-node-all-options resolved))))
      ;; reason must be locked as a sibling of locked skip
      (should (clime-option-locked reason-opt))
      (should-not (clime-option-default reason-opt)))))

(ert-deftest clime-test-alias-for/vals-plain-group-no-propagation ()
  "Plain inline groups (no conformer) do not propagate locks."
  (let* ((opt-a (clime-make-option :name 'alpha :flags '("--alpha")))
         (opt-b (clime-make-option :name 'beta :flags '("--beta")))
         (grp-inline (clime-make-group :name "plain" :inline t
                                       :options (list opt-a opt-b)))
         (cmd (clime-make-command :name "cmd" :handler #'ignore
                                  :children (list (cons "plain" grp-inline))))
         (top (clime-make-group :name "root"
                                :children (list (cons "cmd" cmd))))
         (alias (clime-alias--create
                 :name "locked-a"
                 :target '("root" "cmd")
                 :vals '((alpha . "x"))))
         (app (clime-make-app :name "myapp"
                              :children (list (cons "root" top)
                                              (cons "locked-a" alias)))))
    (let* ((resolved (cdr (assoc "locked-a" (clime-group-children app))))
           (beta-opt (cl-find-if (lambda (o) (eq (clime-option-name o) 'beta))
                                 (clime-node-all-options resolved))))
      ;; beta should NOT be locked — plain group has no conformer
      (should-not (clime-option-locked beta-opt)))))

(ert-deftest clime-test-alias-for/vals-mutex-sibling-unreachable-cli ()
  "Locked mutex siblings are unreachable from CLI parsing."
  (eval '(clime-app clime-test--af-mutex-sibling-cli
           :version "1"
           (clime-command query
             :help "Query"
             (clime-mutex qm
               (clime-opt todo ("--todo") :help "Keyword")
               (clime-opt sexp ("--sexp") :help "Sexp"))
             (clime-handler (ctx) nil))
           (clime-alias-for waiting (query)
             :vals '((todo . "WAITING"))))
        t)
  ;; --sexp is locked as sibling, so passing it from CLI is an error
  (should-error (clime-parse clime-test--af-mutex-sibling-cli
                             '("waiting" "--sexp" "(active)"))
                :type 'clime-usage-error))

(provide 'clime-alias-for-tests)
;;; clime-alias-for-tests.el ends here
