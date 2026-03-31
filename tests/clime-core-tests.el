;;; clime-core-tests.el --- Tests for clime-core  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for command tree data model: structs, constructors, predicates, lookups.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-test-helpers)

;;; ─── Option Struct ──────────────────────────────────────────────────────

(ert-deftest clime-test-make-option/basic ()
  "Create an option with flags and defaults."
  (let ((opt (clime-make-option :name 'verbose
                                :flags '("--verbose" "-v")
                                :help "Increase verbosity")))
    (should (clime-option-p opt))
    (should (eq (clime-option-name opt) 'verbose))
    (should (equal (clime-option-flags opt) '("--verbose" "-v")))
    (should (equal (clime-option-help opt) "Increase verbosity"))
    (should (eq (clime-option-type opt) 'string))
    (should-not (clime-option-required opt))
    (should-not (clime-option-count opt))
    (should-not (clime-option-multiple opt))))

(ert-deftest clime-test-make-option/all-slots ()
  "Create an option with every slot specified."
  (let ((opt (clime-make-option :name 'priority
                                :flags '("--priority" "-p")
                                :type 'string
                                :help "Set priority"
                                :required t
                                :default "B"
                                :nargs 1
                                :env "APP_PRIORITY"
                                :count nil
                                :multiple nil
                                :category "Workflow"
                                :hidden nil)))
    (should (clime-option-p opt))
    (should (eq (clime-option-name opt) 'priority))
    (should (equal (clime-option-env opt) "APP_PRIORITY"))
    (should (equal (clime-option-category opt) "Workflow"))
    (should (= (clime-option-nargs opt) 1))))

(ert-deftest clime-test-make-option/requires-name ()
  "Error when name is missing."
  (should-error (clime-make-option :flags '("--foo"))
                :type 'error))

(ert-deftest clime-test-make-option/requires-flags ()
  "Error when flags list is missing or empty."
  (should-error (clime-make-option :name 'foo)
                :type 'error)
  (should-error (clime-make-option :name 'foo :flags '())
                :type 'error))

;;; ─── Option Boolean Predicate ───────────────────────────────────────────

(ert-deftest clime-test-option-boolean-p/count ()
  "Option with :count t is boolean."
  (let ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                :count t)))
    (should (clime-option-boolean-p opt))))

(ert-deftest clime-test-option-boolean-p/type-boolean ()
  "Option with :type boolean is boolean."
  (let ((opt (clime-make-option :name 'flag :flags '("--flag")
                                :type 'boolean)))
    (should (clime-option-boolean-p opt))))

(ert-deftest clime-test-option-boolean-p/nargs-zero ()
  "Option with :nargs 0 is boolean."
  (let ((opt (clime-make-option :name 'flag :flags '("--flag")
                                :nargs 0)))
    (should (clime-option-boolean-p opt))))

(ert-deftest clime-test-option-boolean-p/value-option ()
  "Option with nargs=1 and type=string is not boolean."
  (let ((opt (clime-make-option :name 'name :flags '("--name")
                                :type 'string)))
    (should-not (clime-option-boolean-p opt))))

;;; ─── Arg Struct ─────────────────────────────────────────────────────────

(ert-deftest clime-test-make-arg/basic ()
  "Create an arg with defaults."
  (let ((arg (clime-make-arg :name 'id)))
    (should (clime-arg-p arg))
    (should (eq (clime-arg-name arg) 'id))
    (should (eq (clime-arg-type arg) 'string))
    (should (eq (clime-arg-required arg) t))
    (should-not (clime-arg-default arg))
    (should-not (clime-arg-nargs arg))))

(ert-deftest clime-test-make-arg/rest ()
  "Create an arg with :nargs :rest."
  (let ((arg (clime-make-arg :name 'ids :nargs :rest :required nil)))
    (should (eq (clime-arg-nargs arg) :rest))
    (should-not (clime-arg-required arg))))

(ert-deftest clime-test-make-arg/with-default-function ()
  "Arg default can be a function."
  (let ((arg (clime-make-arg :name 'id :required nil
                             :default #'ignore)))
    (should (functionp (clime-arg-default arg)))))

(ert-deftest clime-test-make-arg/requires-name ()
  "Error when name is missing."
  (should-error (clime-make-arg)
                :type 'error))

;;; ─── Command Struct ─────────────────────────────────────────────────────

(ert-deftest clime-test-make-command/basic ()
  "Create a command with handler."
  (let ((cmd (clime-make-command :name "show"
                                 :handler #'ignore)))
    (should (clime-command-p cmd))
    (should (equal (clime-command-name cmd) "show"))
    (should (functionp (clime-command-handler cmd)))
    (should-not (clime-command-aliases cmd))
    (should (null (clime-command-options cmd)))
    (should (null (clime-command-args cmd)))
    (should-not (clime-command-parent cmd))))

(ert-deftest clime-test-make-command/with-aliases ()
  "Command with aliases."
  (let ((cmd (clime-make-command :name "show"
                                 :aliases '("get" "view")
                                 :handler #'ignore)))
    (should (equal (clime-command-aliases cmd) '("get" "view")))))

(ert-deftest clime-test-make-command/with-options-and-args ()
  "Command with options and args attached."
  (let* ((opt (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (arg (clime-make-arg :name 'id))
         (cmd (clime-make-command :name "show"
                                  :handler #'ignore
                                  :options (list opt)
                                  :args (list arg))))
    (should (= (length (clime-command-options cmd)) 1))
    (should (= (length (clime-command-args cmd)) 1))))

(ert-deftest clime-test-make-command/requires-name ()
  "Error when name is missing."
  (should-error (clime-make-command :handler #'ignore)
                :type 'error))

(ert-deftest clime-test-make-command/requires-handler ()
  "Error when handler is missing."
  (should-error (clime-make-command :name "show")
                :type 'error))

;;; ─── Group Struct ───────────────────────────────────────────────────────

(ert-deftest clime-test-make-group/basic ()
  "Create a group with no children."
  (let ((grp (clime-make-group :name "dep"
                                :help "Dependency management")))
    (should (clime-group-p grp))
    (should (equal (clime-group-name grp) "dep"))
    (should (null (clime-group-children grp)))))

(ert-deftest clime-test-make-group/requires-name ()
  "Error when name is missing."
  (should-error (clime-make-group)
                :type 'error))

;;; ─── App Struct ─────────────────────────────────────────────────────────

(ert-deftest clime-test-make-app/basic ()
  "Create an app with version and env-prefix."
  (let ((app (clime-make-app :name "myapp"
                              :version "1.0.0"
                              :env-prefix "MYAPP")))
    (should (clime-app-p app))
    (should (equal (clime-app-name app) "myapp"))
    (should (equal (clime-app-version app) "1.0.0"))
    (should (equal (clime-app-env-prefix app) "MYAPP"))
    (should-not (clime-app-json-mode app))))

(ert-deftest clime-test-make-app/is-also-group ()
  "App has group slots (children, options, etc.)."
  (let ((app (clime-make-app :name "myapp" :version "0.1")))
    (should (clime-app-p app))
    ;; app should have children, options, args slots
    (should (null (clime-app-children app)))
    (should (null (clime-app-options app)))
    (should (null (clime-app-args app)))))

(ert-deftest clime-test-make-app/requires-name ()
  "Error when name is missing."
  (should-error (clime-make-app :version "1.0")
                :type 'error))

;;; ─── Node Predicate ─────────────────────────────────────────────────────

(ert-deftest clime-test-node-p/command ()
  "clime-node-p returns t for command."
  (let ((cmd (clime-make-command :name "x" :handler #'ignore)))
    (should (clime-node-p cmd))))

(ert-deftest clime-test-node-p/group ()
  "clime-node-p returns t for group."
  (let ((grp (clime-make-group :name "x")))
    (should (clime-node-p grp))))

(ert-deftest clime-test-node-p/app ()
  "clime-node-p returns t for app."
  (let ((app (clime-make-app :name "x" :version "1")))
    (should (clime-node-p app))))

(ert-deftest clime-test-node-p/other ()
  "clime-node-p returns nil for non-nodes."
  (should-not (clime-node-p "string"))
  (should-not (clime-node-p nil))
  (should-not (clime-node-p 42)))

;;; ─── Node Option Lookup ─────────────────────────────────────────────────

(ert-deftest clime-test-node-find-option/by-long-flag ()
  "Find option by long flag."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt))))
    (should (eq (clime-node-find-option cmd "--verbose") opt))))

(ert-deftest clime-test-node-find-option/by-short-flag ()
  "Find option by short flag."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt))))
    (should (eq (clime-node-find-option cmd "-v") opt))))

(ert-deftest clime-test-node-find-option/on-group ()
  "Find option on a group node."
  (let* ((opt (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (grp (clime-make-group :name "dep" :options (list opt))))
    (should (eq (clime-node-find-option grp "--json") opt))))

(ert-deftest clime-test-node-find-option/on-app ()
  "Find option on an app node."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (app (clime-make-app :name "x" :version "1" :options (list opt))))
    (should (eq (clime-node-find-option app "--verbose") opt))))

(ert-deftest clime-test-node-find-option/not-found ()
  "Return nil for unknown flag."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd (clime-make-command :name "x" :handler #'ignore
                                  :options (list opt))))
    (should-not (clime-node-find-option cmd "--unknown"))))

;;; ─── Group Child Lookup ─────────────────────────────────────────────────

(ert-deftest clime-test-group-find-child/by-name ()
  "Find child command by name."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd)))))
    (should (eq (clime-group-find-child grp "add") cmd))))

(ert-deftest clime-test-group-find-child/by-alias ()
  "Find child command by alias."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                  :aliases '("get")))
         (grp (clime-make-group :name "x"
                                :children (list (cons "show" cmd)))))
    (should (eq (clime-group-find-child grp "get") cmd))))

(ert-deftest clime-test-group-find-child/nested-group ()
  "Find a group child (not just command child)."
  (let* ((inner (clime-make-group :name "profile" :aliases '("prof")))
         (outer (clime-make-group :name "config"
                                  :children (list (cons "profile" inner)))))
    (should (eq (clime-group-find-child outer "profile") inner))
    (should (eq (clime-group-find-child outer "prof") inner))))

(ert-deftest clime-test-group-find-child/not-found ()
  "Return nil for unknown child."
  (let ((grp (clime-make-group :name "x")))
    (should-not (clime-group-find-child grp "nope"))))

;;; ─── Parent Setting at Construction ──────────────────────────────────────

(ert-deftest clime-test-parent/group-sets-children-parent ()
  "clime-make-group sets :parent on each child."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore))
         (grp (clime-make-group :name "repo"
                                :children (list (cons "add" cmd)))))
    (should (eq (clime-node-parent cmd) grp))))

(ert-deftest clime-test-parent/app-sets-children-parent ()
  "clime-make-app sets :parent on each child."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should (eq (clime-node-parent cmd) app))))

(ert-deftest clime-test-parent/nested-groups ()
  "Nested groups set parents at each level."
  (let* ((cmd (clime-make-command :name "get" :handler #'ignore))
         (inner (clime-make-group :name "config"
                                  :children (list (cons "get" cmd))))
         (outer (clime-make-app :name "t" :version "1"
                                :children (list (cons "config" inner)))))
    (should (eq (clime-node-parent cmd) inner))
    (should (eq (clime-node-parent inner) outer))))

(ert-deftest clime-test-parent/inline-group-sets-parent ()
  "Inline group children also get parent set."
  (let* ((cmd (clime-make-command :name "search" :handler #'ignore))
         (grp (clime-make-group :name "filter" :inline t
                                :children (list (cons "search" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "filter" grp)))))
    (should (eq (clime-node-parent cmd) grp))
    (should (eq (clime-node-parent grp) app))))

;;; ─── Group Child Path Lookup ─────────────────────────────────────────────

(ert-deftest clime-test-group-find-child-path/direct ()
  "Direct child returns single-element path."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd)))))
    (should (equal (clime-group-find-child-path grp "add") (list cmd)))))

(ert-deftest clime-test-group-find-child-path/alias ()
  "Alias match returns single-element path."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                   :aliases '("get")))
         (grp (clime-make-group :name "x"
                                :children (list (cons "show" cmd)))))
    (should (equal (clime-group-find-child-path grp "get") (list cmd)))))

(ert-deftest clime-test-group-find-child-path/inline-group ()
  "Inline group descent returns full path including the inline group."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore))
         (inner (clime-make-group :name "repo" :inline t
                                  :children (list (cons "add" cmd))))
         (outer (clime-make-group :name "app"
                                  :children (list (cons "repo" inner)))))
    (should (equal (clime-group-find-child-path outer "add")
                   (list inner cmd)))))

(ert-deftest clime-test-group-find-child-path/nested-inline ()
  "Nested inline groups return full descent path."
  (let* ((cmd (clime-make-command :name "get" :handler #'ignore))
         (config (clime-make-group :name "config" :inline t
                                   :children (list (cons "get" cmd))))
         (admin (clime-make-group :name "admin" :inline t
                                  :children (list (cons "config" config))))
         (app (clime-make-group :name "app"
                                :children (list (cons "admin" admin)))))
    (should (equal (clime-group-find-child-path app "get")
                   (list admin config cmd)))))

(ert-deftest clime-test-group-find-child-path/not-found ()
  "Return nil for unknown child."
  (let ((grp (clime-make-group :name "x")))
    (should-not (clime-group-find-child-path grp "nope"))))

;;; ─── Node Collect ──────────────────────────────────────────────────────

(ert-deftest clime-test-collect/basic-options-and-commands ()
  "Collect returns options and commands from a simple group."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")
                                  :count t))
         (cmd (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "show" cmd))))
         (items (clime-node-collect app)))
    ;; Option with owner-node
    (should (= 2 (length items)))
    (should (eq :option (caar items)))
    (should (eq opt (cadar items)))
    (should (eq app (caddar items)))
    ;; Command
    (should (eq :command (caadr items)))
    (should (eq cmd (cadadr items)))))

(ert-deftest clime-test-collect/inline-group-recurse ()
  "Collect descends into inline groups by default."
  (let* ((cmd (clime-make-command :name "search" :handler #'ignore))
         (grp (clime-make-group :name "filter" :inline t
                                :children (list (cons "search" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "filter" grp))))
         (items (clime-node-collect app)))
    ;; Should emit :group then :command (not the group as a :command)
    (should (cl-some (lambda (i) (and (eq (car i) :group) (eq (cadr i) grp)))
                     items))
    (should (cl-some (lambda (i) (and (eq (car i) :command) (eq (cadr i) cmd)))
                     items))))

(ert-deftest clime-test-collect/non-inline-group-not-recursed ()
  "Collect does not descend into non-inline groups."
  (let* ((cmd (clime-make-command :name "get" :handler #'ignore))
         (sub (clime-make-group :name "config"
                                :children (list (cons "get" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "config" sub))))
         (items (clime-node-collect app)))
    ;; sub emitted as :command (leaf from app's perspective)
    (should (= 1 (length items)))
    (should (eq :command (caar items)))
    (should (eq sub (cadar items)))))

(ert-deftest clime-test-collect/hidden-option-excluded ()
  "Hidden options are excluded from collect."
  (let* ((opt (clime-make-option :name 'debug :flags '("--debug")
                                  :nargs 0 :hidden t))
         (cmd (clime-make-command :name "run" :handler #'ignore))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "run" cmd))))
         (items (clime-node-collect app)))
    (should (= 1 (length items)))
    (should (eq :command (caar items)))))

(ert-deftest clime-test-collect/hidden-child-excluded ()
  "Hidden children are excluded from collect."
  (let* ((cmd (clime-make-command :name "internal" :handler #'ignore
                                   :hidden t))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "internal" cmd))))
         (items (clime-node-collect app)))
    (should (null items))))

(ert-deftest clime-test-collect/inline-group-options ()
  "Options on inline groups are collected with correct owner-node."
  (let* ((opt (clime-make-option :name 'token :flags '("--token")
                                  :help "A token"))
         (grp (clime-make-group :name "admin" :inline t
                                :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin" grp))))
         (items (clime-node-collect app)))
    (should (cl-some (lambda (i) (and (eq (car i) :option)
                                      (eq (cadr i) opt)
                                      (eq (caddr i) grp)))
                     items))))

(ert-deftest clime-test-collect/match-p-filters ()
  "Collect with :match-p filters items."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")
                                  :count t))
         (cmd (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "t" :version "1"
                              :options (list opt)
                              :children (list (cons "show" cmd))))
         (items (clime-node-collect app
                  :match-p (lambda (type _item) (eq type :command)))))
    (should (= 1 (length items)))
    (should (eq :command (caar items)))))

(ert-deftest clime-test-collect/max-depth-limits-recursion ()
  "Collect with :max-depth stops at specified depth."
  (let* ((cmd (clime-make-command :name "get" :handler #'ignore))
         (inner (clime-make-group :name "config" :inline t
                                   :children (list (cons "get" cmd))))
         (outer (clime-make-group :name "admin" :inline t
                                   :children (list (cons "config" inner))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "admin" outer))))
         (items (clime-node-collect app :max-depth 1)))
    ;; Depth 0 = app level, recurses into outer (depth 1), but NOT inner
    ;; So inner appears as :command, not recursed
    (should (cl-some (lambda (i) (and (eq (car i) :group) (eq (cadr i) outer)))
                     items))
    (should (cl-some (lambda (i) (and (eq (car i) :command) (eq (cadr i) inner)))
                     items))
    ;; cmd should NOT appear (it's inside inner which wasn't recursed)
    (should-not (cl-some (lambda (i) (eq (cadr i) cmd)) items))))

;;; ─── Ancestor Flag Collection ───────────────────────────────────────────

(ert-deftest clime-test-node-all-ancestor-flags/no-parent ()
  "Node with no parent returns empty."
  (let ((cmd (clime-make-command :name "x" :handler #'ignore)))
    (should (null (clime-node-all-ancestor-flags cmd)))))

(ert-deftest clime-test-node-all-ancestor-flags/with-parent ()
  "Collects flags from parent chain."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
         (app (clime-make-app :name "myapp" :version "1"
                               :options (list root-opt)))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :parent app)))
    (should (equal (sort (copy-sequence (clime-node-all-ancestor-flags cmd)) #'string<)
                   '("--verbose" "-v")))))

(ert-deftest clime-test-node-all-ancestor-flags/multi-level ()
  "Collects flags from grandparent chain."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (grp-opt (clime-make-option :name 'json :flags '("--json")))
         (app (clime-make-app :name "myapp" :version "1"
                               :options (list root-opt)))
         (grp (clime-make-group :name "dep" :parent app
                                :options (list grp-opt)))
         (cmd (clime-make-command :name "add" :handler #'ignore
                                  :parent grp)))
    (should (equal (sort (copy-sequence (clime-node-all-ancestor-flags cmd)) #'string<)
                   '("--json" "--verbose")))))

;;; ─── Branch Predicate ───────────────────────────────────────────────────

(ert-deftest clime-test-branch-p/command-is-not-branch ()
  "Commands are leaves, not branches."
  (let ((cmd (clime-make-command :name "show" :handler #'ignore)))
    (should-not (clime-branch-p cmd))))

(ert-deftest clime-test-branch-p/group-is-branch ()
  "Groups are branches."
  (let ((grp (clime-make-group :name "dep")))
    (should (clime-branch-p grp))))

(ert-deftest clime-test-branch-p/app-is-branch ()
  "Apps are branches (extend group)."
  (let ((app (clime-make-app :name "t" :version "1")))
    (should (clime-branch-p app))))

;;; ─── Ancestors ─────────────────────────────────────────────────────────

(ert-deftest clime-test-node-ancestors/no-parent ()
  "Node with no parent has empty ancestor list."
  (let ((cmd (clime-make-command :name "x" :handler #'ignore)))
    (should (null (clime-node-ancestors cmd)))))

(ert-deftest clime-test-node-ancestors/single-parent ()
  "Returns list with just the parent."
  (let* ((app (clime-make-app :name "t" :version "1"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :parent app)))
    (should (equal (clime-node-ancestors cmd) (list app)))))

(ert-deftest clime-test-node-ancestors/multi-level ()
  "Returns parent chain from immediate parent to root."
  (let* ((app (clime-make-app :name "t" :version "1"))
         (grp (clime-make-group :name "dep" :parent app))
         (cmd (clime-make-command :name "add" :handler #'ignore
                                  :parent grp)))
    (should (equal (clime-node-ancestors cmd) (list grp app)))))

;;; ─── Set Parent Refs (moved to core) ───────────────────────────────────

(ert-deftest clime-test-set-parent-refs/sets-recursive ()
  "clime--set-parent-refs recursively sets parent on all descendants."
  (let* ((cmd (clime-make-command :name "add" :handler #'ignore))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "dep" grp)))))
    (clime--set-parent-refs app)
    (should (eq (clime-node-parent grp) app))
    (should (eq (clime-node-parent cmd) grp))))

(ert-deftest clime-test-make-app/catches-ancestor-collision ()
  "clime-make-app signals on ancestor flag collision at construction time."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt))))
    (should-error (clime-make-app :name "t" :version "1"
                                  :options (list root-opt)
                                  :children (list (cons "show" cmd)))
                  :type 'error)))

;;; ─── Params Plist ──────────────────────────────────────────────────────

(ert-deftest clime-test-params-plist/all-params ()
  "Convert all context params to keyword plist."
  (let* ((ctx (clime-context--create
               :params '(verbose 3 package "foo" force t))))
    (should (equal (clime-params-plist ctx)
                   '(:verbose 3 :package "foo" :force t)))))

(ert-deftest clime-test-params-plist/filtered ()
  "Convert only named params to keyword plist, omitting nil."
  (let* ((ctx (clime-context--create
               :params '(verbose 3 package "foo" force nil quiet nil))))
    (should (equal (clime-params-plist ctx 'package 'verbose)
                   '(:package "foo" :verbose 3)))))

(ert-deftest clime-test-params-plist/filtered-omits-nil ()
  "Filtered form omits params with nil values."
  (let* ((ctx (clime-context--create
               :params '(verbose nil force nil package "foo"))))
    (should (equal (clime-params-plist ctx 'verbose 'force 'package)
                   '(:package "foo")))))

(ert-deftest clime-test-params-plist/empty-params ()
  "Empty params returns nil."
  (let* ((ctx (clime-context--create :params nil)))
    (should (null (clime-params-plist ctx)))
    (should (null (clime-params-plist ctx 'foo)))))

(ert-deftest clime-test-params-plist/count-and-multiple ()
  "Works with count values and multiple (list) values."
  (let* ((ctx (clime-context--create
               :params '(verbose 3 tag ("dev" "test")))))
    (should (equal (clime-params-plist ctx)
                   '(:verbose 3 :tag ("dev" "test"))))))

;;; ─── Package Version ────────────────────────────────────────────────────

(ert-deftest clime-test-version/constant-exists ()
  "clime-version is a non-empty string."
  (require 'clime)
  (should (stringp clime-version))
  (should (> (length clime-version) 0)))

(ert-deftest clime-test-version/matches-semver ()
  "clime-version looks like a semver string."
  (require 'clime)
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]" clime-version)))

;;; ─── Merge Template ────────────────────────────────────────────────────

(ert-deftest clime-test-merge-template/overrides-win ()
  "Explicit overrides take precedence over template values."
  (let ((tpl '(:type string :help "Template help" :required t)))
    (should (equal (clime--merge-template tpl :help "Override" :required nil)
                   '(:type string :help "Override" :required nil)))))

(ert-deftest clime-test-merge-template/template-preserved ()
  "Template values carry through when not overridden."
  (let ((tpl '(:type integer :conform identity)))
    (let ((result (clime--merge-template tpl :name 'id :flags '("--id"))))
      (should (eq (plist-get result :type) 'integer))
      (should (eq (plist-get result :conform) 'identity))
      (should (eq (plist-get result :name) 'id)))))

(ert-deftest clime-test-merge-template/empty-template ()
  "Empty template just returns overrides."
  (should (equal (clime--merge-template '() :name 'x :flags '("--x"))
                 '(:name x :flags ("--x")))))

(ert-deftest clime-test-merge-template/no-mutation ()
  "Merge does not mutate the original template."
  (let ((tpl (list :type 'string :help "orig")))
    (clime--merge-template tpl :help "changed")
    (should (equal (plist-get tpl :help) "orig"))))

;;; ─── Tree Validation ────────────────────────────────────────────────────

(ert-deftest clime-test-validate/duplicate-flag-error ()
  "Error when two options on the same node share a flag."
  (let ((opt-a (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
        (opt-b (clime-make-option :name 'debug :flags '("--verbose")))
        (cmd (clime-make-command :name "run" :handler #'ignore)))
    (should-error (clime-make-app :name "t"
                                  :options (list opt-a opt-b)
                                  :children (list (cons "run" cmd)))
                  :type 'error)))

(ert-deftest clime-test-validate/duplicate-option-name-error ()
  "Error when two options on the same node share a name."
  (let ((opt-a (clime-make-option :name 'out :flags '("--output")))
        (opt-b (clime-make-option :name 'out :flags '("--out")))
        (cmd (clime-make-command :name "run" :handler #'ignore)))
    (should-error (clime-make-app :name "t"
                                  :options (list opt-a opt-b)
                                  :children (list (cons "run" cmd)))
                  :type 'error)))

(ert-deftest clime-test-validate/duplicate-child-name-error ()
  "Error when a group has two children with the same name."
  (let ((cmd-a (clime-make-command :name "run" :handler #'ignore))
        (cmd-b (clime-make-command :name "run" :handler #'ignore)))
    (should-error (clime-make-app :name "t"
                                  :children (list (cons "run" cmd-a)
                                                  (cons "run" cmd-b)))
                  :type 'error)))

(ert-deftest clime-test-validate/nested-group-duplicate-flag ()
  "Validation catches duplicates in nested groups."
  (let* ((opt-a (clime-make-option :name 'x :flags '("--xx")))
         (opt-b (clime-make-option :name 'y :flags '("--xx")))
         (cmd (clime-make-command :name "run" :handler #'ignore))
         (grp (clime-make-group :name "sub"
                                :options (list opt-a opt-b)
                                :children (list (cons "run" cmd)))))
    (should-error (clime-make-app :name "t"
                                  :children (list (cons "sub" grp)))
                  :type 'error)))

(ert-deftest clime-test-validate/clean-tree-no-error ()
  "No error on a well-formed tree with no structural issues."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd-opt (clime-make-option :name 'json :flags '("--json")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt))))
    ;; Should not signal
    (clime-make-app :name "t"
                    :options (list root-opt)
                    :children (list (cons "show" cmd)))))

;;; ─── Required + Default Warning ─────────────────────────────────────

(ert-deftest clime-test-validate/required-default-option-warns ()
  "Warning when option has both :required t and :default."
  (let ((msgs (clime-test-with-messages
                (clime-make-option :name 'fmt :flags '("--fmt")
                                    :required t :default "json"))))
    (should (cl-some (lambda (m) (string-match-p ":required is vacuous" m))
                     msgs))))

(ert-deftest clime-test-validate/required-no-default-option-no-warn ()
  "No warning when option has :required t without :default."
  (let ((msgs (clime-test-with-messages
                (clime-make-option :name 'fmt :flags '("--fmt")
                                    :required t))))
    (should-not (cl-some (lambda (m) (string-match-p ":required is vacuous" m))
                         msgs))))

(ert-deftest clime-test-validate/required-default-arg-warns ()
  "Warning when arg has both :required t and :default."
  (let ((msgs (clime-test-with-messages
                (clime-make-arg :name 'file :required t :default "out.txt"))))
    (should (cl-some (lambda (m) (string-match-p ":required is vacuous" m))
                     msgs))))

(ert-deftest clime-test-validate/required-no-default-arg-no-warn ()
  "No warning when arg has :required t without :default."
  (let ((msgs (clime-test-with-messages
                (clime-make-arg :name 'file :required t))))
    (should-not (cl-some (lambda (m) (string-match-p ":required is vacuous" m))
                         msgs))))

;;; ─── Deep Copy Tests ───────────────────────────────────────────────

(ert-deftest clime-test-deep-copy/option-mutation-isolated ()
  "Mutating a copied option does not affect the original."
  (let* ((app (clime-make-app
               :name "test" :version "1"
               :options (list (clime-make-option :name 'verbose :flags '("--verbose")))))
         (copy (clime--deep-copy-tree app)))
    (setf (clime-param-help (car (clime-node-options copy))) "changed")
    (should-not (clime-param-help (car (clime-node-options app))))))

(ert-deftest clime-test-deep-copy/arg-mutation-isolated ()
  "Mutating a copied arg does not affect the original."
  (let* ((cmd (clime-make-command
               :name "run" :handler #'ignore
               :args (list (clime-make-arg :name 'file))))
         (app (clime-make-app
               :name "test" :version "1"
               :children (list (cons "run" cmd))))
         (copy (clime--deep-copy-tree app))
         (copy-cmd (cdr (car (clime-group-children copy)))))
    (setf (clime-param-help (car (clime-node-args copy-cmd))) "mutated")
    (should-not (clime-param-help (car (clime-node-args cmd))))))

(ert-deftest clime-test-deep-copy/parent-refs-correct ()
  "Copied children point to copied parent, not original."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (app (clime-make-app
               :name "test" :version "1"
               :children (list (cons "run" cmd))))
         (_ (clime--set-parent-refs-1 app))
         (copy (clime--deep-copy-tree app))
         (copy-cmd (cdr (car (clime-group-children copy)))))
    (should (eq copy (clime-node-parent copy-cmd)))
    (should-not (eq app (clime-node-parent copy-cmd)))))

(ert-deftest clime-test-deep-copy/original-tree-unchanged ()
  "Deep copy does not modify any part of the original tree."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (arg (clime-make-arg :name 'file))
         (cmd (clime-make-command
               :name "run" :handler #'ignore
               :args (list arg)))
         (app (clime-make-app
               :name "test" :version "1"
               :options (list opt)
               :children (list (cons "run" cmd))))
         (copy (clime--deep-copy-tree app)))
    ;; Mutate everything on the copy
    (setf (clime-param-help (car (clime-node-options copy))) "x")
    (setf (clime-node-name copy) "mutated")
    (let ((copy-cmd (cdr (car (clime-group-children copy)))))
      (setf (clime-param-help (car (clime-node-args copy-cmd))) "y"))
    ;; Original is untouched
    (should (equal "test" (clime-node-name app)))
    (should-not (clime-param-help opt))
    (should-not (clime-param-help arg))))

(ert-deftest clime-test-deep-copy/handlers-shared ()
  "Handler functions are shared (not copied) between original and copy."
  (let* ((handler #'ignore)
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app
               :name "test" :version "1"
               :children (list (cons "run" cmd))))
         (copy (clime--deep-copy-tree app))
         (copy-cmd (cdr (car (clime-group-children copy)))))
    (should (eq handler (clime-node-handler copy-cmd)))))

(ert-deftest clime-test-deep-copy/inline-group-options-isolated ()
  "Inline group options are deep-copied and isolated from original."
  (let* ((opt-a (clime-make-option :name 'mode-a :flags '("--mode-a") :nargs 0))
         (opt-b (clime-make-option :name 'mode-b :flags '("--mode-b") :nargs 0))
         (grp (clime-group--create :name "modes" :inline t
                                    :options (list opt-a opt-b)))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :children (list (cons "modes" grp))))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "run" cmd))))
         (_ (clime--set-parent-refs app))
         (copy (clime--deep-copy-tree app))
         (copy-cmd (cdr (car (clime-group-children copy))))
         (copy-grp (cdr (car (clime-group-children copy-cmd))))
         (copy-opt (car (clime-node-options copy-grp))))
    ;; Mutating copy option doesn't affect original
    (setf (clime-param-help copy-opt) "changed")
    (should-not (clime-param-help opt-a))
    ;; Copy inline group parent points to copy cmd
    (should (eq copy-cmd (clime-node-parent copy-grp)))))

;;; ─── Prepare Tree Tests ───────────────────────────────────────────────

(ert-deftest clime-test-prepare-tree/sets-parent-refs ()
  "Prepare-tree sets parent refs on child nodes."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "run" cmd))))
         (copy (clime--prepare-tree app))
         (copy-cmd (cdr (car (clime-group-children copy)))))
    (should (eq copy (clime-node-parent copy-cmd)))))

(ert-deftest clime-test-prepare-tree/resolves-aliases ()
  "Prepare-tree resolves alias nodes into full commands."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "list" :handler #'ignore
                                   :options (list opt)))
         (alias (clime-alias--create :name "csv" :target '("list")
                                      :vals '((format . "csv"))))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "list" cmd)
                                              (cons "csv" alias))))
         (copy (clime--prepare-tree app))
         (csv-node (cdr (assoc "csv" (clime-group-children copy)))))
    ;; Alias resolved: csv is now a command with handler, not an alias
    (should (clime-command-p csv-node))
    (should (clime-node-handler csv-node))))

(ert-deftest clime-test-prepare-tree/deep-copy-isolated ()
  "Prepare-tree returns an isolated copy; mutations don't affect original."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list (clime-make-arg :name 'file))))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "run" cmd))))
         (copy (clime--prepare-tree app))
         (copy-cmd (cdr (car (clime-group-children copy)))))
    (setf (clime-param-help (car (clime-node-args copy-cmd))) "mutated")
    (should-not (clime-param-help (car (clime-node-args cmd))))))

(ert-deftest clime-test-prepare-tree/idempotent-on-original ()
  "Calling prepare-tree twice on same app produces equivalent results."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (alias (clime-alias--create :name "go" :target '("run")))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "run" cmd)
                                              (cons "go" alias))))
         (copy1 (clime--prepare-tree app))
         (copy2 (clime--prepare-tree app))
         (go1 (cdr (assoc "go" (clime-group-children copy1))))
         (go2 (cdr (assoc "go" (clime-group-children copy2)))))
    ;; Both copies resolved the alias
    (should (clime-command-p go1))
    (should (clime-command-p go2))
    ;; But they are distinct structs
    (should-not (eq go1 go2))))

(provide 'clime-core-tests)
;;; clime-core-tests.el ends here
