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

(ert-deftest clime-test-set-parent-refs/runs-collision-check ()
  "clime--set-parent-refs signals on ancestor flag collision."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                              :options (list root-opt)
                              :children (list (cons "show" cmd)))))
    (should-error (clime--set-parent-refs app) :type 'error)))

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

(provide 'clime-core-tests)
;;; clime-core-tests.el ends here
