;;; clime-dsl-tests.el --- Tests for clime-dsl  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the declarative DSL macros.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-dsl)
(require 'clime-test-helpers)

;;; ─── Simple App ─────────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/simple-app ()
  "clime-app creates a clime-app struct bound to a variable."
  (eval '(clime-app clime-test--dsl-simple
           :version "1.0"
           :help "A test app"
           (clime-command ping
             :help "Ping command"
             (clime-handler (ctx) "pong")))
        t)
  (should (clime-app-p clime-test--dsl-simple))
  (should (equal (clime-app-name clime-test--dsl-simple) "clime-test--dsl-simple"))
  (should (equal (clime-app-version clime-test--dsl-simple) "1.0")))

;;; ─── Command With Options and Args ──────────────────────────────────────

(ert-deftest clime-test-dsl/command-options-args ()
  "Command with options and args creates correct structs."
  (eval '(clime-app clime-test--dsl-opts
           :version "1"
           (clime-command show
             :help "Show a thing"
             :aliases ("get")
             (clime-option format ("-f" "--format") :default "text" :help "Output format")
             (clime-arg id :help "Resource ID")
             (clime-handler (ctx) nil)))
        t)
  (let* ((app clime-test--dsl-opts)
         (show (cdr (assoc "show" (clime-group-children app)))))
    (should (clime-command-p show))
    (should (equal (clime-command-name show) "show"))
    (should (equal (clime-command-aliases show) '("get")))
    (should (= (length (clime-command-options show)) 1))
    (should (= (length (clime-command-args show)) 1))
    (let ((opt (car (clime-command-options show))))
      (should (eq (clime-option-name opt) 'format))
      (should (equal (clime-option-flags opt) '("-f" "--format")))
      (should (equal (clime-option-default opt) "text")))
    (let ((arg (car (clime-command-args show))))
      (should (eq (clime-arg-name arg) 'id)))))

;;; ─── Root Options ───────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/root-options ()
  "Options at app level become root options."
  (eval '(clime-app clime-test--dsl-root-opts
           :version "1"
           (clime-option verbose ("-v" "--verbose") :count t :help "Verbosity")
           (clime-command ping
             :help "Ping"
             (clime-handler (ctx) nil)))
        t)
  (let ((app clime-test--dsl-root-opts))
    (should (= (length (clime-group-options app)) 1))
    (let ((opt (car (clime-group-options app))))
      (should (eq (clime-option-name opt) 'verbose))
      (should (clime-option-count opt)))))

;;; ─── Group With Subcommands ─────────────────────────────────────────────

(ert-deftest clime-test-dsl/group ()
  "clime-group creates a group with child commands."
  (eval '(clime-app clime-test--dsl-grp
           :version "1"
           (clime-group dep
             :help "Dependencies"
             (clime-command add
               :help "Add dep"
               (clime-arg id :help "Dep ID")
               (clime-handler (ctx) nil))
             (clime-command remove
               :aliases ("rm")
               :help "Remove dep"
               (clime-arg id :help "Dep ID")
               (clime-handler (ctx) nil))))
        t)
  (let* ((app clime-test--dsl-grp)
         (dep (cdr (assoc "dep" (clime-group-children app)))))
    (should (clime-group-p dep))
    (should (equal (clime-group-name dep) "dep"))
    (should (= (length (clime-group-children dep)) 2))
    (should (cdr (assoc "add" (clime-group-children dep))))
    (should (cdr (assoc "remove" (clime-group-children dep))))))

;;; ─── Inline Groups ─────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/inline-group ()
  "clime-group with :inline t sets the inline slot."
  (eval '(clime-app clime-test--dsl-inline
           :version "1"
           (clime-group repo
             :inline t
             :help "Manage repos"
             (clime-command add
               :help "Add repo"
               (clime-handler (ctx) nil))))
        t)
  (let* ((app clime-test--dsl-inline)
         (grp (cdr (assoc "repo" (clime-group-children app)))))
    (should (clime-group-p grp))
    (should (clime-node-inline grp))))

;;; ─── Nested Groups ─────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/nested-groups ()
  "Groups can be nested inside groups."
  (eval '(clime-app clime-test--dsl-nested
           :version "1"
           (clime-group config
             :help "Configuration"
             (clime-group profile
               :help "Profile management"
               (clime-command list
                 :help "List profiles"
                 (clime-handler (ctx) nil)))))
        t)
  (let* ((app clime-test--dsl-nested)
         (config (cdr (assoc "config" (clime-group-children app))))
         (profile (cdr (assoc "profile" (clime-group-children config)))))
    (should (clime-group-p config))
    (should (clime-group-p profile))
    (should (cdr (assoc "list" (clime-group-children profile))))))

;;; ─── Handler ────────────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/handler-is-function ()
  "clime-handler produces a callable function on the command."
  (eval '(clime-app clime-test--dsl-handler
           :version "1"
           (clime-command echo
             :help "Echo"
             (clime-arg msg :help "Message")
             (clime-handler (ctx) (concat "echo: " (clime-ctx-get ctx 'msg)))))
        t)
  (let* ((app clime-test--dsl-handler)
         (cmd (cdr (assoc "echo" (clime-group-children app)))))
    (should (functionp (clime-command-handler cmd)))
    (let ((ctx (clime-context--create :params '(msg "hi"))))
      (should (equal (funcall (clime-command-handler cmd) ctx)
                     "echo: hi")))))

;;; ─── Boolean / Count / Multiple Options ─────────────────────────────────

(ert-deftest clime-test-dsl/option-types ()
  "Options with :count, :nargs 0, :multiple create correct specs."
  (eval '(clime-app clime-test--dsl-opt-types
           :version "1"
           (clime-command test
             :help "Test"
             (clime-option verbose ("-v") :count t)
             (clime-option json ("--json") :nargs 0)
             (clime-option tag ("--tag" "-t") :multiple t)
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd (cdr (assoc "test" (clime-group-children clime-test--dsl-opt-types))))
         (opts (clime-command-options cmd)))
    (should (= (length opts) 3))
    (let ((v (cl-find-if (lambda (o) (eq (clime-option-name o) 'verbose)) opts))
          (j (cl-find-if (lambda (o) (eq (clime-option-name o) 'json)) opts))
          (tg (cl-find-if (lambda (o) (eq (clime-option-name o) 'tag)) opts)))
      (should (clime-option-boolean-p v))
      (should (clime-option-count v))
      (should (clime-option-boolean-p j))
      (should (clime-option-multiple tg)))))

;;; ─── Arg With Rest ──────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/arg-rest ()
  "Arg with :nargs :rest works in DSL."
  (eval '(clime-app clime-test--dsl-rest
           :version "1"
           (clime-command show
             :help "Show"
             (clime-arg ids :nargs :rest :required nil :help "IDs")
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd (cdr (assoc "show" (clime-group-children clime-test--dsl-rest))))
         (arg (car (clime-command-args cmd))))
    (should (eq (clime-arg-nargs arg) :rest))
    (should-not (clime-arg-required arg))))

;;; ─── Integration: DSL + Parser ──────────────────────────────────────────

(ert-deftest clime-test-dsl/parse-integration ()
  "App built with DSL macros works with clime-parse."
  (eval '(clime-app clime-test--dsl-integration
           :version "2.0"
           (clime-option verbose ("-v" "--verbose") :count t)
           (clime-command show
             :aliases ("get")
             :help "Show resource"
             (clime-option format ("-f" "--format") :default "text")
             (clime-arg id :help "ID")
             (clime-handler (ctx) nil)))
        t)
  (let ((result (clime-parse clime-test--dsl-integration
                             '("-v" "show" "--format" "json" "abc"))))
    (should (clime-parse-result-p result))
    (should (equal (clime-command-name (clime-parse-result-command result)) "show"))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "json"))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "abc"))))

(ert-deftest clime-test-dsl/parse-integration-alias ()
  "Command alias works through DSL + parser."
  (eval '(clime-app clime-test--dsl-alias-int
           :version "1"
           (clime-command show
             :aliases ("get")
             :help "Show"
             (clime-arg id)
             (clime-handler (ctx) nil)))
        t)
  (let ((result (clime-parse clime-test--dsl-alias-int '("get" "123"))))
    (should (equal (clime-command-name (clime-parse-result-command result)) "show"))))

(ert-deftest clime-test-dsl/parse-integration-group ()
  "Group app built with DSL works with parser."
  (eval '(clime-app clime-test--dsl-grp-int
           :version "1"
           (clime-option verbose ("-v") :count t)
           (clime-group dep
             :help "Deps"
             (clime-command add
               :help "Add"
               (clime-arg id)
               (clime-handler (ctx) nil))))
        t)
  (let ((result (clime-parse clime-test--dsl-grp-int '("-v" "dep" "add" "X"))))
    (should (equal (clime-command-name (clime-parse-result-command result)) "add"))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "X"))))

;;; ─── Mixed Order ────────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/mixed-order ()
  "Options, args, and commands can appear in any order in body."
  (eval '(clime-app clime-test--dsl-mixed
           :version "1"
           (clime-command first
             :help "First"
             (clime-arg name)
             (clime-option flag ("--flag") :nargs 0)
             (clime-handler (ctx) nil))
           (clime-option global ("--global") :nargs 0)
           (clime-command second
             :help "Second"
             (clime-handler (ctx) nil)))
        t)
  (let ((app clime-test--dsl-mixed))
    (should (= (length (clime-group-options app)) 1))
    (should (= (length (clime-group-children app)) 2))))

;;; ─── Error Cases ─────────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/missing-handler-error ()
  "Command without clime-handler signals error."
  (should-error
   (eval '(clime-app clime-test--dsl-no-handler
            :version "1"
            (clime-command broken
              :help "No handler"
              (clime-arg id)))
         t)
   :type 'error))

(ert-deftest clime-test-dsl/unknown-form-error ()
  "Unknown DSL form signals error."
  (should-error
   (eval '(clime-app clime-test--dsl-unknown
            :version "1"
            (clime-bogus foo))
         t)
   :type 'error))

;;; ─── Group With Invoke Handler ──────────────────────────────────────────

(ert-deftest clime-test-dsl/group-invoke ()
  "Group with clime-handler sets handler slot."
  (eval '(clime-app clime-test--dsl-grp-invoke
           :version "1"
           (clime-group status
             :help "Show status"
             (clime-handler (ctx) "status overview")
             (clime-command detail
               :help "Detailed status"
               (clime-handler (ctx) nil))))
        t)
  (let ((grp (cdr (assoc "status" (clime-group-children clime-test--dsl-grp-invoke)))))
    (should (functionp (clime-group-handler grp)))))

;;; ─── Group With Options and Args ────────────────────────────────────────

(ert-deftest clime-test-dsl/group-options-args ()
  "Group-level options and args created correctly."
  (eval '(clime-app clime-test--dsl-grp-opts
           :version "1"
           (clime-group config
             :help "Config"
             (clime-option scope ("--scope") :default "local")
             (clime-arg profile :help "Profile name")
             (clime-command list
               :help "List"
               (clime-handler (ctx) nil))))
        t)
  (let ((grp (cdr (assoc "config" (clime-group-children clime-test--dsl-grp-opts)))))
    (should (= (length (clime-group-options grp)) 1))
    (should (= (length (clime-group-args grp)) 1))
    (should (eq (clime-option-name (car (clime-group-options grp))) 'scope))
    (should (eq (clime-arg-name (car (clime-group-args grp))) 'profile))))

;;; ─── App Keyword Args ───────────────────────────────────────────────────

(ert-deftest clime-test-dsl/app-keywords ()
  "App keyword args :env-prefix, :json-mode set correctly."
  (eval '(clime-app clime-test--dsl-keywords
           :version "3.0"
           :env-prefix "MYAPP"
           :json-mode t
           :help "A helpful app"
           (clime-command ping
             :help "Ping"
             (clime-handler (ctx) nil)))
        t)
  (let ((app clime-test--dsl-keywords))
    (should (equal (clime-app-env-prefix app) "MYAPP"))
    (should (clime-app-json-mode app))
    (should (equal (clime-group-help app) "A helpful app"))))

;;; ─── Hidden ─────────────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/hidden-command ()
  "Hidden keyword on command and option."
  (eval '(clime-app clime-test--dsl-hidden
           :version "1"
           (clime-command secret
             :help "Secret cmd"
             :hidden t
             (clime-option debug ("--debug") :nargs 0 :hidden t)
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (assoc "secret" (clime-group-children clime-test--dsl-hidden)))))
    (should (clime-command-hidden cmd))
    (should (clime-option-hidden (car (clime-command-options cmd))))))

;;; ─── Context Struct ─────────────────────────────────────────────────────

(ert-deftest clime-test-context/struct ()
  "clime-context struct works with accessors."
  (let ((ctx (clime-context--create
              :app nil
              :command nil
              :path '("myapp" "show")
              :params '(id "123" format "json"))))
    (should (clime-context-p ctx))
    (should (equal (clime-ctx-get ctx 'id) "123"))
    (should (equal (clime-ctx-get ctx 'format) "json"))
    (should-not (clime-ctx-get ctx 'missing))
    (should (equal (clime-context-path ctx) '("myapp" "show")))))

;;; ─── :flag Shorthand ──────────────────────────────────────────────────

(ert-deftest clime-test-dsl/flag-shorthand ()
  ":flag t in DSL produces a boolean option (nargs=0)."
  (eval '(clime-app clime-test--dsl-flag
           :version "1"
           (clime-command test
             :help "Test"
             (clime-option force ("--force") :flag t)
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd (cdr (assoc "test" (clime-group-children clime-test--dsl-flag))))
         (opt (car (clime-command-options cmd))))
    (should (clime-option-boolean-p opt))
    (should (eql (clime-option-nargs opt) 0))))

(ert-deftest clime-test-dsl/bool-shorthand ()
  ":bool t in DSL produces a boolean option (nargs=0)."
  (eval '(clime-app clime-test--dsl-bool
           :version "1"
           (clime-command test
             :help "Test"
             (clime-option force ("--force") :bool t)
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd (cdr (assoc "test" (clime-group-children clime-test--dsl-bool))))
         (opt (car (clime-command-options cmd))))
    (should (clime-option-boolean-p opt))
    (should (eql (clime-option-nargs opt) 0))))

(ert-deftest clime-test-dsl/defopt-bool-shorthand ()
  "clime-defopt normalizes :bool t to :nargs 0."
  (eval '(clime-defopt test-bool-tmpl
           :bool t
           :help "A boolean template")
        t)
  (should (eql (plist-get clime--opt-test-bool-tmpl :nargs) 0))
  (should-not (plist-member clime--opt-test-bool-tmpl :bool)))

(ert-deftest clime-test-dsl/flag-deprecated-warning ()
  ":flag t emits a deprecation warning."
  (let ((msgs (clime-test-with-messages
                (eval '(clime-app clime-test--dsl-flag-depr
                         :version "1"
                         (clime-command test
                           :help "Test"
                           (clime-option force ("--force") :flag t)
                           (clime-handler (ctx) nil)))
                      t))))
    (should (cl-some (lambda (m) (string-match-p ":bool" m)) msgs))))

;;; ─── Separator Shorthand ────────────────────────────────────────────

(ert-deftest clime-test-dsl/separator-implies-multiple ()
  ":separator in DSL auto-sets :multiple t."
  (eval '(clime-app clime-test--dsl-sep
           :version "1"
           (clime-command test
             :help "Test"
             (clime-option tag ("--tag") :separator ",")
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd (cdr (assoc "test" (clime-group-children clime-test--dsl-sep))))
         (opt (car (clime-command-options cmd))))
    (should (clime-option-multiple opt))
    (should (equal (clime-option-separator opt) ","))))

;;; ─── Symbol Aliases ──────────────────────────────────────────────────

(ert-deftest clime-test-dsl/symbol-aliases ()
  "Symbol aliases in DSL are converted to strings."
  (eval '(clime-app clime-test--dsl-sym-alias
           :version "1"
           (clime-command install
             :help "Install"
             :aliases (i ins)
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (assoc "install" (clime-group-children clime-test--dsl-sym-alias)))))
    (should (equal (clime-command-aliases cmd) '("i" "ins")))))

(ert-deftest clime-test-dsl/mixed-aliases ()
  "Mixed string and symbol aliases both work."
  (eval '(clime-app clime-test--dsl-mix-alias
           :version "1"
           (clime-command list
             :help "List"
             :aliases ("ls" l)
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (assoc "list" (clime-group-children clime-test--dsl-mix-alias)))))
    (should (equal (clime-command-aliases cmd) '("ls" "l")))))

;;; ─── clime-let ──────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/clime-let-simple ()
  "clime-let binds params by name."
  (let ((ctx (clime-context--create
              :params '(id "123" format "json"))))
    (clime-let ctx (id format)
      (should (equal id "123"))
      (should (equal format "json")))))

(ert-deftest clime-test-dsl/clime-let-rename ()
  "clime-let (var param) binds param under a different name."
  (let ((ctx (clime-context--create
              :params '(tag ("a" "b") verbose 2))))
    (clime-let ctx ((tags tag) (v verbose))
      (should (equal tags '("a" "b")))
      (should (= v 2)))))

(ert-deftest clime-test-dsl/clime-let-missing ()
  "clime-let with missing param binds nil."
  (let ((ctx (clime-context--create :params '(id "123"))))
    (clime-let ctx (id missing)
      (should (equal id "123"))
      (should-not missing))))

;;; ─── :doc Alias ─────────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/doc-alias-on-app ()
  ":doc works as alias for :help on app."
  (eval '(clime-app clime-test--dsl-doc-app
           :version "1"
           :doc "App via doc"
           (clime-command noop
             :help "noop"
             (clime-handler (_ctx) nil)))
        t)
  (should (equal (clime-node-help clime-test--dsl-doc-app) "App via doc")))

(ert-deftest clime-test-dsl/doc-alias-on-command ()
  ":doc works as alias for :help on command."
  (eval '(clime-app clime-test--dsl-doc-cmd
           :version "1"
           (clime-command ping
             :doc "Ping via doc"
             (clime-handler (_ctx) "pong")))
        t)
  (let* ((children (clime-group-children clime-test--dsl-doc-cmd))
         (cmd (cdr (assoc "ping" children))))
    (should (equal (clime-node-help cmd) "Ping via doc"))))

(ert-deftest clime-test-dsl/doc-alias-on-group ()
  ":doc works as alias for :help on group."
  (eval '(clime-app clime-test--dsl-doc-grp
           :version "1"
           (clime-group stuff
             :doc "Stuff via doc"
             (clime-command do-it
               :help "Do it"
               (clime-handler (_ctx) nil))))
        t)
  (let* ((children (clime-group-children clime-test--dsl-doc-grp))
         (grp (cdr (assoc "stuff" children))))
    (should (equal (clime-node-help grp) "Stuff via doc"))))

(ert-deftest clime-test-dsl/doc-and-help-errors ()
  "Using both :doc and :help on the same form signals an error."
  (should-error
   (eval '(clime-app clime-test--dsl-doc-both
            :version "1"
            :doc "doc" :help "help"
            (clime-command noop
              :help "noop"
              (clime-handler (_ctx) nil)))
         t)
   :type 'error))

;;; ─── clime-defopt: Option Templates ────────────────────────────────────

(ert-deftest clime-test-dsl/defopt-basic ()
  "clime-defopt creates a clime--opt-NAME variable holding option slot defaults."
  (eval '(clime-defopt test-basic
           :type 'string
           :conform #'identity
           :help "A basic template")
        t)
  (should (listp clime--opt-test-basic))
  (should (eq (plist-get clime--opt-test-basic :type) 'string))
  (should (functionp (plist-get clime--opt-test-basic :conform)))
  (should (equal (plist-get clime--opt-test-basic :help) "A basic template")))

(ert-deftest clime-test-dsl/defopt-flag-shorthand ()
  "clime-defopt normalizes :flag t to :nargs 0."
  (eval '(clime-defopt test-flag
           :flag t
           :help "A boolean template")
        t)
  (should (eql (plist-get clime--opt-test-flag :nargs) 0))
  (should-not (plist-member clime--opt-test-flag :flag)))

(ert-deftest clime-test-dsl/defopt-separator-implies-multiple ()
  "clime-defopt normalizes :separator to imply :multiple t."
  (eval '(clime-defopt test-sep
           :separator ",")
        t)
  (should (eq (plist-get clime--opt-test-sep :multiple) t))
  (should (equal (plist-get clime--opt-test-sep :separator) ",")))

(ert-deftest clime-test-dsl/defopt-rejects-name ()
  "clime-defopt errors if :name is included."
  (should-error
   (eval '(clime-defopt test-bad-name
            :name 'foo
            :help "Bad")
         t)
   :type 'error))

(ert-deftest clime-test-dsl/defopt-rejects-flags ()
  "clime-defopt errors if :flags is included."
  (should-error
   (eval '(clime-defopt test-bad-flags
            :flags '("--foo")
            :help "Bad")
         t)
   :type 'error))

;;; ─── :from in clime-option ────────────────────────────────────────────

(ert-deftest clime-test-dsl/from-basic ()
  ":from inherits template defaults into an option."
  (eval '(progn
           (clime-defopt test-id
             :type 'string
             :conform #'identity
             :help "A verified ID")
           (clime-app clime-test--from-basic
             :version "1"
             (clime-command show
               :help "Show"
               (clime-option id ("--id") :from test-id)
               (clime-handler (ctx) nil))))
        t)
  (let* ((cmd (cdr (assoc "show" (clime-group-children clime-test--from-basic))))
         (opt (car (clime-command-options cmd))))
    (should (eq (clime-option-name opt) 'id))
    (should (equal (clime-option-flags opt) '("--id")))
    (should (eq (clime-option-type opt) 'string))
    (should (functionp (clime-option-conform opt)))
    (should (equal (clime-option-help opt) "A verified ID"))))

(ert-deftest clime-test-dsl/from-override ()
  ":from values are overridden by explicit slot values."
  (eval '(progn
           (clime-defopt test-ovr
             :type 'string
             :help "Template help"
             :required t)
           (clime-app clime-test--from-override
             :version "1"
             (clime-command show
               :help "Show"
               (clime-option id ("--id") :from test-ovr
                 :help "Overridden help" :required nil)
               (clime-handler (ctx) nil))))
        t)
  (let* ((cmd (cdr (assoc "show" (clime-group-children clime-test--from-override))))
         (opt (car (clime-command-options cmd))))
    (should (equal (clime-option-help opt) "Overridden help"))
    (should-not (clime-option-required opt))
    ;; Non-overridden slot inherits from template
    (should (eq (clime-option-type opt) 'string))))

(ert-deftest clime-test-dsl/from-multiple-commands ()
  "Multiple commands can share the same template."
  (eval '(progn
           (clime-defopt test-shared
             :type 'string
             :conform #'identity)
           (clime-app clime-test--from-shared
             :version "1"
             (clime-command copy
               :help "Copy"
               (clime-option src ("--src") :from test-shared :multiple t)
               (clime-option dst ("--dst") :from test-shared)
               (clime-handler (ctx) nil))
             (clime-command move
               :help "Move"
               (clime-option src ("--src") :from test-shared)
               (clime-option dst ("--dst") :from test-shared)
               (clime-handler (ctx) nil))))
        t)
  (let* ((copy (cdr (assoc "copy" (clime-group-children clime-test--from-shared))))
         (move (cdr (assoc "move" (clime-group-children clime-test--from-shared))))
         (copy-src (cl-find-if (lambda (o) (eq (clime-option-name o) 'src))
                               (clime-command-options copy)))
         (copy-dst (cl-find-if (lambda (o) (eq (clime-option-name o) 'dst))
                               (clime-command-options copy)))
         (move-src (cl-find-if (lambda (o) (eq (clime-option-name o) 'src))
                               (clime-command-options move))))
    ;; copy --src has :multiple from override
    (should (clime-option-multiple copy-src))
    ;; copy --dst does not
    (should-not (clime-option-multiple copy-dst))
    ;; move --src does not
    (should-not (clime-option-multiple move-src))
    ;; All share the conform from template
    (should (functionp (clime-option-conform copy-src)))
    (should (functionp (clime-option-conform move-src)))))

(ert-deftest clime-test-dsl/from-parse-integration ()
  ":from options parse correctly end-to-end."
  (eval '(progn
           (clime-defopt test-parse
             :type 'string
             :help "An ID param")
           (clime-app clime-test--from-parse
             :version "1"
             (clime-command show
               :help "Show"
               (clime-option id ("--id" "-i") :from test-parse)
               (clime-handler (ctx) nil))))
        t)
  (let ((result (clime-parse clime-test--from-parse '("show" "--id" "abc"))))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "abc")))
  ;; Short flag works too
  (let ((result (clime-parse clime-test--from-parse '("show" "-i" "xyz"))))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "xyz"))))

;;; ─── clime-defarg: Arg Templates ───────────────────────────────────────

(ert-deftest clime-test-dsl/defarg-basic ()
  "clime-defarg creates a clime--arg-NAME variable holding arg slot defaults."
  (eval '(clime-defarg test-arg-basic
           :type 'string
           :conform #'identity
           :help "A reusable arg")
        t)
  (should (listp clime--arg-test-arg-basic))
  (should (eq (plist-get clime--arg-test-arg-basic :type) 'string))
  (should (functionp (plist-get clime--arg-test-arg-basic :conform)))
  (should (equal (plist-get clime--arg-test-arg-basic :help) "A reusable arg")))

(ert-deftest clime-test-dsl/defarg-rejects-name ()
  "clime-defarg errors if :name is included."
  (should-error
   (eval '(clime-defarg test-arg-bad
            :name 'foo
            :help "Bad")
         t)
   :type 'error))

(ert-deftest clime-test-dsl/defarg-from-basic ()
  ":from inherits arg template defaults."
  (eval '(progn
           (clime-defarg test-arg-id
             :type 'string
             :conform #'identity
             :help "A verified ID")
           (clime-app clime-test--arg-from-basic
             :version "1"
             (clime-command show
               :help "Show"
               (clime-arg id :from test-arg-id)
               (clime-handler (ctx) nil))))
        t)
  (let* ((cmd (cdr (assoc "show" (clime-group-children clime-test--arg-from-basic))))
         (arg (car (clime-command-args cmd))))
    (should (eq (clime-arg-name arg) 'id))
    (should (eq (clime-arg-type arg) 'string))
    (should (functionp (clime-arg-conform arg)))
    (should (equal (clime-arg-help arg) "A verified ID"))))

(ert-deftest clime-test-dsl/defarg-from-override ()
  ":from arg values are overridden by explicit slot values."
  (eval '(progn
           (clime-defarg test-arg-ovr
             :type 'string
             :help "Template help"
             :required t)
           (clime-app clime-test--arg-from-override
             :version "1"
             (clime-command show
               :help "Show"
               (clime-arg id :from test-arg-ovr :help "Overridden" :required nil)
               (clime-handler (ctx) nil))))
        t)
  (let* ((cmd (cdr (assoc "show" (clime-group-children clime-test--arg-from-override))))
         (arg (car (clime-command-args cmd))))
    (should (equal (clime-arg-help arg) "Overridden"))
    (should-not (clime-arg-required arg))
    (should (eq (clime-arg-type arg) 'string))))

(ert-deftest clime-test-dsl/defarg-from-parse-integration ()
  ":from arg templates parse correctly end-to-end."
  (eval '(progn
           (clime-defarg test-arg-parse
             :type 'string
             :help "An ID param")
           (clime-app clime-test--arg-from-parse
             :version "1"
             (clime-command show
               :help "Show"
               (clime-arg id :from test-arg-parse)
               (clime-handler (ctx) nil))))
        t)
  (let ((result (clime-parse clime-test--arg-from-parse '("show" "abc"))))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "abc"))))

;;; ─── Indent Rules ──────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/indent-option ()
  "clime-option has lisp-indent-function 2 (name + flags)."
  (should (equal 2 (get 'clime-option 'lisp-indent-function))))

(ert-deftest clime-test-dsl/indent-arg ()
  "clime-arg has lisp-indent-function 1 (name)."
  (should (equal 1 (get 'clime-arg 'lisp-indent-function))))

(ert-deftest clime-test-dsl/indent-command ()
  "clime-command has lisp-indent-function 1 (name)."
  (should (equal 1 (get 'clime-command 'lisp-indent-function))))

(ert-deftest clime-test-dsl/indent-group ()
  "clime-group has lisp-indent-function 1 (name)."
  (should (equal 1 (get 'clime-group 'lisp-indent-function))))

(ert-deftest clime-test-dsl/indent-handler ()
  "clime-handler has lisp-indent-function 1 (arglist)."
  (should (equal 1 (get 'clime-handler 'lisp-indent-function))))

(provide 'clime-dsl-tests)
;;; clime-dsl-tests.el ends here
