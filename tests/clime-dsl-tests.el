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
  ":flag t emits a deprecation warning via display-warning."
  (let ((msgs (clime-test-with-messages
                (eval '(clime-app clime-test--dsl-flag-depr
                         :version "1"
                         (clime-command test
                           :help "Test"
                           (clime-option force ("--force") :flag t)
                           (clime-handler (ctx) nil)))
                      t))))
    (should (cl-some (lambda (m) (string-match-p "Warning (clime):.*:bool" m)) msgs))))

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

;;; ─── Standalone Macro Evaluation ────────────────────────────────────────

(ert-deftest clime-test-dsl/standalone-option ()
  "clime-option produces a clime-option struct standalone."
  (let ((opt (eval '(clime-option verbose ("-v" "--verbose")
                      :count t :help "Verbosity")
                   t)))
    (should (clime-option-p opt))
    (should (eq (clime-option-name opt) 'verbose))
    (should (equal (clime-option-flags opt) '("-v" "--verbose")))
    (should (clime-option-count opt))
    (should (equal (clime-option-help opt) "Verbosity"))))

(ert-deftest clime-test-dsl/standalone-option-bool ()
  "clime-option with :bool t produces boolean option standalone."
  (let ((opt (eval '(clime-option force ("--force") :bool t :help "Force") t)))
    (should (clime-option-p opt))
    (should (clime-option-boolean-p opt))
    (should (eql (clime-option-nargs opt) 0))))

(ert-deftest clime-test-dsl/standalone-arg ()
  "clime-arg produces a clime-arg struct standalone."
  (let ((arg (eval '(clime-arg name :help "Person" :required nil) t)))
    (should (clime-arg-p arg))
    (should (eq (clime-arg-name arg) 'name))
    (should (equal (clime-arg-help arg) "Person"))
    (should-not (clime-arg-required arg))))

(ert-deftest clime-test-dsl/standalone-handler ()
  "clime-handler produces a callable lambda standalone."
  (let ((h (eval '(clime-handler (ctx) (format "hello %s" ctx)) t)))
    (should (functionp h))
    (should (equal (funcall h "world") "hello world"))))

(ert-deftest clime-test-dsl/standalone-command ()
  "clime-command produces a (name . struct) cons standalone."
  (let ((result (eval '(clime-command greet
                         :help "Say hello"
                         :aliases (g)
                         (clime-arg name :help "Who")
                         (clime-handler (ctx) "hi"))
                      t)))
    (should (consp result))
    (should (equal (car result) "greet"))
    (should (clime-command-p (cdr result)))
    (should (equal (clime-node-help (cdr result)) "Say hello"))
    (should (equal (clime-node-aliases (cdr result)) '("g")))
    (should (= (length (clime-command-args (cdr result))) 1))))

(ert-deftest clime-test-dsl/standalone-group ()
  "clime-group produces a (name . struct) cons standalone."
  (let ((result (eval '(clime-group admin
                         :help "Admin commands"
                         :inline t
                         (clime-command status
                           :help "Status"
                           (clime-handler (ctx) "ok")))
                      t)))
    (should (consp result))
    (should (equal (car result) "admin"))
    (should (clime-group-p (cdr result)))
    (should (clime-node-inline (cdr result)))
    (should (= (length (clime-group-children (cdr result))) 1))))

(ert-deftest clime-test-dsl/standalone-alias-for ()
  "clime-alias-for produces a (name . clime-alias) cons standalone."
  (let ((result (eval '(clime-alias-for waiting (query)
                         :help "Show WAITING"
                         :vals '((todo . "WAITING")))
                      t)))
    (should (consp result))
    (should (equal (car result) "waiting"))
    (should (clime-alias-p (cdr result)))
    (should (equal (clime-node-help (cdr result)) "Show WAITING"))
    (should (equal (clime-alias-vals (cdr result)) '((todo . "WAITING"))))))

(ert-deftest clime-test-dsl/standalone-output-format ()
  "clime-output-format produces a clime-output-format struct standalone."
  (let ((fmt (eval '(clime-output-format json ("--json")
                      :help "JSON output"
                      :streaming t)
                   t)))
    (should (clime-output-format-p fmt))
    (should (clime-option-p fmt))
    (should (eq (clime-output-format-name fmt) 'json))
    (should (equal (clime-option-flags fmt) '("--json")))
    (should (clime-output-format-streaming fmt))
    (should (equal (clime-option-help fmt) "JSON output"))))

(ert-deftest clime-test-dsl/app-with-output-format ()
  "clime-app with clime-output-format generates option and stores format."
  (eval '(clime-app clime-test--output-fmt-app
           :version "1.0"
           :help "Test app"
           (clime-output-format json ("--json") :help "JSON")
           (clime-command test
             :help "Test"
             (clime-handler (_ctx) "ok")))
        t)
  (let ((app (symbol-value 'clime-test--output-fmt-app)))
    (should (= (length (clime-app-output-formats app)) 1))
    (should (clime-node-find-option app "--json"))
    (should (eq (clime-output-format-name (car (clime-app-output-formats app))) 'json))))

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

;;; ─── Emit Helpers ──────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/emit-kw-basic ()
  "emit-kw returns flat plist of present keys."
  (let ((kw '(:help "desc" :hidden t :epilog "end")))
    (should (equal (clime--emit-kw kw '(:help :hidden))
                   '(:help "desc" :hidden t)))))

(ert-deftest clime-test-dsl/emit-kw-absent-keys-omitted ()
  "emit-kw omits keys not in the keywords plist."
  (let ((kw '(:help "desc")))
    (should (equal (clime--emit-kw kw '(:help :hidden :epilog))
                   '(:help "desc")))))

(ert-deftest clime-test-dsl/emit-kw-preserves-explicit-nil ()
  "emit-kw preserves explicitly-false values via plist-member."
  (let ((kw '(:hidden nil :help "desc")))
    (should (equal (clime--emit-kw kw '(:hidden :help))
                   '(:hidden nil :help "desc")))))

(ert-deftest clime-test-dsl/emit-kw-empty ()
  "emit-kw returns nil when no keys match."
  (should (null (clime--emit-kw '(:help "x") '(:hidden :epilog)))))

(ert-deftest clime-test-dsl/emit-body-collections ()
  "emit-body wraps collection values in (list ...)."
  (let* ((opt-form '(clime-make-option :name 'v :flags '("-v")))
         (classified (list :options (list opt-form) :args nil :handler nil)))
    (should (equal (clime--emit-body classified '(:options :args))
                   `(:options (list ,opt-form))))))

(ert-deftest clime-test-dsl/emit-body-handler-bare ()
  "emit-body emits :handler value without list wrapping."
  (let ((handler-form '(lambda (ctx) "ok")))
    (should (equal (clime--emit-body (list :handler handler-form) '(:handler))
                   `(:handler ,handler-form)))))

(ert-deftest clime-test-dsl/emit-body-skips-nil ()
  "emit-body omits keys with nil values."
  (should (null (clime--emit-body '(:options nil :args nil) '(:options :args)))))

(ert-deftest clime-test-dsl/prepare-aliases-normalizes ()
  "prepare-aliases normalizes symbol aliases to quoted string lists."
  (let* ((kw '(:help "desc" :aliases (foo bar)))
         (result (clime--prepare-aliases kw)))
    (should (equal (plist-get result :aliases) '(quote ("foo" "bar"))))
    (should (equal (plist-get result :help) "desc"))))

(ert-deftest clime-test-dsl/prepare-aliases-no-op ()
  "prepare-aliases returns keywords unchanged when no :aliases."
  (let ((kw '(:help "desc")))
    (should (eq (clime--prepare-aliases kw) kw))))

;;; ─── Bare Boolean Normalization ─────────────────────────────────────

(ert-deftest clime-test-dsl/bare-bool-single ()
  "Bare :bool at end of plist normalizes to :bool t."
  (should (equal (clime--normalize-bare-booleans '(:bool) clime--boolean-keywords)
                 '(:bool t))))

(ert-deftest clime-test-dsl/bare-bool-before-keyword ()
  "Bare :bool before another keyword inserts t."
  (should (equal (clime--normalize-bare-booleans '(:bool :hidden) clime--boolean-keywords)
                 '(:bool t :hidden t))))

(ert-deftest clime-test-dsl/bare-bool-before-cons ()
  "Bare :hidden before a cons form (DSL child) inserts t."
  (should (equal (clime--normalize-bare-booleans
                  '(:hidden (clime-option foo ("--foo")))
                  clime--boolean-keywords)
                 '(:hidden t (clime-option foo ("--foo"))))))

(ert-deftest clime-test-dsl/bare-bool-explicit-t-unchanged ()
  "Explicit :bool t passes through unchanged."
  (should (equal (clime--normalize-bare-booleans '(:bool t :hidden t) clime--boolean-keywords)
                 '(:bool t :hidden t))))

(ert-deftest clime-test-dsl/bare-bool-explicit-nil-preserved ()
  "Explicit :hidden nil is preserved (not replaced with t)."
  (should (equal (clime--normalize-bare-booleans '(:hidden nil) clime--boolean-keywords)
                 '(:hidden nil))))

(ert-deftest clime-test-dsl/bare-bool-non-boolean-key-unchanged ()
  "Non-boolean keyword :help with value passes through normally."
  (should (equal (clime--normalize-bare-booleans '(:help "desc" :bool) clime--boolean-keywords)
                 '(:help "desc" :bool t))))

(ert-deftest clime-test-dsl/bare-bool-deprecated-string-preserved ()
  ":deprecated followed by string preserves the string value."
  (should (equal (clime--normalize-bare-booleans '(:deprecated "use --new") clime--boolean-keywords)
                 '(:deprecated "use --new"))))

(ert-deftest clime-test-dsl/bare-bool-deprecated-bare ()
  "Bare :deprecated at end normalizes to :deprecated t."
  (should (equal (clime--normalize-bare-booleans '(:deprecated) clime--boolean-keywords)
                 '(:deprecated t))))

(ert-deftest clime-test-dsl/bare-bool-mixed-plist ()
  "Mixed bare and valued keywords in option-like plist."
  (should (equal (clime--normalize-bare-booleans
                  '(:bool :multiple :default "x" :hidden)
                  clime--boolean-keywords)
                 '(:bool t :multiple t :default "x" :hidden t))))

(ert-deftest clime-test-dsl/bare-bool-nargs-keyword-value ()
  "Non-boolean :nargs consumes :rest as its value, not as bare boolean."
  (should (equal (clime--normalize-bare-booleans
                  '(:nargs :rest :required nil :help "IDs")
                  clime--boolean-keywords)
                 '(:nargs :rest :required nil :help "IDs"))))

(ert-deftest clime-test-dsl/bare-bool-nargs-integer ()
  "Non-boolean :nargs with integer value passes through."
  (should (equal (clime--normalize-bare-booleans
                  '(:nargs 0 :hidden)
                  clime--boolean-keywords)
                 '(:nargs 0 :hidden t)))
  (should (equal (clime--normalize-bare-booleans
                  '(:nargs 2 :help "stuff")
                  clime--boolean-keywords)
                 '(:nargs 2 :help "stuff"))))

(ert-deftest clime-test-dsl/bare-bool-option-integration ()
  "Bare :bool in clime-option DSL produces :nargs 0 option."
  (eval '(clime-app clime-test--bare-bool-opt
           :version "1"
           (clime-command run
             :help "run"
             (clime-option verbose ("--verbose" "-v") :bool :hidden)
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd (cdr (car (clime-group-children clime-test--bare-bool-opt))))
         (opt (car (clime-command-options cmd))))
    (should (= 0 (clime-option-nargs opt)))
    (should (clime-option-hidden opt))))

(ert-deftest clime-test-dsl/bare-bool-command-integration ()
  "Bare :hidden in clime-command DSL hides the command."
  (eval '(clime-app clime-test--bare-bool-cmd
           :version "1"
           (clime-command secret
             :hidden
             :help "secret cmd"
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (car (clime-group-children clime-test--bare-bool-cmd)))))
    (should (clime-node-hidden cmd))))

(ert-deftest clime-test-dsl/bare-bool-group-inline ()
  "Bare :inline in clime-group DSL sets inline flag."
  (eval '(clime-app clime-test--bare-bool-grp
           :version "1"
           (clime-group admin
             :inline
             :help "admin"
             (clime-command nuke
               :help "nuke"
               (clime-handler (ctx) nil))))
        t)
  (let ((grp (cdr (car (clime-group-children clime-test--bare-bool-grp)))))
    (should (clime-group-inline grp))))

;;; ─── DSL Aliases ──────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/clime-opt-alias ()
  "clime-opt produces identical struct to clime-option."
  (let ((via-option (eval '(clime-option verbose ("-v" "--verbose") :bool :help "Verbose") t))
        (via-opt    (eval '(clime-opt    verbose ("-v" "--verbose") :bool :help "Verbose") t)))
    (should (equal via-option via-opt))))

(ert-deftest clime-test-dsl/clime-argument-alias ()
  "clime-argument produces identical struct to clime-arg."
  (let ((via-arg      (eval '(clime-arg      name :help "Person" :required nil) t))
        (via-argument (eval '(clime-argument name :help "Person" :required nil) t)))
    (should (equal via-arg via-argument))))

(ert-deftest clime-test-dsl/clime-defoption-alias ()
  "clime-defoption works as alias for clime-defopt."
  (eval '(clime-defoption test-alias-defoption :type 'string :help "Test") t)
  (should (boundp 'clime--opt-test-alias-defoption))
  (should (plist-get (symbol-value 'clime--opt-test-alias-defoption) :type)))

(ert-deftest clime-test-dsl/clime-defargument-alias ()
  "clime-defargument works as alias for clime-defarg."
  (eval '(clime-defargument test-alias-defargument :type 'string :help "Test") t)
  (should (boundp 'clime--arg-test-alias-defargument))
  (should (plist-get (symbol-value 'clime--arg-test-alias-defargument) :type)))

(ert-deftest clime-test-dsl/classify-body-recognizes-clime-opt ()
  "clime--classify-body recognizes clime-opt in app/command bodies."
  (eval '(clime-app clime-test--alias-opt-app
           :version "1"
           (clime-opt name ("-n" "--name") :help "Name")
           (clime-handler (ctx) nil))
        t)
  (should (clime-node-find-option clime-test--alias-opt-app "--name")))

(ert-deftest clime-test-dsl/classify-body-recognizes-clime-argument ()
  "clime--classify-body recognizes clime-argument in app/command bodies."
  (eval '(clime-app clime-test--alias-arg-app
           :version "1"
           (clime-argument who :help "Who")
           (clime-handler (ctx) nil))
        t)
  (should (= 1 (length (clime-app-args clime-test--alias-arg-app)))))

(ert-deftest clime-test-dsl/clime-opt-inside-command ()
  "clime-opt works inside a clime-command body."
  (eval '(clime-app clime-test--alias-opt-cmd-app
           :version "1"
           (clime-command greet
             :help "Greet"
             (clime-opt name ("-n" "--name") :help "Name")
             (clime-argument who :help "Who")
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (car (clime-app-children clime-test--alias-opt-cmd-app)))))
    (should (clime-node-find-option cmd "--name"))
    (should (= 1 (length (clime-command-args cmd))))))

(ert-deftest clime-test-dsl/indent-clime-opt ()
  "clime-opt has same indent as clime-option (2)."
  (should (equal 2 (get 'clime-opt 'lisp-indent-function))))

(ert-deftest clime-test-dsl/indent-clime-argument ()
  "clime-argument has same indent as clime-arg (1)."
  (should (equal 1 (get 'clime-argument 'lisp-indent-function))))

;;; ─── Alias Forms in Mutex/Zip ──────────────────────────────────────────

(ert-deftest clime-test-dsl/clime-opt-inside-mutex ()
  "clime-opt (short alias) works inside clime-mutex."
  (eval '(clime-app clime-test--alias-opt-mutex
           :version "1"
           (clime-command run
             :help "Run"
             (clime-mutex fmt
               (clime-opt json ("--json") :bool)
               (clime-opt csv ("--csv") :bool))
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (car (clime-app-children clime-test--alias-opt-mutex)))))
    (should (clime-node-find-option cmd "--json"))
    (should (clime-node-find-option cmd "--csv"))))

(ert-deftest clime-test-dsl/clime-opt-inside-zip ()
  "clime-opt (short alias) works inside clime-zip."
  (eval '(clime-app clime-test--alias-opt-zip
           :version "1"
           (clime-command run
             :help "Run"
             (clime-zip sr
               (clime-opt skip ("--skip"))
               (clime-opt reason ("--reason")))
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (car (clime-app-children clime-test--alias-opt-zip)))))
    (should (clime-node-find-option cmd "--skip"))
    (should (clime-node-find-option cmd "--reason"))))

(ert-deftest clime-test-dsl/mutex-alias-identical-to-canonical ()
  "clime-mutex with clime-opt produces same options as clime-option."
  (eval '(clime-app clime-test--mutex-via-opt
           :version "1"
           (clime-command run
             :help "Run"
             (clime-mutex fmt
               (clime-opt json ("--json") :bool)
               (clime-opt csv ("--csv") :bool))
             (clime-handler (ctx) nil)))
        t)
  (eval '(clime-app clime-test--mutex-via-option
           :version "1"
           (clime-command run
             :help "Run"
             (clime-mutex fmt
               (clime-option json ("--json") :bool)
               (clime-option csv ("--csv") :bool))
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd-opt (cdr (car (clime-app-children clime-test--mutex-via-opt))))
         (cmd-option (cdr (car (clime-app-children clime-test--mutex-via-option)))))
    (should (equal (clime-command-options cmd-opt)
                   (clime-command-options cmd-option)))))

(ert-deftest clime-test-dsl/zip-alias-identical-to-canonical ()
  "clime-zip with clime-opt produces same options as clime-option."
  (eval '(clime-app clime-test--zip-via-opt
           :version "1"
           (clime-command run
             :help "Run"
             (clime-zip sr
               (clime-opt skip ("--skip"))
               (clime-opt reason ("--reason")))
             (clime-handler (ctx) nil)))
        t)
  (eval '(clime-app clime-test--zip-via-option
           :version "1"
           (clime-command run
             :help "Run"
             (clime-zip sr
               (clime-option skip ("--skip"))
               (clime-option reason ("--reason")))
             (clime-handler (ctx) nil)))
        t)
  (let* ((cmd-opt (cdr (car (clime-app-children clime-test--zip-via-opt))))
         (cmd-option (cdr (car (clime-app-children clime-test--zip-via-option)))))
    (should (equal (clime-command-options cmd-opt)
                   (clime-command-options cmd-option)))))

;;; ─── Alias Forms in Group Bodies ───────────────────────────────────────

(ert-deftest clime-test-dsl/clime-opt-inside-group ()
  "clime-opt works inside a clime-group body."
  (eval '(clime-app clime-test--alias-opt-grp
           :version "1"
           (clime-group config
             :help "Config"
             (clime-opt scope ("--scope") :default "local")
             (clime-argument profile :help "Profile name")
             (clime-command list
               :help "List"
               (clime-handler (ctx) nil))))
        t)
  (let ((grp (cdr (assoc "config" (clime-group-children clime-test--alias-opt-grp)))))
    (should (= (length (clime-group-options grp)) 1))
    (should (= (length (clime-group-args grp)) 1))
    (should (eq (clime-option-name (car (clime-group-options grp))) 'scope))
    (should (eq (clime-arg-name (car (clime-group-args grp))) 'profile))))

;;; ─── Template (:from) Inside Mutex/Zip ─────────────────────────────────

(ert-deftest clime-test-dsl/from-option-inside-mutex ()
  "clime-opt with :from template works inside clime-mutex."
  (eval '(progn
           (clime-defopt test-mutex-tmpl :bool :help "A format flag")
           (clime-app clime-test--from-mutex
             :version "1"
             (clime-command run
               :help "Run"
               (clime-mutex fmt
                 (clime-opt json ("--json") :from test-mutex-tmpl)
                 (clime-opt csv ("--csv") :from test-mutex-tmpl))
               (clime-handler (ctx) nil))))
        t)
  (let ((cmd (cdr (car (clime-app-children clime-test--from-mutex)))))
    (should (clime-node-find-option cmd "--json"))
    (should (clime-node-find-option cmd "--csv"))
    (should (clime-option-boolean-p (clime-node-find-option cmd "--json")))))

(ert-deftest clime-test-dsl/from-option-inside-zip ()
  "clime-opt with :from template works inside clime-zip."
  (eval '(progn
           (clime-defopt test-zip-tmpl :type 'string :help "A value")
           (clime-app clime-test--from-zip
             :version "1"
             (clime-command run
               :help "Run"
               (clime-zip sr
                 (clime-opt skip ("--skip") :from test-zip-tmpl)
                 (clime-opt reason ("--reason") :from test-zip-tmpl))
               (clime-handler (ctx) nil))))
        t)
  (let ((cmd (cdr (car (clime-app-children clime-test--from-zip)))))
    (should (clime-node-find-option cmd "--skip"))
    (should (clime-node-find-option cmd "--reason"))
    ;; Zip auto-sets :multiple
    (should (clime-option-multiple (clime-node-find-option cmd "--skip")))))

;;; ─── Form Composition: clime-def* Macros ─────────────────────────────────

(ert-deftest clime-test-dsl/defcommand-basic ()
  "clime-defcommand binds a defvar to (name . command-struct)."
  (eval '(clime-defcommand clime-test--defcmd-show
           :help "Show a thing"
           (clime-arg id :help "Resource ID")
           (clime-handler (ctx) "shown"))
        t)
  (should (consp clime-test--defcmd-show))
  (should (equal (car clime-test--defcmd-show) "clime-test--defcmd-show"))
  (should (clime-command-p (cdr clime-test--defcmd-show)))
  (should (equal (clime-node-help (cdr clime-test--defcmd-show)) "Show a thing"))
  (should (= 1 (length (clime-command-args (cdr clime-test--defcmd-show)))))
  (should (functionp (clime-command-handler (cdr clime-test--defcmd-show)))))

(ert-deftest clime-test-dsl/defgroup-basic ()
  "clime-defgroup binds a defvar to (name . group-struct)."
  (eval '(clime-defgroup clime-test--defgrp-admin
           :help "Admin commands"
           (clime-command status
             :help "Status"
             (clime-handler (ctx) "ok")))
        t)
  (should (consp clime-test--defgrp-admin))
  (should (equal (car clime-test--defgrp-admin) "clime-test--defgrp-admin"))
  (should (clime-group-p (cdr clime-test--defgrp-admin)))
  (should (equal (clime-node-help (cdr clime-test--defgrp-admin)) "Admin commands"))
  (should (= 1 (length (clime-group-children (cdr clime-test--defgrp-admin))))))

(ert-deftest clime-test-dsl/defcommand-indent ()
  "clime-defcommand has indent 1."
  (should (equal 1 (get 'clime-defcommand 'lisp-indent-function))))

(ert-deftest clime-test-dsl/defgroup-indent ()
  "clime-defgroup has indent 1."
  (should (equal 1 (get 'clime-defgroup 'lisp-indent-function))))

;;; ─── Form Composition: Keyword-Merge in Containers ──────────────────────

(ert-deftest clime-test-dsl/app-kw-options ()
  "clime-app :options keyword merges with inline options."
  (eval '(progn
           (defvar clime-test--shared-verbose
             (clime-opt verbose ("-v" "--verbose") :count t :help "Verbosity"))
           (clime-app clime-test--kw-opts-app
             :version "1"
             :options (list clime-test--shared-verbose)
             (clime-opt debug ("-d" "--debug") :bool :help "Debug mode")
             (clime-command ping
               :help "Ping"
               (clime-handler (ctx) "pong"))))
        t)
  (let ((app clime-test--kw-opts-app))
    ;; Both keyword-provided and inline options present
    (should (= 2 (length (clime-group-options app))))
    ;; Keyword options come first
    (should (eq 'verbose (clime-option-name (car (clime-group-options app)))))
    (should (eq 'debug (clime-option-name (cadr (clime-group-options app)))))))

(ert-deftest clime-test-dsl/app-kw-children ()
  "clime-app :children keyword merges with inline children."
  (eval '(progn
           (clime-defcommand clime-test--ext-show
             :help "Show"
             (clime-arg id :help "ID")
             (clime-handler (ctx) nil))
           (clime-app clime-test--kw-children-app
             :version "1"
             :children (list clime-test--ext-show)
             (clime-command add
               :help "Add"
               (clime-handler (ctx) nil))))
        t)
  (let ((app clime-test--kw-children-app))
    (should (= 2 (length (clime-group-children app))))
    ;; Keyword children come first
    (should (equal "clime-test--ext-show" (caar (clime-group-children app))))
    (should (equal "add" (caadr (clime-group-children app))))))

(ert-deftest clime-test-dsl/app-kw-args ()
  "clime-app :args keyword merges with inline args."
  (eval '(progn
           (defvar clime-test--shared-target
             (clime-arg target :help "Target"))
           (clime-app clime-test--kw-args-app
             :version "1"
             :args (list clime-test--shared-target)
             (clime-arg extra :help "Extra" :required nil)
             (clime-handler (ctx) nil)))
        t)
  (let ((app clime-test--kw-args-app))
    (should (= 2 (length (clime-group-args app))))
    (should (eq 'target (clime-arg-name (car (clime-group-args app)))))
    (should (eq 'extra (clime-arg-name (cadr (clime-group-args app)))))))

(ert-deftest clime-test-dsl/app-kw-output-formats ()
  "clime-app :output-formats keyword merges with inline formats."
  (eval '(progn
           (defvar clime-test--ext-yaml
             (clime-output-format yaml ("--yaml") :help "YAML output"))
           (clime-app clime-test--kw-fmts-app
             :version "1"
             :output-formats (list clime-test--ext-yaml)
             (clime-output-format csv ("--csv") :help "CSV output")
             (clime-command test
               :help "Test"
               (clime-handler (ctx) nil))))
        t)
  (let ((app clime-test--kw-fmts-app))
    (should (= 2 (length (clime-app-output-formats app))))))

(ert-deftest clime-test-dsl/group-kw-merge ()
  "clime-group accepts :options, :children, :args keywords."
  (eval '(progn
           (defvar clime-test--grp-shared-opt
             (clime-opt scope ("--scope") :default "local"))
           (clime-defcommand clime-test--grp-ext-list
             :help "List"
             (clime-handler (ctx) nil))
           (clime-app clime-test--kw-grp-app
             :version "1"
             (clime-group config
               :help "Config"
               :options (list clime-test--grp-shared-opt)
               :children (list clime-test--grp-ext-list)
               (clime-opt color ("--color") :bool)
               (clime-command set
                 :help "Set"
                 (clime-handler (ctx) nil)))))
        t)
  (let ((grp (cdr (assoc "config" (clime-group-children clime-test--kw-grp-app)))))
    (should (= 2 (length (clime-group-options grp))))
    (should (eq 'scope (clime-option-name (car (clime-group-options grp)))))
    (should (= 2 (length (clime-group-children grp))))
    (should (equal "clime-test--grp-ext-list" (caar (clime-group-children grp))))))

(ert-deftest clime-test-dsl/command-kw-merge ()
  "clime-command accepts :options, :args keywords."
  (eval '(progn
           (defvar clime-test--cmd-shared-opt
             (clime-opt format ("-f" "--format") :default "text"))
           (clime-app clime-test--kw-cmd-app
             :version "1"
             (clime-command show
               :help "Show"
               :options (list clime-test--cmd-shared-opt)
               (clime-opt color ("--color") :bool)
               (clime-arg id :help "ID")
               (clime-handler (ctx) nil))))
        t)
  (let ((cmd (cdr (assoc "show" (clime-group-children clime-test--kw-cmd-app)))))
    (should (= 2 (length (clime-command-options cmd))))
    (should (eq 'format (clime-option-name (car (clime-command-options cmd)))))))

(ert-deftest clime-test-dsl/kw-only-no-inline ()
  "Keyword-only composition works (no inline forms)."
  (eval '(progn
           (clime-defcommand clime-test--kw-only-show
             :help "Show"
             (clime-arg id :help "ID")
             (clime-handler (ctx) nil))
           (clime-defcommand clime-test--kw-only-add
             :help "Add"
             (clime-handler (ctx) nil))
           (clime-app clime-test--kw-only-app
             :version "1"
             :children (list clime-test--kw-only-show
                            clime-test--kw-only-add)))
        t)
  (let ((app clime-test--kw-only-app))
    (should (= 2 (length (clime-group-children app))))))

(ert-deftest clime-test-dsl/inline-only-unchanged ()
  "Inline-only composition (no keywords) still works as before."
  (eval '(clime-app clime-test--inline-only-app
           :version "1"
           (clime-command ping
             :help "Ping"
             (clime-handler (ctx) "pong"))
           (clime-command pong
             :help "Pong"
             (clime-handler (ctx) "ping")))
        t)
  (should (= 2 (length (clime-group-children clime-test--inline-only-app)))))

;;; ─── Form Composition: Integration ──────────────────────────────────────

(ert-deftest clime-test-dsl/compose-parse-dispatch ()
  "App composed via keyword-merge parses and dispatches correctly."
  (eval '(progn
           (clime-defcommand clime-test--compose-show
             :help "Show"
             (clime-opt format ("-f" "--format") :default "text")
             (clime-arg id :help "ID")
             (clime-handler (ctx)
               (list (clime-ctx-get ctx 'id)
                     (clime-ctx-get ctx 'format))))
           (defvar clime-test--compose-verbose
             (clime-opt verbose ("-v" "--verbose") :count t))
           (clime-app clime-test--compose-app
             :version "1"
             :options (list clime-test--compose-verbose)
             :children (list clime-test--compose-show)
             (clime-command add
               :help "Add"
               (clime-handler (ctx) "added"))))
        t)
  ;; Parse: external child command
  (let ((result (clime-parse clime-test--compose-app
                             '("-v" "clime-test--compose-show" "-f" "json" "abc"))))
    (should (equal (clime-command-name (clime-parse-result-command result))
                   "clime-test--compose-show"))
    (should (= 1 (plist-get (clime-parse-result-params result) 'verbose)))
    (should (equal "json" (plist-get (clime-parse-result-params result) 'format)))
    (should (equal "abc" (plist-get (clime-parse-result-params result) 'id))))
  ;; Parse: inline child command
  (let ((result (clime-parse clime-test--compose-app '("add"))))
    (should (equal (clime-command-name (clime-parse-result-command result)) "add"))))

(ert-deftest clime-test-dsl/compose-group-with-defcommands ()
  "defgroup with defcommand children composes into app."
  (eval '(progn
           (clime-defcommand clime-test--dep-add
             :help "Add dependency"
             (clime-arg id :help "Dep ID")
             (clime-handler (ctx) nil))
           (clime-defcommand clime-test--dep-rm
             :help "Remove dependency"
             :aliases (rm)
             (clime-arg id :help "Dep ID")
             (clime-handler (ctx) nil))
           (clime-defgroup clime-test--dep-grp
             :help "Dependencies"
             :children (list clime-test--dep-add
                            clime-test--dep-rm))
           (clime-app clime-test--compose-grp-app
             :version "1"
             :children (list clime-test--dep-grp)))
        t)
  (let* ((app clime-test--compose-grp-app)
         (dep (cdr (assoc "clime-test--dep-grp" (clime-group-children app)))))
    (should (clime-group-p dep))
    (should (= 2 (length (clime-group-children dep))))))

(ert-deftest clime-test-dsl/compose-tree-validation ()
  "Tree validation catches flag collisions from keyword-merged options."
  (should-error
   (eval '(progn
            (defvar clime-test--collision-opt
              (clime-opt verbose ("-v" "--verbose") :count t))
            (clime-app clime-test--collision-app
              :version "1"
              :options (list clime-test--collision-opt)
              (clime-command show
                :help "Show"
                ;; Collides with root -v/--verbose
                (clime-opt verbose ("-v") :bool)
                (clime-handler (ctx) nil))))
         t)
   :type 'error))

;;; ─── Merge Helper ───────────────────────────────────────────────────────

(ert-deftest clime-test-dsl/merge-kw-body-both ()
  "clime--merge-kw-body with both kw and body produces append form."
  (let ((result (clime--merge-kw-body 'my-list '(a b))))
    (should (equal result '(append my-list (list a b))))))

(ert-deftest clime-test-dsl/merge-kw-body-kw-only ()
  "clime--merge-kw-body with kw only returns kw expression."
  (should (equal (clime--merge-kw-body 'my-list nil) 'my-list)))

(ert-deftest clime-test-dsl/merge-kw-body-body-only ()
  "clime--merge-kw-body with body only returns (list ...)."
  (should (equal (clime--merge-kw-body nil '(a b)) '(list a b))))

(ert-deftest clime-test-dsl/merge-kw-body-neither ()
  "clime--merge-kw-body with neither returns nil."
  (should-not (clime--merge-kw-body nil nil)))

;;; ─── Form Composition: Edge Cases ────────────────────────────────────────

(ert-deftest clime-test-dsl/kw-options-with-mutex ()
  "Keyword :options merges correctly alongside inline clime-mutex."
  (eval '(progn
           (defvar clime-test--mutex-shared-opt
             (clime-opt verbose ("-v" "--verbose") :count t))
           (clime-app clime-test--kw-mutex-app
             :version "1"
             :options (list clime-test--mutex-shared-opt)
             (clime-command run
               :help "Run"
               (clime-mutex fmt
                 (clime-opt json ("--json") :bool)
                 (clime-opt csv ("--csv") :bool))
               (clime-handler (ctx) nil))))
        t)
  (let* ((app clime-test--kw-mutex-app)
         (cmd (cdr (assoc "run" (clime-group-children app)))))
    ;; Root has keyword-provided option
    (should (= 1 (length (clime-group-options app))))
    (should (eq 'verbose (clime-option-name (car (clime-group-options app)))))
    ;; Command has mutex options
    (should (= 2 (length (clime-command-options cmd))))
    (should (clime-node-find-option cmd "--json"))
    (should (clime-node-find-option cmd "--csv"))))

(ert-deftest clime-test-dsl/kw-empty-list ()
  "Empty :options keyword list doesn't break things."
  (eval '(clime-app clime-test--kw-empty-app
           :version "1"
           :options nil
           :children nil
           (clime-opt debug ("-d") :bool)
           (clime-command ping
             :help "Ping"
             (clime-handler (ctx) nil)))
        t)
  (let ((app clime-test--kw-empty-app))
    (should (= 1 (length (clime-group-options app))))
    (should (= 1 (length (clime-group-children app))))))

(ert-deftest clime-test-dsl/defcommand-with-kw-options ()
  "clime-defcommand itself can use :options keyword."
  (eval '(progn
           (defvar clime-test--defcmd-shared-opt
             (clime-opt format ("-f" "--format") :default "text"))
           (clime-defcommand clime-test--defcmd-kw-show
             :help "Show"
             :options (list clime-test--defcmd-shared-opt)
             (clime-opt color ("--color") :bool)
             (clime-arg id :help "ID")
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr clime-test--defcmd-kw-show)))
    (should (= 2 (length (clime-command-options cmd))))
    (should (eq 'format (clime-option-name (car (clime-command-options cmd)))))
    (should (eq 'color (clime-option-name (cadr (clime-command-options cmd)))))))

(ert-deftest clime-test-dsl/kw-duplicate-flags-error ()
  "Duplicate flags between keyword-merged options on same node error."
  (should-error
   (eval '(progn
            (defvar clime-test--dup-opt-a
              (clime-opt verbose ("-v" "--verbose") :count t))
            (clime-app clime-test--dup-app
              :version "1"
              :options (list clime-test--dup-opt-a)
              ;; Same flags on same node — duplicate flag error
              (clime-opt verbose2 ("-v") :bool)
              (clime-command ping
                :help "Ping"
                (clime-handler (ctx) nil))))
         t)
   :type 'error))

;;; ─── :examples Keyword ──────────────────────────────────────────────────

(ert-deftest clime-test-dsl/examples-on-command ()
  ":examples keyword passes through to command struct."
  (eval '(clime-app clime-test--ex-cmd-app
           :version "1"
           (clime-command show
             :help "Show"
             :examples '(("app show 1" . "By ID")
                         ("app show --all" . "All"))
             (clime-handler (ctx) nil)))
        t)
  (let ((cmd (cdr (assoc "show" (clime-group-children clime-test--ex-cmd-app)))))
    (should (= 2 (length (clime-node-examples cmd))))
    (should (equal (caar (clime-node-examples cmd)) "app show 1"))))

(ert-deftest clime-test-dsl/examples-on-app ()
  ":examples keyword works on app."
  (eval '(clime-app clime-test--ex-app
           :version "1"
           :examples '(("myapp init" . "Init"))
           (clime-command ping
             :help "Ping"
             (clime-handler (ctx) nil)))
        t)
  (should (= 1 (length (clime-node-examples clime-test--ex-app)))))

(provide 'clime-dsl-tests)
;;; clime-dsl-tests.el ends here
