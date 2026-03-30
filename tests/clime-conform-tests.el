;;; clime-conform-tests.el --- Tests for unified :conform interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the unified :conform model:
;; - Option/arg :conform signature: (value, param) → value
;; - Node :conform signature: (values, node) → values
;; - Leaf→root walk order during finalization
;; - Composition (list of conform fns on a node)
;; - Exclusive/paired checks as node conform functions
;;
;; These tests define the target API.  They should fail until
;; the implementation replaces the cohort system.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Option :conform (value, param) → value ──────────────────────────

(ert-deftest clime-test-conform/option-receives-param ()
  "Option :conform receives (value, param) — param is the option struct."
  (let* ((received-param nil)
         (opt (clime-make-option :name 'id :flags '("--id")
                                  :conform (lambda (v param)
                                             (setq received-param param)
                                             v)))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "show" cmd)))))
    (clime-parse app '("show" "--id" "42"))
    (should (clime-option-p received-param))
    (should (eq (clime-option-name received-param) 'id))))

(ert-deftest clime-test-conform/option-transforms-value ()
  "Option :conform (value, param) return value replaces the param."
  (let* ((opt (clime-make-option :name 'status :flags '("--status")
                                  :conform (lambda (v _param) (upcase v))))
         (cmd (clime-make-command :name "list" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "list" cmd))))
         (result (clime-parse app '("list" "--status" "todo"))))
    (should (equal (plist-get (clime-parse-result-params result) 'status)
                   "TODO"))))

(ert-deftest clime-test-conform/option-fail-signals-usage-error ()
  "Option :conform that signals error produces clime-usage-error."
  (let* ((opt (clime-make-option :name 'id :flags '("--id")
                                  :conform (lambda (v _param)
                                             (unless (string-match-p "^[0-9]+$" v)
                                               (error "Must be numeric"))
                                             v)))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--id" "abc"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-conform/option-nil-skipped ()
  "Option :conform is skipped when value is nil (not supplied)."
  (let* ((called nil)
         (opt (clime-make-option :name 'id :flags '("--id")
                                  :conform (lambda (_v _param) (setq called t))))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "show" cmd)))))
    (clime-parse app '("show"))
    (should-not called)))

(ert-deftest clime-test-conform/option-multiple-receives-list ()
  "Option :conform receives the full list for :multiple options."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag") :multiple t
                                  :conform (lambda (vs _param) (mapcar #'upcase vs))))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--tag" "a" "--tag" "b"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("A" "B")))))

(ert-deftest clime-test-conform/option-can-inspect-param-metadata ()
  "Option :conform can use param struct to inspect :type, :flags, etc."
  (let* ((opt (clime-make-option :name 'port :flags '("--port" "-p")
                                  :type 'integer
                                  :conform (lambda (v param)
                                             ;; Use flags from the param struct
                                             (when (and (eq (clime-option-type param) 'integer)
                                                        (< v 1))
                                               (error "Port must be positive"))
                                             v)))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "serve" cmd)))))
    (should-error (clime-parse app '("serve" "--port" "0"))
                  :type 'clime-usage-error)))

;;; ─── Arg :conform (value, param) → value ─────────────────────────────

(ert-deftest clime-test-conform/arg-receives-param ()
  "Arg :conform receives (value, param) — param is the arg struct."
  (let* ((received-param nil)
         (arg (clime-make-arg :name 'file :required t
                               :conform (lambda (v param)
                                          (setq received-param param)
                                          v)))
         (cmd (clime-make-command :name "load" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "load" cmd)))))
    (clime-parse app '("load" "test.el"))
    (should (clime-arg-p received-param))
    (should (eq (clime-arg-name received-param) 'file))))

(ert-deftest clime-test-conform/arg-transforms-value ()
  "Arg :conform return value replaces the param."
  (let* ((arg (clime-make-arg :name 'name :required t
                               :conform (lambda (v _param) (downcase v))))
         (cmd (clime-make-command :name "greet" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "greet" cmd))))
         (result (clime-parse app '("greet" "WORLD"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "world"))))

;;; ─── Node :conform (values, node) → values ──────────────────────────

(ert-deftest clime-test-conform/node-conform-on-command ()
  "Command-level :conform receives (values, node) and transforms values."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--aa") :nargs 0))
         (opt-b (clime-make-option :name 'b :flags '("--bb") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'derived)))
                                  :options (list opt-a opt-b)
                                  :conform (lambda (values _node)
                                             ;; Inject a derived value
                                             (clime-values-set values 'derived
                                                               (if (clime-values-value values 'a) "got-a" "no-a")
                                                               'conform))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--aa")))))
    (should (equal output "got-a"))))

(ert-deftest clime-test-conform/node-conform-receives-node ()
  "Node :conform second arg is the node struct itself."
  (let* ((received-node nil)
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (lambda (params node)
                                             (setq received-node node)
                                             params)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (clime-parse app '("run"))
    (should (clime-command-p received-node))
    (should (equal (clime-node-name received-node) "run"))))

(ert-deftest clime-test-conform/node-conform-error-becomes-usage-error ()
  "Error signaled in node :conform becomes clime-usage-error."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (lambda (_params _node)
                                             (error "Something went wrong"))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error (clime-parse app '("run"))
                  :type 'clime-usage-error)))

;;; ─── Walk Order: Option Conform Before Node Conform ──────────────────

(ert-deftest clime-test-conform/option-before-node ()
  "Option :conform runs before node :conform."
  (let* ((order '())
         (opt (clime-make-option :name 'x :flags '("--xx")
                                  :conform (lambda (v _param)
                                             (push 'option order)
                                             v)))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)
                                  :conform (lambda (params _node)
                                             (push 'node order)
                                             params)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (clime-parse app '("run" "--xx" "val"))
    (should (equal (nreverse order) '(option node)))))

;;; ─── Walk Order: Leaf → Root ─────────────────────────────────────────

(ert-deftest clime-test-conform/leaf-to-root-walk ()
  "Node :conform runs leaf→root: command before app."
  (let* ((order '())
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (lambda (params _node)
                                             (push 'command order)
                                             params)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))
                               :conform (lambda (params _node)
                                           (push 'app order)
                                           params))))
    (clime-parse app '("run"))
    (should (equal (nreverse order) '(command app)))))

(ert-deftest clime-test-conform/intermediate-group-in-walk ()
  "Node :conform on intermediate group runs between command and app."
  (let* ((order '())
         (cmd (clime-make-command :name "start" :handler #'ignore
                                  :conform (lambda (params _node)
                                             (push 'command order)
                                             params)))
         (grp (clime-make-group :name "agents"
                                 :children (list (cons "start" cmd))
                                 :conform (lambda (params _node)
                                            (push 'group order)
                                            params)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "agents" grp))
                               :conform (lambda (params _node)
                                           (push 'app order)
                                           params))))
    (clime-parse app '("agents" "start"))
    (should (equal (nreverse order) '(command group app)))))

;;; ─── Conform List (Composition) ──────────────────────────────────────

(ert-deftest clime-test-conform/node-conform-list ()
  "Node :conform accepts a list of functions, run in sequence."
  (let* ((order '())
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (list (lambda (values _node)
                                                   (push 'first order)
                                                   (clime-values-set values 'x 1 'conform))
                                                 (lambda (values _node)
                                                   (push 'second order)
                                                   (clime-values-set values 'y 2 'conform)))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (result (clime-parse app '("run"))))
    (should (equal (nreverse order) '(first second)))
    (should (= (plist-get (clime-parse-result-params result) 'x) 1))
    (should (= (plist-get (clime-parse-result-params result) 'y) 2))))

(ert-deftest clime-test-conform/node-conform-list-threads-params ()
  "List of conform fns threads values: output of first is input to second."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (list (lambda (values _node)
                                                   (clime-values-set values 'x 10 'conform))
                                                 (lambda (values _node)
                                                   ;; Second fn sees x=10 from first
                                                   (clime-values-set values 'y
                                                                     (* 2 (clime-values-value values 'x))
                                                                     'conform)))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (result (clime-parse app '("run"))))
    (should (= (plist-get (clime-parse-result-params result) 'y) 20))))

;;; ─── Exclusive Check via Node Conform ────────────────────────────────

(ert-deftest clime-test-conform/exclusive-single-ok ()
  "Exclusive check via node :conform: one member set succeeds."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--json")))))
    (should (equal output "ok"))))

(ert-deftest clime-test-conform/exclusive-two-error ()
  "Exclusive check via node :conform: two members signals error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-conform/exclusive-none-ok ()
  "Exclusive check: zero members set is fine (not required)."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "ok"))))

(ert-deftest clime-test-conform/exclusive-derived-value ()
  "Exclusive check injects winner option name under group key."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv")))))
    (should (equal output "csv"))))

;;; ─── Exclusive: Default Value ───────────────────────────────────────

(ert-deftest clime-test-conform/exclusive-default-when-none-set ()
  "Exclusive :default injects value when no member is set."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv) 'text)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; No member set → default injected
    (let ((output (with-output-to-string
                    (clime-run app '("run")))))
      (should (equal output "text")))
    ;; Member set → derived value overrides default
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--json")))))
      (should (equal output "json")))))

(ert-deftest clime-test-conform/exclusive-default-suppressed-on-sibling ()
  "Setting one exclusive member suppresses defaults on siblings."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0 :default t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (let ((p (clime-context-params ctx)))
                                               (format "json=%s csv=%s"
                                                       (plist-get p 'json)
                                                       (plist-get p 'csv))))
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; Setting --json suppresses csv's default
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--json")))))
      (should (equal output "json=t csv=nil")))
    ;; No member set: csv default applies normally
    (let ((output (with-output-to-string
                    (clime-run app '("run")))))
      (should (equal output "json=nil csv=t")))))

(ert-deftest clime-test-conform/exclusive-no-default-no-injection ()
  "Without :default, no group key injected when no member set."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json)
                                  :conform (clime-check-exclusive 'fmt '(json))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "nil"))))

(ert-deftest clime-test-conform/dsl-mutex-default ()
  "clime-mutex DSL :default passes through to clime-check-exclusive."
  (eval
   '(clime-app clime-test--conform-mutex-default-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-mutex fmt :default 'text
          (clime-option json ("--json") :bool)
          (clime-option csv ("--csv") :bool))
        (clime-handler (ctx)
          (format "%s" (clime-ctx-get ctx 'fmt)))))
   t)
  (let* ((app (symbol-value 'clime-test--conform-mutex-default-app)))
    ;; No member → default
    (let ((output (with-output-to-string
                    (clime-run app '("run")))))
      (should (equal output "text")))
    ;; Member set → derived value
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--csv")))))
      (should (equal output "csv")))))

;;; ─── Exclusive :required ─────────────────────────────────────────────

(ert-deftest clime-test-conform/exclusive-required-none-set-error ()
  "Exclusive group with :required errors when no member is set."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv) nil t)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error (clime-parse app '("run")) :type 'clime-usage-error)))

(ert-deftest clime-test-conform/exclusive-required-one-set-ok ()
  "Exclusive group with :required passes when one member is set."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv) nil t)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should (equal (with-output-to-string (clime-run app '("run" "--json")))
                   "json"))))

(ert-deftest clime-test-conform/exclusive-required-error-message ()
  "Exclusive :required error lists all member names."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--aa") :nargs 0))
         (opt-b (clime-make-option :name 'b :flags '("--bb") :nargs 0))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt-a opt-b)
                                  :conform (clime-check-exclusive 'g '(a b) nil t)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (condition-case err
        (progn (clime-parse app '("run")) (ert-fail "Expected error"))
      (clime-usage-error
       (should (string-match-p "One of" (cadr err)))
       (should (string-match-p "a" (cadr err)))
       (should (string-match-p "b" (cadr err)))))))

(ert-deftest clime-test-conform/dsl-mutex-required ()
  "clime-mutex DSL :required passes through to clime-check-exclusive."
  (eval
   '(clime-app clime-test--conform-mutex-required-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-mutex fmt :required
          (clime-option json ("--json") :bool)
          (clime-option csv ("--csv") :bool))
        (clime-handler (ctx)
          (format "%s" (clime-ctx-get ctx 'fmt)))))
   t)
  (let ((app (symbol-value 'clime-test--conform-mutex-required-app)))
    ;; No member → error
    (should (= (clime-run app '("run")) 2))
    ;; Member set → ok
    (should (equal (with-output-to-string (clime-run app '("run" "--json")))
                   "json"))))

(ert-deftest clime-test-conform/exclusive-required-default-option-warns ()
  "Option inside mutex with :required t and :default triggers warning."
  (let ((msgs (clime-test-with-messages
                (clime-make-option :name 'csv :flags '("--csv")
                                    :nargs 0 :required t :default t))))
    (should (cl-some (lambda (m) (string-match-p ":required is vacuous" m))
                     msgs))))

(ert-deftest clime-test-conform/dsl-mutex-required-with-default-child ()
  "clime-mutex :required warns when a child option has :default.
Defaults apply after exclusivity check, so the child's default cannot
satisfy the at-least-one requirement — user must explicitly choose."
  ;; Construction warns about vacuous :default
  (let ((msgs (clime-test-with-messages
                (eval
                 '(clime-app clime-test--mutex-req-default-app
                    :version "1"
                    (clime-command run
                      :help "Run"
                      (clime-mutex fmt :required
                        (clime-option json ("--json") :bool)
                        (clime-option csv ("--csv") :bool :default t))
                      (clime-handler (ctx)
                        (format "%s" (clime-ctx-get ctx 'fmt)))))
                 t))))
    (should (cl-some (lambda (m) (string-match-p "vacuous" m)) msgs)))
  ;; Runtime: no explicit member → error despite csv having :default
  (let ((app (symbol-value 'clime-test--mutex-req-default-app)))
    (should (= (clime-run app '("run")) 2))
    ;; Explicit choice → ok
    (should (equal (with-output-to-string (clime-run app '("run" "--csv")))
                   "csv"))))

(ert-deftest clime-test-conform/dsl-mutex-required-default-warns ()
  "clime-mutex with both :required and :default warns — :default is vacuous."
  (let ((msgs (clime-test-with-messages
                (eval
                 '(clime-app clime-test--mutex-req-def-app
                    :version "1"
                    (clime-command run
                      :help "Run"
                      (clime-mutex fmt :required :default 'json
                        (clime-option json ("--json") :bool)
                        (clime-option csv ("--csv") :bool))
                      (clime-handler (ctx)
                        (format "%s" (clime-ctx-get ctx 'fmt)))))
                 t))))
    (should (cl-some (lambda (m) (string-match-p ":default is vacuous" m)) msgs))
    ;; :required still enforced at runtime
    (let ((app (symbol-value 'clime-test--mutex-req-def-app)))
      (should (= (clime-run app '("run")) 2))
      (should (equal (with-output-to-string (clime-run app '("run" "--json")))
                     "json")))))

(ert-deftest clime-test-conform/dsl-zip-required ()
  "clime-zip :required errors when no members are provided."
  (eval
   '(clime-app clime-test--zip-req-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-zip sr :required
          (clime-option skip ("--skip") :multiple)
          (clime-option reason ("--reason") :multiple))
        (clime-handler (ctx)
          (format "%S" (clime-ctx-get ctx 'sr)))))
   t)
  (let ((app (symbol-value 'clime-test--zip-req-app)))
    ;; No members → error
    (should (= (clime-run app '("run")) 2))
    ;; All members → ok
    (should (string-match-p "skip"
              (with-output-to-string
                (clime-run app '("run" "--skip" "x" "--reason" "y")))))))

;;; ─── Paired Check via Node Conform ───────────────────────────────────

(ert-deftest clime-test-conform/paired-two-options-zipped ()
  "Paired check via node :conform zips member values."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'sr)))
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "A" "--reason" "x"
                                    "--skip" "B" "--reason" "y")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 2))
      (should (equal (alist-get 'skip (nth 0 result)) "A"))
      (should (equal (alist-get 'reason (nth 0 result)) "x")))))

(ert-deftest clime-test-conform/paired-partial-error ()
  "Paired check: partial presence signals error."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "A"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-conform/paired-cardinality-error ()
  "Paired check: unequal cardinality signals error."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "A" "--skip" "B" "--reason" "x"))
     :type 'clime-usage-error)))

;;; ─── Multiple Conform Groups on Same Node ────────────────────────────

(ert-deftest clime-test-conform/two-groups-same-command ()
  "Two independent exclusive groups on one command via conform list."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (opt-local (clime-make-option :name 'local :flags '("--local") :nargs 0))
         (opt-remote (clime-make-option :name 'remote :flags '("--remote") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s/%s"
                                                     (clime-ctx-get ctx 'fmt)
                                                     (clime-ctx-get ctx 'src)))
                                  :options (list opt-json opt-csv opt-local opt-remote)
                                  :conform (list
                                            (clime-check-exclusive 'fmt '(json csv))
                                            (clime-check-exclusive 'src '(local remote)))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv" "--remote")))))
    (should (equal output "csv/remote"))))

(ert-deftest clime-test-conform/two-groups-independent-errors ()
  "Violation in one group doesn't mask violations in another."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (opt-local (clime-make-option :name 'local :flags '("--local") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv opt-local)
                                  :conform (list
                                            (clime-check-exclusive 'fmt '(json csv))
                                            (clime-check-exclusive 'src '(local)))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; fmt group violation
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)
    ;; src group: single member is fine
    (let ((exit-code (clime-run app '("run" "--local"))))
      (should (= exit-code 0)))))

;;; ─── App-Level Conform ───────────────────────────────────────────────

(ert-deftest clime-test-conform/app-level-conform ()
  "App-level :conform runs after command-level (leaf→root walk)."
  (let* ((opt-v (clime-make-option :name 'verbose :flags '("--verbose") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (ctx)
                                                          (format "%s" (clime-ctx-get ctx 'log-level)))
                                  :conform (lambda (values _node)
                                             ;; Command conform runs first
                                             (clime-values-set values 'cmd-seen t 'conform))))
         (app (clime-make-app :name "t" :version "1"
                               :options (list opt-v)
                               :children (list (cons "run" cmd))
                               :conform (lambda (values _node)
                                           ;; App conform runs second, can see cmd-seen
                                           (clime-values-set values 'log-level
                                                             (if (clime-values-value values 'cmd-seen)
                                                                 "debug" "info")
                                                             'conform)))))
    (let ((result (clime-parse app '("run"))))
      (should (equal (plist-get (clime-parse-result-params result) 'log-level)
                     "debug")))))

;;; ─── Node :conform Slot on Struct ────────────────────────────────────

(ert-deftest clime-test-conform/command-has-conform-slot ()
  "clime-command accepts :conform in constructor."
  (let ((cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (lambda (p _n) p))))
    (should (functionp (clime-node-conform cmd)))))

(ert-deftest clime-test-conform/group-has-conform-slot ()
  "clime-group accepts :conform in constructor."
  (let ((grp (clime-make-group :name "agents"
                                :conform (lambda (p _n) p))))
    (should (functionp (clime-node-conform grp)))))

(ert-deftest clime-test-conform/app-has-conform-slot ()
  "clime-app accepts :conform in constructor."
  (let ((app (clime-make-app :name "t" :version "1"
                              :conform (lambda (p _n) p))))
    (should (functionp (clime-node-conform app)))))

(ert-deftest clime-test-conform/node-conform-nil-by-default ()
  "Node :conform defaults to nil."
  (let ((cmd (clime-make-command :name "run" :handler #'ignore)))
    (should-not (clime-node-conform cmd))))

;;; ─── Exclusive: Error Message ───────────────────────────────────────

(ert-deftest clime-test-conform/exclusive-error-lists-flags ()
  "Exclusive error message lists conflicting options."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (err (should-error
               (clime-parse app '("run" "--json" "--csv"))
               :type 'clime-usage-error)))
    (should (string-match-p "json" (cadr err)))
    (should (string-match-p "csv" (cadr err)))))

;;; ─── Exclusive: Derived Value Nil ──────────────────────────────────

(ert-deftest clime-test-conform/exclusive-derived-nil-when-none ()
  "Exclusive: no member set → group key absent from params."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json)
                                  :conform (clime-check-exclusive 'fmt '(json))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "nil"))))

;;; ─── Exclusive: Value-Taking Options ───────────────────────────────

(ert-deftest clime-test-conform/exclusive-value-taking-options ()
  "Exclusive group works with value-taking (non-boolean) options."
  (let* ((opt-file (clime-make-option :name 'file :flags '("--file")))
         (opt-url (clime-make-option :name 'url :flags '("--url")))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s:%s"
                                                     (clime-ctx-get ctx 'out)
                                                     (or (clime-ctx-get ctx 'file)
                                                         (clime-ctx-get ctx 'url))))
                                  :options (list opt-file opt-url)
                                  :conform (clime-check-exclusive 'out '(file url))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--file" "/tmp/x")))))
      (should (equal output "file:/tmp/x")))
    (should-error
     (clime-parse app '("run" "--file" "/tmp/x" "--url" "http://y"))
     :type 'clime-usage-error)))

;;; ─── Exclusive: Three Options ──────────────────────────────────────

(ert-deftest clime-test-conform/exclusive-three-options-error ()
  "Three options from same exclusive group signals error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (opt-text (clime-make-option :name 'text :flags '("--text") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv opt-text)
                                  :conform (clime-check-exclusive 'fmt '(json csv text))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv" "--text"))
     :type 'clime-usage-error)))

;;; ─── Exclusive: Exit Code ──────────────────────────────────────────

(ert-deftest clime-test-conform/exclusive-exit-code-2 ()
  "Exclusive violation through clime-run returns exit code 2."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--json" "--csv")))))
    (should (= exit-code 2))))

;;; ─── Exclusive: Env Var Conflict ───────────────────────────────────

(ert-deftest clime-test-conform/exclusive-env-var-conflict ()
  "Exclusive violation via env vars signals usage error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0 :env t))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0 :env t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_EX"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_EX_JSON=1" "TEST_EX_CSV=1")
                                       process-environment)))
      (should-error
       (clime-parse app '("run"))
       :type 'clime-usage-error))))

(ert-deftest clime-test-conform/exclusive-cli-plus-env-conflict ()
  "Exclusive violation from CLI + env var combination signals error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0 :env t))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0 :env t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :conform (clime-check-exclusive 'fmt '(json csv))))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_EX"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_EX_CSV=1")
                                       process-environment)))
      (should-error
       (clime-parse app '("run" "--json"))
       :type 'clime-usage-error))))

;;; ─── Paired: All Absent OK ─────────────────────────────────────────

(ert-deftest clime-test-conform/paired-all-absent-ok ()
  "Paired check: all absent is fine."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "ok"))))

;;; ─── Paired: Three Options ─────────────────────────────────────────

(ert-deftest clime-test-conform/paired-three-options ()
  "Paired check works with 3+ options."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--aa") :multiple t))
         (opt-b (clime-make-option :name 'b :flags '("--bb") :multiple t))
         (opt-c (clime-make-option :name 'c :flags '("--cc") :multiple t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'trio)))
                                  :options (list opt-a opt-b opt-c)
                                  :conform (clime-check-paired 'trio '(a b c))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--aa" "1" "--bb" "x" "--cc" "p"
                                    "--aa" "2" "--bb" "y" "--cc" "q")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 2))
      (should (equal (alist-get 'a (nth 0 result)) "1"))
      (should (equal (alist-get 'b (nth 0 result)) "x"))
      (should (equal (alist-get 'c (nth 0 result)) "p")))))

;;; ─── Paired: Exit Code ─────────────────────────────────────────────

(ert-deftest clime-test-conform/paired-exit-code-2 ()
  "Paired violation through clime-run returns exit code 2."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--skip" "A" "--skip" "B" "--reason" "one")))))
    (should (= exit-code 2))))

;;; ─── Paired: Individual Lists Preserved ────────────────────────────

(ert-deftest clime-test-conform/paired-individual-lists-preserved ()
  "Paired: individual option lists are still available."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S %S"
                                                     (clime-ctx-get ctx 'skip)
                                                     (clime-ctx-get ctx 'reason)))
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "SPEC" "--reason" "no spec")))))
    (should (equal output "(\"SPEC\") (\"no spec\")"))))

;;; ─── Paired: Env Var Participates ──────────────────────────────────

(ert-deftest clime-test-conform/paired-env-var-participates ()
  "Env vars contribute to paired groups."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :multiple t :env t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :multiple t :env t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'sr)))
                                  :options (list opt-skip opt-reason)
                                  :conform (clime-check-paired 'sr '(skip reason))))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_ZIP"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_ZIP_REASON=from-env")
                                       process-environment)))
      (let ((output (with-output-to-string
                      (clime-run app '("run" "--skip" "SPEC")))))
        (let ((result (car (read-from-string output))))
          (should (= (length result) 1))
          (should (equal (alist-get 'skip (nth 0 result)) "SPEC"))
          (should (equal (alist-get 'reason (nth 0 result)) "from-env")))))))

;;; ─── Ancestor Conform ─────────────────────────────────────────────

(ert-deftest clime-test-conform/ancestor-exclusive ()
  "Exclusive conform on app applies to command-level options."
  (let* ((app-opt (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (cmd-opt (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                               :options (list app-opt)
                               :conform (clime-check-exclusive 'fmt '(json csv))
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv")))))
    (should (equal output "ok"))))

(ert-deftest clime-test-conform/ancestor-exclusive-conflict ()
  "Exclusive conform on app catches conflict across app+command options."
  (let* ((app-opt (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (cmd-opt (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                               :options (list app-opt)
                               :conform (clime-check-exclusive 'fmt '(json csv))
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)))

;;; ─── Inline Group Conform ─────────────────────────────────────────

(ert-deftest clime-test-conform/inline-group-ancestor-paired ()
  "Inline group as ancestor: :conform on inline group works for dispatch path."
  (let* ((opt-from (clime-make-option :name 'from :flags '("--from") :multiple t))
         (opt-to (clime-make-option :name 'to :flags '("--to") :multiple t))
         (cmd (clime-make-command :name "run" :handler
                (lambda (ctx) (format "%S" (clime-ctx-get ctx 'range)))))
         (inner-grp (clime-make-group :name "range" :inline t
                                       :options (list opt-from opt-to)
                                       :conform (clime-check-paired 'range '(from to))
                                       :children (list (cons "run" cmd))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "range" inner-grp)))))
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--from" "A" "--to" "B")))))
      (let ((result (car (read-from-string output))))
        (should (= (length result) 1))
        (should (equal (alist-get 'from (nth 0 result)) "A"))
        (should (equal (alist-get 'to (nth 0 result)) "B"))))))

(ert-deftest clime-test-conform/inline-group-sibling-conform-runs ()
  "Inline group as sibling (not on dispatch path): :conform still runs."
  (let* ((conform-ran nil)
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")))
         (sibling-grp (clime-make-group :name "meta" :inline t
                                         :conform (lambda (values _node)
                                                    (setq conform-ran t)
                                                    (clime-values-set values 'meta-seen t 'conform))))
         (parent-grp (clime-make-group :name "cmds"
                                        :children (list (cons "meta" sibling-grp)
                                                        (cons "run" cmd))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "cmds" parent-grp)))))
    (with-output-to-string (clime-run app '("cmds" "run")))
    (should conform-ran)))

;;; ─── DSL: clime-mutex ──────────────────────────────────────────────

(ert-deftest clime-test-conform/dsl-mutex ()
  "clime-mutex DSL wraps options into exclusive group via :conform."
  (eval
   '(clime-app clime-test--conform-mutex-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-mutex output-format
          (clime-option fmt-json ("--json") :bool)
          (clime-option fmt-csv ("--csv") :bool))
        (clime-handler (ctx)
          (format "%s" (clime-ctx-get ctx 'output-format)))))
   t)
  (let* ((app (symbol-value 'clime-test--conform-mutex-app)))
    ;; Should succeed and derive value
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--json")))))
      (should (equal output "fmt-json")))
    ;; Should error with both
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-conform/dsl-mutex-promotes-options ()
  "clime-mutex promotes wrapped options to enclosing node's :options."
  (eval
   '(clime-app clime-test--conform-mutex-opts-app
      :version "1"
      (clime-mutex fmt
        (clime-option json ("--json") :bool)
        (clime-option csv ("--csv") :bool))
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--conform-mutex-opts-app)))
    (should (clime-node-find-option app "--json"))
    (should (clime-node-find-option app "--csv"))))

;;; ─── DSL: clime-zip ───────────────────────────────────────────────

(ert-deftest clime-test-conform/dsl-zip ()
  "clime-zip DSL wraps options into paired group via :conform."
  (eval
   '(clime-app clime-test--conform-zip-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-zip skip-reason
          (clime-option skip ("--skip"))
          (clime-option reason ("--reason")))
        (clime-handler (ctx)
          (format "%S" (clime-ctx-get ctx 'skip-reason)))))
   t)
  (let* ((app (symbol-value 'clime-test--conform-zip-app))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "A" "--reason" "x")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 1))
      (should (equal (alist-get 'skip (nth 0 result)) "A"))
      (should (equal (alist-get 'reason (nth 0 result)) "x")))))

;;; ─── Param Base Struct ─────────────────────────────────────────────

(ert-deftest clime-test-conform/param-not-constructible ()
  "clime-param has no public constructor (abstract base)."
  (should-not (fboundp 'clime-make-param))
  (should-not (fboundp 'clime-param--create)))

(ert-deftest clime-test-conform/param-accessors-on-option ()
  "clime-param-name and clime-param-required work on clime-option."
  (let ((opt (clime-make-option :name 'verbose :flags '("--verbose")
                                 :required t :help "Be loud")))
    (should (eq (clime-param-name opt) 'verbose))
    (should (eq (clime-param-required opt) t))
    (should (equal (clime-param-help opt) "Be loud"))
    (should (eq (clime-option-name opt) 'verbose))))

(ert-deftest clime-test-conform/param-accessors-on-arg ()
  "clime-param-name and clime-param-required work on clime-arg."
  (let ((arg (clime-make-arg :name 'file :required t :help "Input file")))
    (should (eq (clime-param-name arg) 'file))
    (should (eq (clime-param-required arg) t))
    (should (equal (clime-param-help arg) "Input file"))
    (should (eq (clime-arg-name arg) 'file))))

(ert-deftest clime-test-conform/param-predicate ()
  "clime-param-p recognizes options and args."
  (should (clime-param-p (clime-make-option :name 'v :flags '("-v"))))
  (should (clime-param-p (clime-make-arg :name 'f))))

;;; ─── Negatable + Exclusive Interaction ──────────────────────────────

(ert-deftest clime-test-conform/exclusive-negatable-suppressed-by-winner ()
  "Negatable option suppressed to nil when sibling wins exclusive group."
  (let* ((opt-color (clime-make-option :name 'color :flags '("--color")
                                        :negatable t :default t))
         (opt-plain (clime-make-option :name 'plain :flags '("--plain") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "color=%s plain=%s display=%s"
                                                     (clime-param ctx 'color 'auto)
                                                     (clime-ctx-get ctx 'plain)
                                                     (clime-ctx-get ctx 'display)))
                                  :options (list opt-color opt-plain)
                                  :conform (clime-check-exclusive 'display '(color plain))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; --plain wins: color suppressed to nil, default skipped
    (should (equal (with-output-to-string (clime-run app '("run" "--plain")))
                   "color=nil plain=t display=plain"))
    ;; nothing set: color gets default t, no winner
    (should (equal (with-output-to-string (clime-run app '("run")))
                   "color=t plain=nil display=nil"))
    ;; --color explicit: color wins
    (should (equal (with-output-to-string (clime-run app '("run" "--color")))
                   "color=t plain=nil display=color"))
    ;; --no-color: explicitly set but not a winner — no derived key
    (should (equal (with-output-to-string (clime-run app '("run" "--no-color")))
                   "color=nil plain=nil display=nil"))))

(ert-deftest clime-test-conform/exclusive-negatable-default-nil-absent ()
  "Negatable option with :default nil stays absent (no special interaction)."
  (let* ((opt-color (clime-make-option :name 'color :flags '("--color")
                                        :negatable t :default nil))
         (opt-plain (clime-make-option :name 'plain :flags '("--plain") :nargs 0))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "color=%s"
                                                     (clime-param ctx 'color 'auto)))
                                  :options (list opt-color opt-plain)
                                  :conform (clime-check-exclusive 'display '(color plain))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; :default nil is no-op — color stays absent, clime-param returns fallback
    (should (equal (with-output-to-string (clime-run app '("run")))
                   "color=auto"))))

;;; ─── Edge Cases ─────────────────────────────────────────────────────

(ert-deftest clime-test-conform/option-conform-on-env-var-value ()
  "Option :conform runs on values sourced from environment variables."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                  :env "TEST_PORT_CONFORM"
                                  :conform (lambda (v _p) (string-to-number v))))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'port)))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (clime-test-with-env (("TEST_PORT_CONFORM" "8080"))
      (should (equal (with-output-to-string (clime-run app '("run")))
                     "8080")))))

(ert-deftest clime-test-conform/node-conform-sees-option-conform-result ()
  "Node :conform receives the already-conformed option value."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                  :conform (lambda (v _p) (string-to-number v))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)
                                  :conform (lambda (values _node)
                                             ;; Should see the number, not the string
                                             (should (numberp (clime-values-value values 'port)))
                                             values)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (clime-parse app '("run" "--port" "3000"))))

(ert-deftest clime-test-conform/arg-conform-feeds-node-conform ()
  "Arg :conform result is visible to node :conform on same command."
  (let* ((arg (clime-make-arg :name 'count
                               :conform (lambda (v _p) (string-to-number v))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :args (list arg)
                                  :conform (lambda (values _node)
                                             (should (numberp (clime-values-value values 'count)))
                                             values)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (clime-parse app '("run" "42"))))

(ert-deftest clime-test-conform/option-conform-returning-nil ()
  "Option :conform returning nil sets param to nil (present but falsy)."
  (let* ((opt (clime-make-option :name 'x :flags '("--xx")
                                  :conform (lambda (_v _p) nil)))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (result (clime-parse app '("run" "--xx" "val")))
         (params (clime-parse-result-params result)))
    ;; Key is present (plist-member) but value is nil
    (should (plist-member params 'x))
    (should (null (plist-get params 'x)))))

(ert-deftest clime-test-conform/node-conform-usage-error-re-signaled ()
  "clime-usage-error from node :conform is re-signaled, not double-wrapped."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore
                                  :conform (lambda (_params _node)
                                             (signal 'clime-usage-error
                                                     '("custom message")))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (condition-case err
        (progn (clime-parse app '("run")) (ert-fail "Expected error"))
      (clime-usage-error
       ;; Should be the original message, not wrapped in "Invalid value..."
       (should (equal (cadr err) "custom message"))))))

(ert-deftest clime-test-conform/group-handler-with-conform ()
  "Node :conform runs on a group with a handler (invoked group)."
  (let* ((grp (clime-make-group :name "config" :inline nil
                                 :handler (lambda (ctx)
                                            (format "val=%s"
                                                    (clime-ctx-get ctx 'derived)))
                                 :conform (lambda (values _node)
                                            (clime-values-set values 'derived "injected" 'conform))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "config" grp)))))
    (should (equal (with-output-to-string (clime-run app '("config")))
                   "val=injected"))))

;;; ─── Inline Group Structure (clime-swl) ────────────────────────────

(ert-deftest clime-test-conform/mutex-expands-to-inline-group ()
  "clime-mutex creates an inline group child with conformer attached."
  (eval
   '(clime-app clime-test--mutex-inline-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-mutex output-format
          (clime-option fmt-json ("--json") :bool)
          (clime-option fmt-csv ("--csv") :bool))
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--mutex-inline-app))
         (cmd (cdr (assoc "run" (clime-group-children app))))
         ;; The command itself should NOT have a :conform (it moved to the group)
         (cmd-conform (clime-node-conform cmd)))
    ;; Command should have an inline group child
    (should (clime-group-p cmd))
    (let* ((children (clime-group-children cmd))
           (grp-entry (assoc "output-format" children))
           (grp (cdr grp-entry)))
      (should grp-entry)
      (should (clime-group-p grp))
      (should (clime-node-inline grp))
      ;; Conformer lives on the inline group, not the command
      (should (clime-node-conform grp))
      (should-not cmd-conform)
      ;; Options live on the inline group
      (should (= 2 (length (clime-node-options grp))))
      ;; But options are still findable from the command (via inline promotion)
      (should (clime-node-find-option cmd "--json"))
      (should (clime-node-find-option cmd "--csv")))))

(ert-deftest clime-test-conform/zip-expands-to-inline-group ()
  "clime-zip creates an inline group child with conformer attached."
  (eval
   '(clime-app clime-test--zip-inline-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-zip skip-reason
          (clime-option skip ("--skip"))
          (clime-option reason ("--reason")))
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--zip-inline-app))
         (cmd (cdr (assoc "run" (clime-group-children app))))
         (cmd-conform (clime-node-conform cmd)))
    (should (clime-group-p cmd))
    (let* ((children (clime-group-children cmd))
           (grp-entry (assoc "skip-reason" children))
           (grp (cdr grp-entry)))
      (should grp-entry)
      (should (clime-group-p grp))
      (should (clime-node-inline grp))
      (should (clime-node-conform grp))
      (should-not cmd-conform)
      (should (= 2 (length (clime-node-options grp)))))))

(ert-deftest clime-test-conform/mutex-inline-group-at-app-level ()
  "clime-mutex at app level creates inline group child on app."
  (eval
   '(clime-app clime-test--mutex-app-level-app
      :version "1"
      (clime-mutex fmt
        (clime-option json ("--json") :bool)
        (clime-option csv ("--csv") :bool))
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--mutex-app-level-app))
         (grp-entry (assoc "fmt" (clime-group-children app)))
         (grp (cdr grp-entry)))
    (should grp-entry)
    (should (clime-group-p grp))
    (should (clime-node-inline grp))
    ;; Options promoted through inline group
    (should (clime-node-find-option app "--json"))
    (should (clime-node-find-option app "--csv"))))

(ert-deftest clime-test-conform/exclusive-members-property-removed ()
  "clime-check-exclusive no longer sets clime-exclusive-members property."
  (let ((conformer (clime-check-exclusive 'fmt '(json csv))))
    (should-not (get conformer 'clime-exclusive-members))))

(ert-deftest clime-test-conform/all-options-collects-inline-group-options ()
  "clime-node-all-options returns own + inline group child options."
  (let* ((opt-a (clime-make-option :name 'aaa :flags '("--aaa") :nargs 0))
         (opt-b (clime-make-option :name 'bbb :flags '("--bbb") :nargs 0))
         (opt-c (clime-make-option :name 'ccc :flags '("--ccc") :nargs 0))
         (grp (clime-group--create :name "grp" :inline t
                                   :options (list opt-b opt-c)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-a)
                                  :children (list (cons "grp" grp)))))
    (let ((all (clime-node-all-options cmd)))
      (should (= 3 (length all)))
      (should (memq opt-a all))
      (should (memq opt-b all))
      (should (memq opt-c all)))))

(ert-deftest clime-test-conform/all-options-no-inline-children ()
  "clime-node-all-options with no inline children returns own options."
  (let* ((opt (clime-make-option :name 'foo :flags '("--foo") :nargs 0))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt))))
    (should (equal (list opt) (clime-node-all-options cmd)))))

;;; ─── Arity-1 Conform (clime-yo0) ─────────────────────────────────────

(ert-deftest clime-test-conform/option-1-arg-lambda ()
  "Option :conform with 1-arg lambda works without param."
  (let* ((opt (clime-make-option :name 'status :flags '("--status")
                                  :conform (lambda (v) (upcase v))))
         (cmd (clime-make-command :name "list" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "list" cmd))))
         (result (clime-parse app '("list" "--status" "todo"))))
    (should (equal (plist-get (clime-parse-result-params result) 'status)
                   "TODO"))))

(ert-deftest clime-test-conform/option-1-arg-builtin ()
  "Option :conform with arity-1 builtin function works."
  (let* ((opt (clime-make-option :name 'name :flags '("--name")
                                  :conform #'upcase))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (result (clime-parse app '("run" "--name" "hello"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "HELLO"))))

(ert-deftest clime-test-conform/option-identity-through-parse ()
  "Option :conform #'identity works through full parse (1-arg fn)."
  (let* ((opt (clime-make-option :name 'val :flags '("--val")
                                  :conform #'identity))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (result (clime-parse app '("run" "--val" "keep"))))
    (should (equal (plist-get (clime-parse-result-params result) 'val)
                   "keep"))))

(ert-deftest clime-test-conform/arg-1-arg-lambda ()
  "Arg :conform with 1-arg lambda works."
  (let* ((arg (clime-make-arg :name 'name :required t
                               :conform (lambda (v) (downcase v))))
         (cmd (clime-make-command :name "greet" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "greet" cmd))))
         (result (clime-parse app '("greet" "WORLD"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "world"))))

(ert-deftest clime-test-conform/option-1-arg-error-signals-usage-error ()
  "1-arg conformer that signals error produces clime-usage-error."
  (let* ((opt (clime-make-option :name 'id :flags '("--id")
                                  :conform (lambda (v)
                                             (unless (string-match-p "^[0-9]+$" v)
                                               (error "Must be numeric"))
                                             v)))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--id" "abc"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-conform/option-rest-arg-receives-2-args ()
  "Conform fn with &rest receives both value and param."
  (let* ((received-args nil)
         (opt (clime-make-option :name 'x :flags '("--xx")
                                  :conform (lambda (&rest args)
                                             (setq received-args args)
                                             (car args))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (clime-parse app '("run" "--xx" "val"))
    ;; &rest fn gets 2 args (value + param)
    (should (= 2 (length received-args)))
    (should (equal "val" (car received-args)))
    (should (clime-option-p (cadr received-args)))))

;;; ─── Values-Map Conformer Specs (clime-7je) ─────────────────────────

(ert-deftest clime-test-conform/run-conformers-reads-values-map ()
  "clime--run-conformers reads value from values map, not struct."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                  :conform (lambda (val _param)
                                             (if (< val 1024)
                                                 (error "Port too low")
                                               val))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (values (list (list 'port :value 8080 :source 'user))))
    ;; Call with nodes and values map
    (let ((result (clime--run-conformers (list cmd) values)))
      ;; Conformed value in returned values map
      (should (= 8080 (clime-values-value result 'port))))))

(ert-deftest clime-test-conform/run-conformers-writes-values-map ()
  "clime--run-conformers writes conformed value to values map, returns updated map."
  (let* ((opt (clime-make-option :name 'name :flags '("--name")
                                  :conform #'upcase))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (values (list (list 'name :value "alice" :source 'user))))
    (let ((result (clime--run-conformers (list cmd) values)))
      ;; Values map updated with conformed value
      (should (equal "ALICE" (clime-values-value result 'name)))
      ;; Source unchanged
      (should (eq 'user (clime-values-source result 'name))))))

(ert-deftest clime-test-conform/node-conform-receives-values-and-node ()
  "Node :conform receives (values, node), returns updated values."
  (let* ((received-args nil)
         (cmd (clime-make-command
               :name "run" :handler #'ignore
               :conform (lambda (values node)
                          (setq received-args (list values node))
                          values)))
         (values '()))
    (clime--apply-node-conform cmd values)
    (should (= 2 (length received-args)))
    (should (listp (car received-args)))
    (should (clime-command-p (cadr received-args)))))

(ert-deftest clime-test-conform/exclusive-writes-values-map ()
  "check-exclusive writes group-name entry to values map."
  (let* ((values (list (list 'json :value t :source 'user)))
         (grp (clime-make-group :name "format" :inline t
                                 :options (list (clime-make-option :name 'json :flags '("--json") :nargs 0)
                                               (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
                                 :conform (clime-check-exclusive 'format '(json csv)))))
    (let ((result (clime--apply-node-conform grp values)))
      ;; Winner written under group-name key with source 'conform
      (should (eq 'json (clime-values-value result 'format)))
      (should (eq 'conform (clime-values-source result 'format))))))

(ert-deftest clime-test-conform/paired-writes-values-map ()
  "check-paired writes zipped alist to values map."
  (let* ((values (list (list 'skip :value '("a") :source 'user)
                       (list 'reason :value '("why") :source 'user)))
         (grp (clime-make-group :name "pairs" :inline t
                                 :options (list (clime-make-option :name 'skip :flags '("--skip") :multiple t)
                                               (clime-make-option :name 'reason :flags '("--reason") :multiple t))
                                 :conform (clime-check-paired 'pairs '(skip reason)))))
    (let ((result (clime--apply-node-conform grp values)))
      (let ((pairs (clime-values-value result 'pairs)))
        (should (= 1 (length pairs)))
        (should (equal '((skip . "a") (reason . "why")) (car pairs)))
        (should (eq 'conform (clime-values-source result 'pairs)))))))

(ert-deftest clime-test-conform/finalize-produces-values-map ()
  "parse-finalize populates :values slot on parse-result."
  (let* ((opt (clime-make-option :name 'port :flags '("--port") :type 'integer
                                  :conform (lambda (val _p) (1+ val))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "run" cmd))))
         (result (clime-parse app '("run" "--port" "80"))))
    ;; Values map has conformed value
    (let ((values (clime-parse-result-values result)))
      (should (= 81 (clime-values-value values 'port)))
      (should (eq 'user (clime-values-source values 'port))))
    ;; Params plist also has it (derived for backward compat)
    (should (= 81 (plist-get (clime-parse-result-params result) 'port)))))

(provide 'clime-conform-tests)
;;; clime-conform-tests.el ends here
