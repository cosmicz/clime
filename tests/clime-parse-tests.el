;;; clime-parse-tests.el --- Tests for clime-parse  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the single-pass CLI argument parser.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-test-helpers)

;;; ─── Test Fixtures ──────────────────────────────────────────────────────

(defun clime-test--simple-app ()
  "Build a simple app with one command for testing."
  (let* ((opt-v (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                   :count t))
         (opt-json (clime-make-option :name 'json :flags '("--json")
                                      :nargs 0))
         (cmd-opt (clime-make-option :name 'format :flags '("--format" "-f")))
         (cmd-arg (clime-make-arg :name 'id))
         (cmd (clime-make-command :name "show"
                                  :aliases '("get")
                                  :handler #'ignore
                                  :options (list cmd-opt)
                                  :args (list cmd-arg))))
    (clime-make-app :name "myapp"
                    :version "1.0.0"
                    :options (list opt-v opt-json)
                    :children (list (cons "show" cmd)))))

(defun clime-test--group-app ()
  "Build an app with a group containing subcommands."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                      :count t))
         (add-arg (clime-make-arg :name 'id))
         (add-opt (clime-make-option :name 'blocking :flags '("--blocking")
                                     :nargs 0))
         (cmd-add (clime-make-command :name "add"
                                      :handler #'ignore
                                      :args (list add-arg)
                                      :options (list add-opt)))
         (cmd-rm (clime-make-command :name "remove"
                                     :aliases '("rm")
                                     :handler #'ignore
                                     :args (list (clime-make-arg :name 'id))))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd-add)
                                                (cons "remove" cmd-rm)))))
    (clime-make-app :name "myapp"
                    :version "1.0.0"
                    :options (list root-opt)
                    :children (list (cons "dep" grp)))))

(defun clime-test--multi-arg-app ()
  "Build an app with a command that takes :rest args."
  (let* ((arg-ids (clime-make-arg :name 'ids :nargs :rest :required nil))
         (cmd (clime-make-command :name "show"
                                  :handler #'ignore
                                  :args (list arg-ids))))
    (clime-make-app :name "myapp" :version "1"
                    :children (list (cons "show" cmd)))))

(defun clime-test--multi-opt-app ()
  "Build an app with a :multiple option."
  (let* ((tag-opt (clime-make-option :name 'tag :flags '("--tag" "-t")
                                     :multiple t))
         (cmd (clime-make-command :name "create"
                                  :handler #'ignore
                                  :options (list tag-opt))))
    (clime-make-app :name "myapp" :version "1"
                    :children (list (cons "create" cmd)))))

(defun clime-test--group-with-args-app ()
  "Build an app with a group that has required positional args before subcommands."
  (let* ((grp-arg (clime-make-arg :name 'scope))
         (cmd (clime-make-command :name "list" :handler #'ignore))
         (grp (clime-make-group :name "config"
                                :args (list grp-arg)
                                :children (list (cons "list" cmd)))))
    (clime-make-app :name "myapp" :version "1"
                    :children (list (cons "config" grp)))))

;;; ─── Parse Result Struct ────────────────────────────────────────────────

(ert-deftest clime-test-parse-result/struct ()
  "Parse result struct exists with expected slots."
  (let ((app (clime-test--simple-app)))
    (let ((result (clime-parse app '("show" "123"))))
      (should (clime-parse-result-p result))
      (should (clime-parse-result-command result))
      (should (clime-parse-result-path result))
      (should (clime-parse-result-params result)))))

;;; ─── Long Options ───────────────────────────────────────────────────────

(ert-deftest clime-test-parse/long-option-space ()
  "Parse --name value form."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("show" "--format" "json" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "json"))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "123"))))

(ert-deftest clime-test-parse/long-option-equals ()
  "Parse --name=value form."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("show" "--format=json" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "json"))))

;;; ─── Short Options ──────────────────────────────────────────────────────

(ert-deftest clime-test-parse/short-option ()
  "Parse -n value form."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("show" "-f" "json" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "json"))))

;;; ─── Short Bundles ──────────────────────────────────────────────────────

(ert-deftest clime-test-parse/short-bundle ()
  "Parse -vvv as count=3 via boolean bundle expansion."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("-vvv" "show" "123"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 3))))

;;; ─── Boolean / Count Flags ──────────────────────────────────────────────

(ert-deftest clime-test-parse/boolean-flag ()
  "Boolean flag consumes no value."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("--json" "show" "123"))))
    (should (eq (plist-get (clime-parse-result-params result) 'json) t))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "123"))))

(ert-deftest clime-test-parse/count-flag ()
  "Count flag increments with each occurrence."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("-v" "-v" "show" "123"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 2))))

;;; ─── Multiple Options ───────────────────────────────────────────────────

(ert-deftest clime-test-parse/multiple-option ()
  "Repeated :multiple option collects values into a list."
  (let* ((app (clime-test--multi-opt-app))
         (result (clime-parse app '("create" "--tag" "a" "--tag" "b"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("a" "b")))))

(ert-deftest clime-test-parse/multiple-option-short ()
  "Repeated :multiple option with short flag."
  (let* ((app (clime-test--multi-opt-app))
         (result (clime-parse app '("create" "-t" "a" "-t" "b"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("a" "b")))))

;;; ─── Positional Args ────────────────────────────────────────────────────

(ert-deftest clime-test-parse/positional-arg ()
  "Positional arg consumed by position."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("show" "abc123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "abc123"))))

(ert-deftest clime-test-parse/rest-args ()
  "Positional :rest arg collects multiple values."
  (let* ((app (clime-test--multi-arg-app))
         (result (clime-parse app '("show" "a" "b" "c"))))
    (should (equal (plist-get (clime-parse-result-params result) 'ids)
                   '("a" "b" "c")))))

(ert-deftest clime-test-parse/excess-positional-error ()
  "Excess positional when no :rest arg signals usage error."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("show" "123" "extra"))
                  :type 'clime-usage-error)))

;;; ─── End of Options (--) ────────────────────────────────────────────────

(ert-deftest clime-test-parse/double-dash ()
  "-- stops option parsing; subsequent --flag is positional."
  (let* ((app (clime-test--multi-arg-app))
         (result (clime-parse app '("show" "--" "--not-a-flag"))))
    (should (equal (plist-get (clime-parse-result-params result) 'ids)
                   '("--not-a-flag")))))

;;; ─── Root Global Options ────────────────────────────────────────────────

(ert-deftest clime-test-parse/root-option-before-command ()
  "Root option before command name."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("--verbose" "show" "123"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))))

(ert-deftest clime-test-parse/root-option-after-command ()
  "Root option after command name (global)."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("show" "-v" "123"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))))

(ert-deftest clime-test-parse/root-option-not-after-double-dash ()
  "Root option after -- becomes positional, not parsed as option."
  (let* ((app (clime-test--multi-arg-app))
         (result (clime-parse app '("show" "--" "-v"))))
    (should (equal (plist-get (clime-parse-result-params result) 'ids)
                   '("-v")))))

;;; ─── Group Descent ──────────────────────────────────────────────────────

(ert-deftest clime-test-parse/group-subcommand ()
  "Parse group + subcommand correctly."
  (let* ((app (clime-test--group-app))
         (result (clime-parse app '("dep" "add" "ID1"))))
    (should (equal (clime-command-name (clime-parse-result-command result)) "add"))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "ID1"))))

(ert-deftest clime-test-parse/group-subcommand-alias ()
  "Parse group + subcommand alias."
  (let* ((app (clime-test--group-app))
         (result (clime-parse app '("dep" "rm" "ID1"))))
    (should (equal (clime-command-name (clime-parse-result-command result)) "remove"))))

(ert-deftest clime-test-parse/group-with-root-global ()
  "Root global option works with group subcommand."
  (let* ((app (clime-test--group-app))
         (result (clime-parse app '("-v" "dep" "add" "ID1"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "ID1"))))

(ert-deftest clime-test-parse/group-with-positional-args ()
  "Group with required positional args before subcommand descent."
  (let* ((app (clime-test--group-with-args-app))
         (result (clime-parse app '("config" "global" "list"))))
    (should (equal (plist-get (clime-parse-result-params result) 'scope) "global"))
    (should (equal (clime-command-name (clime-parse-result-command result)) "list"))))

;;; ─── Command Path ───────────────────────────────────────────────────────

(ert-deftest clime-test-parse/path-simple ()
  "Path for simple command is (app command)."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("show" "123"))))
    (should (equal (clime-parse-result-path result) '("myapp" "show")))))

(ert-deftest clime-test-parse/path-group ()
  "Path for group command is (app group command)."
  (let* ((app (clime-test--group-app))
         (result (clime-parse app '("dep" "add" "ID1"))))
    (should (equal (clime-parse-result-path result) '("myapp" "dep" "add")))))

;;; ─── Command Alias ──────────────────────────────────────────────────────

(ert-deftest clime-test-parse/command-alias ()
  "Command alias dispatches to canonical command."
  (let* ((app (clime-test--simple-app))
         (result (clime-parse app '("get" "123"))))
    (should (equal (clime-command-name (clime-parse-result-command result)) "show"))))

;;; ─── Error Cases ────────────────────────────────────────────────────────

(ert-deftest clime-test-parse/unknown-option ()
  "Unknown option signals usage error."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("show" "--bogus" "123"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/unknown-command ()
  "Unknown command signals usage error."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("bogus"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/missing-required-arg ()
  "Missing required positional arg signals usage error."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("show"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/missing-option-value ()
  "Option expecting value at end of argv signals usage error."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("show" "123" "--format"))
                  :type 'clime-usage-error)))

;;; ─── Defaults ───────────────────────────────────────────────────────────

(ert-deftest clime-test-parse/default-applied ()
  "Default value applied when option not provided."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                 :default "text"))
         (arg (clime-make-arg :name 'id))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)
                                  :args (list arg)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "text"))))

(ert-deftest clime-test-parse/default-function-called ()
  "Default function is called when option not provided."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                 :default (lambda () "computed")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)
                                  :args (list (clime-make-arg :name 'id))))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "computed"))))

;;; ─── Help / Version ─────────────────────────────────────────────────────

(ert-deftest clime-test-parse/help-at-root ()
  "--help at root signals help-requested."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("--help"))
                  :type 'clime-help-requested)))

(ert-deftest clime-test-parse/help-short ()
  "-h signals help-requested."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("-h"))
                  :type 'clime-help-requested)))

(ert-deftest clime-test-parse/help-at-command ()
  "--help after command signals help-requested."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("show" "--help"))
                  :type 'clime-help-requested)))

(ert-deftest clime-test-parse/version ()
  "--version at root signals help-requested with version."
  (let ((app (clime-test--simple-app)))
    (should-error (clime-parse app '("--version"))
                  :type 'clime-help-requested)))

;;; ─── Additional Error Cases ──────────────────────────────────────────────

(ert-deftest clime-test-parse/missing-required-option ()
  "Missing required option signals usage error."
  (let* ((opt (clime-make-option :name 'token :flags '("--token")
                                 :required t))
         (cmd (clime-make-command :name "deploy" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "deploy" cmd)))))
    (should-error (clime-parse app '("deploy"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/short-bundle-non-boolean-error ()
  "Short bundle with non-boolean flag signals usage error."
  (let* ((opt-v (clime-make-option :name 'verbose :flags '("-v") :count t))
         (opt-f (clime-make-option :name 'format :flags '("-f")))  ;; value option
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id))))
         (app (clime-make-app :name "myapp" :version "1"
                              :options (list opt-v opt-f)
                              :children (list (cons "show" cmd)))))
    ;; -vf is not a valid bundle since -f is not boolean
    (should-error (clime-parse app '("-vf" "show" "123"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/equals-on-boolean-error ()
  "--flag=value on boolean option signals usage error."
  (let* ((opt (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id))))
         (app (clime-make-app :name "myapp" :version "1"
                              :options (list opt)
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("--json=true" "show" "123"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/group-missing-subcommand ()
  "Group without subcommand signals usage error."
  (let ((app (clime-test--group-app)))
    (should-error (clime-parse app '("dep"))
                  :type 'clime-usage-error)))

;;; ─── Multiple Fixed Positional Args ─────────────────────────────────────

(ert-deftest clime-test-parse/two-positional-args ()
  "Command with two fixed positional args."
  (let* ((arg1 (clime-make-arg :name 'src))
         (arg2 (clime-make-arg :name 'dst))
         (cmd (clime-make-command :name "copy" :handler #'ignore
                                  :args (list arg1 arg2)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "copy" cmd))))
         (result (clime-parse app '("copy" "a.txt" "b.txt"))))
    (should (equal (plist-get (clime-parse-result-params result) 'src) "a.txt"))
    (should (equal (plist-get (clime-parse-result-params result) 'dst) "b.txt"))))

(ert-deftest clime-test-parse/two-args-missing-second ()
  "Missing second required positional arg signals error."
  (let* ((arg1 (clime-make-arg :name 'src))
         (arg2 (clime-make-arg :name 'dst))
         (cmd (clime-make-command :name "copy" :handler #'ignore
                                  :args (list arg1 arg2)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "copy" cmd)))))
    (should-error (clime-parse app '("copy" "a.txt"))
                  :type 'clime-usage-error)))

;;; ─── Optional Positional Args ───────────────────────────────────────

(ert-deftest clime-test-parse/optional-arg-omitted ()
  "Optional arg not provided results in nil param, no error."
  (let* ((arg (clime-make-arg :name 'note :required nil))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id)
                                              arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "123"))
    (should-not (plist-member (clime-parse-result-params result) 'note))))

(ert-deftest clime-test-parse/optional-arg-with-default ()
  "Optional arg with default gets default applied when omitted."
  (let* ((arg (clime-make-arg :name 'format :required nil :default "text"))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id)
                                              arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "123"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format) "text"))))

;;; ─── Version Scope ─────────────────────────────────────────────────

(ert-deftest clime-test-parse/version-at-command-not-triggered ()
  "--version at command level does NOT trigger version display."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    ;; --version after command should be treated as unknown option
    (should-error (clime-parse app '("show" "--version"))
                  :type 'clime-usage-error)))

;;; ─── Group Invoke ──────────────────────────────────────────────────

(ert-deftest clime-test-parse/group-invoke-no-subcommand ()
  "Group with :handler handler and no subcommand succeeds."
  (let* ((cmd (clime-make-command :name "detail" :handler #'ignore))
         (grp (clime-make-group :name "status"
                                :handler (lambda (_ctx) "overview")
                                :children (list (cons "detail" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "status" grp))))
         (result (clime-parse app '("status"))))
    ;; Should not error — group has invoke handler
    (should (clime-parse-result-p result))
    ;; Command should be nil (we ended on a group, not a leaf command)
    (should-not (clime-parse-result-command result))))

;;; ─── Rest Args + Double Dash ───────────────────────────────────────

(ert-deftest clime-test-parse/rest-args-with-double-dash ()
  "-- inside :rest arg collection stops option parsing for remaining tokens."
  (let* ((arg (clime-make-arg :name 'args :nargs :rest :required nil))
         (opt (clime-make-option :name 'verbose :flags '("-v") :count t))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :args (list arg)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "run" cmd))))
         (result (clime-parse app '("run" "a" "--" "-v" "b"))))
    ;; -- is consumed (disables option parsing), -v after it is positional
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("a" "-v" "b")))
    ;; verbose should NOT be incremented since -v came after --
    (should-not (plist-get (clime-parse-result-params result) 'verbose))))

;;; ─── Group Scope Closure ───────────────────────────────────────────

(ert-deftest clime-test-parse/group-option-rejected-after-descent ()
  "Group-scoped option is rejected after descending into child command."
  (let* ((grp-opt (clime-make-option :name 'scope :flags '("--scope")))
         (cmd (clime-make-command :name "list" :handler #'ignore))
         (grp (clime-make-group :name "config"
                                :options (list grp-opt)
                                :children (list (cons "list" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "config" grp)))))
    ;; --scope after descending into "list" should be unknown
    (should-error (clime-parse app '("config" "list" "--scope" "local"))
                  :type 'clime-usage-error)))

;;; ─── Root Global After Deep Descent ────────────────────────────────

(ert-deftest clime-test-parse/root-global-after-group-command ()
  "Root global option works after group + command + positional."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("-v") :count t))
         (cmd (clime-make-command :name "add" :handler #'ignore
                                  :args (list (clime-make-arg :name 'id))))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :options (list root-opt)
                              :children (list (cons "dep" grp))))
         (result (clime-parse app '("dep" "add" "ID1" "-v"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))
    (should (equal (plist-get (clime-parse-result-params result) 'id) "ID1"))))

(provide 'clime-parse-tests)
;;; clime-parse-tests.el ends here
