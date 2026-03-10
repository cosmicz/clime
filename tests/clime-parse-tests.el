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

;;; ─── Separator Options ────────────────────────────────────────────────────

(ert-deftest clime-test-parse/separator-single-value ()
  "Separator option with comma-separated values splits into a list."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag" "-t")
                                 :separator "," :multiple t))
         (cmd (clime-make-command :name "create" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "create" cmd))))
         (result (clime-parse app '("create" "--tag" "a,b,c"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("a" "b" "c")))))

(ert-deftest clime-test-parse/separator-repeated-flags ()
  "Separator option with repeated flags flattens all values."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag" "-t")
                                 :separator "," :multiple t))
         (cmd (clime-make-command :name "create" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "create" cmd))))
         (result (clime-parse app '("create" "--tag" "a" "--tag" "b,c"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("a" "b" "c")))))

(ert-deftest clime-test-parse/separator-no-split-without-separator ()
  "Multiple option without separator does not split on commas."
  (let* ((app (clime-test--multi-opt-app))
         (result (clime-parse app '("create" "--tag" "a,b"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("a,b")))))

(ert-deftest clime-test-parse/separator-with-choices ()
  "Separator option validates each element against choices."
  (let* ((opt (clime-make-option :name 'status :flags '("--status")
                                 :separator "," :multiple t
                                 :choices '("TODO" "ON" "DONE")))
         (cmd (clime-make-command :name "list" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "list" cmd))))
         (result (clime-parse app '("list" "--status" "TODO,ON"))))
    (should (equal (plist-get (clime-parse-result-params result) 'status)
                   '("TODO" "ON")))))

(ert-deftest clime-test-parse/separator-choices-rejects-invalid ()
  "Separator option rejects invalid elements against choices."
  (let* ((opt (clime-make-option :name 'status :flags '("--status")
                                 :separator "," :multiple t
                                 :choices '("TODO" "ON" "DONE")))
         (cmd (clime-make-command :name "list" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "list" cmd)))))
    (should-error (clime-parse app '("list" "--status" "TODO,BOGUS"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/separator-with-coerce ()
  "Separator option applies coerce to each element."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag")
                                 :separator "," :multiple t
                                 :coerce #'upcase))
         (cmd (clime-make-command :name "create" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "create" cmd))))
         (result (clime-parse app '("create" "--tag" "dev,ci"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("DEV" "CI")))))

(ert-deftest clime-test-parse/separator-empty-segments-filtered ()
  "Empty segments from splitting are filtered out."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag")
                                 :separator "," :multiple t))
         (cmd (clime-make-command :name "create" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "myapp" :version "1"
                              :children (list (cons "create" cmd))))
         (result (clime-parse app '("create" "--tag" "a,,b"))))
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
  "Group without subcommand signals help-requested."
  (let ((app (clime-test--group-app)))
    (should-error (clime-parse app '("dep"))
                  :type 'clime-help-requested)))

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

;;; ─── Ancestor Option Propagation ────────────────────────────────────

(ert-deftest clime-test-parse/group-option-visible-to-child ()
  "Group option is visible to child commands via ancestor propagation."
  (let* ((grp-opt (clime-make-option :name 'scope :flags '("--scope")))
         (cmd (clime-make-command :name "list" :handler #'ignore))
         (grp (clime-make-group :name "config"
                                :options (list grp-opt)
                                :children (list (cons "list" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "config" grp))))
         (result (clime-parse app '("config" "list" "--scope" "local"))))
    (should (equal (plist-get (clime-parse-result-params result) 'scope)
                   "local"))))

(ert-deftest clime-test-parse/group-option-before-child ()
  "Group option works before the child command name."
  (let* ((grp-opt (clime-make-option :name 'scope :flags '("--scope")))
         (cmd (clime-make-command :name "list" :handler #'ignore))
         (grp (clime-make-group :name "config"
                                :options (list grp-opt)
                                :children (list (cons "list" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "config" grp))))
         (result (clime-parse app '("config" "--scope" "local" "list"))))
    (should (equal (plist-get (clime-parse-result-params result) 'scope)
                   "local"))))

(ert-deftest clime-test-parse/multi-level-ancestor-propagation ()
  "Options propagate through multiple group levels."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("-v") :count t))
         (mid-opt (clime-make-option :name 'env :flags '("--env")))
         (cmd (clime-make-command :name "start" :handler #'ignore))
         (inner-grp (clime-make-group :name "svc"
                                       :children (list (cons "start" cmd))))
         (outer-grp (clime-make-group :name "deploy"
                                       :options (list mid-opt)
                                       :children (list (cons "svc" inner-grp))))
         (app (clime-make-app :name "t" :version "1"
                              :options (list root-opt)
                              :children (list (cons "deploy" outer-grp))))
         (result (clime-parse app '("deploy" "svc" "start" "-v" "--env" "prod"))))
    (should (= (plist-get (clime-parse-result-params result) 'verbose) 1))
    (should (equal (plist-get (clime-parse-result-params result) 'env) "prod"))))

(ert-deftest clime-test-parse/ancestor-boolean-option ()
  "Boolean ancestor option works from a descendant."
  (let* ((grp-opt (clime-make-option :name 'dry-run :flags '("--dry-run") :nargs 0))
         (cmd (clime-make-command :name "apply" :handler #'ignore))
         (grp (clime-make-group :name "config"
                                :options (list grp-opt)
                                :children (list (cons "apply" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "config" grp))))
         (result (clime-parse app '("config" "apply" "--dry-run"))))
    (should (eq (plist-get (clime-parse-result-params result) 'dry-run) t))))

(ert-deftest clime-test-parse/ancestor-option-with-equals ()
  "Ancestor option with --flag=value syntax works."
  (let* ((grp-opt (clime-make-option :name 'env :flags '("--env")))
         (cmd (clime-make-command :name "run" :handler #'ignore))
         (grp (clime-make-group :name "deploy"
                                :options (list grp-opt)
                                :children (list (cons "run" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "deploy" grp))))
         (result (clime-parse app '("deploy" "run" "--env=staging"))))
    (should (equal (plist-get (clime-parse-result-params result) 'env)
                   "staging"))))

(ert-deftest clime-test-parse/ancestor-short-bundle ()
  "Short flag bundle includes ancestor boolean options."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (grp-opt (clime-make-option :name 'force :flags '("-f") :nargs 0))
         (cmd (clime-make-command :name "apply" :handler #'ignore))
         (grp (clime-make-group :name "config"
                                :options (list grp-opt)
                                :children (list (cons "apply" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :options (list root-opt)
                              :children (list (cons "config" grp))))
         (result (clime-parse app '("config" "apply" "-vf"))))
    (should (eq (plist-get (clime-parse-result-params result) 'verbose) t))
    (should (eq (plist-get (clime-parse-result-params result) 'force) t))))

(ert-deftest clime-test-parse/inline-group-option-propagates ()
  "Inline group options propagate to descendants."
  (let* ((grp-opt (clime-make-option :name 'env :flags '("--env")))
         (cmd (clime-make-command :name "start" :handler #'ignore))
         (grp (clime-make-group :name "svc" :inline t
                                :options (list grp-opt)
                                :children (list (cons "start" cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "svc" grp))))
         (result (clime-parse app '("start" "--env" "prod"))))
    (should (equal (plist-get (clime-parse-result-params result) 'env)
                   "prod"))))

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

;;; ─── Type Conversion ──────────────────────────────────────────────────

(ert-deftest clime-test-parse/type-integer-option ()
  "Option with :type 'integer converts string to integer."
  (let* ((opt (clime-make-option :name 'count :flags '("--count" "-c")
                                 :type 'integer))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--count" "42"))))
    (should (= (plist-get (clime-parse-result-params result) 'count) 42))))

(ert-deftest clime-test-parse/type-integer-negative ()
  "Negative integer is parsed correctly."
  (let* ((opt (clime-make-option :name 'offset :flags '("--offset")
                                 :type 'integer))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--offset" "-5"))))
    (should (= (plist-get (clime-parse-result-params result) 'offset) -5))))

(ert-deftest clime-test-parse/type-integer-invalid ()
  "Non-integer value for :type 'integer signals usage error."
  (let* ((opt (clime-make-option :name 'count :flags '("--count")
                                 :type 'integer))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--count" "abc"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/type-integer-float-rejected ()
  "Float value for :type 'integer signals usage error."
  (let* ((opt (clime-make-option :name 'count :flags '("--count")
                                 :type 'integer))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--count" "3.14"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/type-number-float ()
  "Option with :type 'number accepts float values."
  (let* ((opt (clime-make-option :name 'rate :flags '("--rate")
                                 :type 'number))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--rate" "3.14"))))
    (should (= (plist-get (clime-parse-result-params result) 'rate) 3.14))))

(ert-deftest clime-test-parse/type-number-invalid ()
  "Non-numeric value for :type 'number signals usage error."
  (let* ((opt (clime-make-option :name 'rate :flags '("--rate")
                                 :type 'number))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--rate" "abc"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/type-integer-arg ()
  "Positional arg with :type 'integer converts string to integer."
  (let* ((arg (clime-make-arg :name 'port :type 'integer))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "serve" cmd))))
         (result (clime-parse app '("serve" "8080"))))
    (should (= (plist-get (clime-parse-result-params result) 'port) 8080))))

(ert-deftest clime-test-parse/type-multiple-integer ()
  "Multiple option with :type 'integer coerces each value."
  (let* ((opt (clime-make-option :name 'port :flags '("--port" "-p")
                                 :type 'integer :multiple t))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "serve" cmd))))
         (result (clime-parse app '("serve" "-p" "80" "-p" "443"))))
    (should (equal (plist-get (clime-parse-result-params result) 'port)
                   '(80 443)))))

(ert-deftest clime-test-parse/type-string-default ()
  "Default :type 'string leaves values as strings."
  (let* ((opt (clime-make-option :name 'name :flags '("--name")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--name" "42"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name) "42"))))

(ert-deftest clime-test-parse/type-integer-equals ()
  "Integer type works with --flag=value syntax."
  (let* ((opt (clime-make-option :name 'count :flags '("--count")
                                 :type 'integer))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--count=10"))))
    (should (= (plist-get (clime-parse-result-params result) 'count) 10))))

;;; ─── Choices Validation ──────────────────────────────────────────────

(ert-deftest clime-test-parse/choices-option-valid ()
  "Option with :choices accepts a valid value."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices '("json" "table" "csv")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--format" "json"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format)
                   "json"))))

(ert-deftest clime-test-parse/choices-option-invalid ()
  "Option with :choices rejects an invalid value."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices '("json" "table" "csv")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--format" "xml"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/choices-with-integer-type ()
  "Choices validation works post-coercion with integer type."
  (let* ((opt (clime-make-option :name 'level :flags '("--level")
                                  :type 'integer :choices '(1 2 3)))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--level" "2"))))
    (should (= (plist-get (clime-parse-result-params result) 'level) 2))))

(ert-deftest clime-test-parse/choices-integer-invalid ()
  "Choices with integer type rejects values not in set."
  (let* ((opt (clime-make-option :name 'level :flags '("--level")
                                  :type 'integer :choices '(1 2 3)))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--level" "5"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/choices-positional-arg ()
  "Positional arg with :choices validates value."
  (let* ((arg (clime-make-arg :name 'action :choices '("start" "stop")))
         (cmd (clime-make-command :name "svc" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "svc" cmd))))
         (result (clime-parse app '("svc" "start"))))
    (should (equal (plist-get (clime-parse-result-params result) 'action)
                   "start"))))

(ert-deftest clime-test-parse/choices-positional-arg-invalid ()
  "Positional arg with :choices rejects invalid value."
  (let* ((arg (clime-make-arg :name 'action :choices '("start" "stop")))
         (cmd (clime-make-command :name "svc" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "svc" cmd)))))
    (should-error (clime-parse app '("svc" "restart"))
                  :type 'clime-usage-error)))

;;; ─── Coerce Function ──────────────────────────────────────────────────

(ert-deftest clime-test-parse/coerce-option ()
  "Option with :coerce applies custom transform."
  (let* ((opt (clime-make-option :name 'path :flags '("--path")
                                  :coerce #'expand-file-name))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--path" "~/foo"))))
    (should (equal (plist-get (clime-parse-result-params result) 'path)
                   (expand-file-name "~/foo")))))

(ert-deftest clime-test-parse/coerce-positional-arg ()
  "Positional arg with :coerce applies custom transform."
  (let* ((arg (clime-make-arg :name 'name :coerce #'upcase))
         (cmd (clime-make-command :name "greet" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "greet" cmd))))
         (result (clime-parse app '("greet" "hello"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "HELLO"))))

(ert-deftest clime-test-parse/coerce-with-integer-type ()
  "Coerce runs after type conversion."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                  :type 'integer
                                  :coerce (lambda (n) (* n 10))))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "serve" cmd))))
         (result (clime-parse app '("serve" "--port" "8"))))
    (should (= (plist-get (clime-parse-result-params result) 'port) 80))))

(ert-deftest clime-test-parse/choices-and-coerce-combined ()
  "Choices validates before coerce runs."
  (let* ((opt (clime-make-option :name 'env :flags '("--env")
                                  :choices '("dev" "prod")
                                  :coerce #'upcase))
         (cmd (clime-make-command :name "deploy" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "deploy" cmd))))
         (result (clime-parse app '("deploy" "--env" "dev"))))
    (should (equal (plist-get (clime-parse-result-params result) 'env)
                   "DEV"))))

(ert-deftest clime-test-parse/choices-and-coerce-rejects-invalid ()
  "Choices rejects before coerce would run."
  (let* ((opt (clime-make-option :name 'env :flags '("--env")
                                  :choices '("dev" "prod")
                                  :coerce #'upcase))
         (cmd (clime-make-command :name "deploy" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "deploy" cmd)))))
    (should-error (clime-parse app '("deploy" "--env" "staging"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/coerce-multiple-option ()
  "Coerce applies to each value in a :multiple option."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag")
                                  :multiple t :coerce #'upcase))
         (cmd (clime-make-command :name "create" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "create" cmd))))
         (result (clime-parse app '("create" "--tag" "dev" "--tag" "ci"))))
    (should (equal (plist-get (clime-parse-result-params result) 'tag)
                   '("DEV" "CI")))))

(ert-deftest clime-test-parse/choices-multiple-option ()
  "Choices validates each value in a :multiple option."
  (let* ((opt (clime-make-option :name 'tag :flags '("--tag")
                                  :multiple t
                                  :choices '("dev" "ci" "prod")))
         (cmd (clime-make-command :name "create" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "create" cmd)))))
    ;; Valid values
    (let ((result (clime-parse app '("create" "--tag" "dev" "--tag" "ci"))))
      (should (equal (plist-get (clime-parse-result-params result) 'tag)
                     '("dev" "ci"))))
    ;; Invalid value
    (should-error (clime-parse app '("create" "--tag" "dev" "--tag" "nope"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/choices-equals-syntax ()
  "Choices works with --flag=value syntax."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices '("json" "csv")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (let ((result (clime-parse app '("show" "--format=json"))))
      (should (equal (plist-get (clime-parse-result-params result) 'format)
                     "json")))
    (should-error (clime-parse app '("show" "--format=xml"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/choices-error-message-lists-values ()
  "Choices error message includes allowed values."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices '("json" "csv")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (condition-case err
        (progn (clime-parse app '("show" "--format" "xml")) (should nil))
      (clime-usage-error
       (let ((msg (cadr err)))
         (should (string-match-p "json" msg))
         (should (string-match-p "csv" msg))
         (should (string-match-p "xml" msg)))))))

;;; ─── Lazy Choices (Function) ────────────────────────────────────────

(ert-deftest clime-test-parse/choices-function-option-valid ()
  "Option with :choices as a function accepts valid values."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices (lambda () '("json" "table" "csv"))))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd))))
         (result (clime-parse app '("show" "--format" "json"))))
    (should (equal (plist-get (clime-parse-result-params result) 'format)
                   "json"))))

(ert-deftest clime-test-parse/choices-function-option-invalid ()
  "Option with :choices as a function rejects invalid values."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices (lambda () '("json" "table" "csv"))))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (should-error (clime-parse app '("show" "--format" "xml"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/choices-function-positional-arg ()
  "Positional arg with :choices as a function validates value."
  (let* ((arg (clime-make-arg :name 'action
                               :choices (lambda () '("start" "stop"))))
         (cmd (clime-make-command :name "svc" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "svc" cmd))))
         (result (clime-parse app '("svc" "start"))))
    (should (equal (plist-get (clime-parse-result-params result) 'action)
                   "start"))))

(ert-deftest clime-test-parse/choices-function-error-lists-values ()
  "Error message from function choices includes the resolved values."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices (lambda () '("json" "csv"))))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "show" cmd)))))
    (condition-case err
        (progn (clime-parse app '("show" "--format" "xml")) (should nil))
      (clime-usage-error
       (let ((msg (cadr err)))
         (should (string-match-p "json" msg))
         (should (string-match-p "csv" msg)))))))

;;; ─── Rest Args + Known Options ──────────────────────────────────────

(defun clime-test--rest-with-opts-app ()
  "Build an app with rest args plus options in scope."
  (let* ((script-arg (clime-make-arg :name 'script))
         (rest-arg (clime-make-arg :name 'args :nargs :rest :required nil))
         (cmd-opt (clime-make-option :name 'force :flags '("--force" "-f")
                                      :nargs 0))
         (root-opt (clime-make-option :name 'json :flags '("--json")
                                       :nargs 0))
         (fmt-opt (clime-make-option :name 'format :flags '("--format")
                                      :help "Output format"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :args (list script-arg rest-arg)
                                  :options (list cmd-opt fmt-opt))))
    (clime-make-app :name "t" :version "1"
                    :options (list root-opt)
                    :children (list (cons "run" cmd)))))

(ert-deftest clime-test-parse/rest-known-bool-option ()
  "Known boolean option during rest collection is parsed as option."
  (let* ((app (clime-test--rest-with-opts-app))
         (result (clime-parse app '("run" "myscript" "arg1" "--json"))))
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("arg1")))
    (should (eq (plist-get (clime-parse-result-params result) 'json) t))))

(ert-deftest clime-test-parse/rest-known-value-option ()
  "Known value option during rest collection is parsed with its value."
  (let* ((app (clime-test--rest-with-opts-app))
         (result (clime-parse app '("run" "myscript" "arg1" "--format" "csv"))))
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("arg1")))
    (should (equal (plist-get (clime-parse-result-params result) 'format)
                   "csv"))))

(ert-deftest clime-test-parse/rest-unknown-option-collected ()
  "Unknown option-like tokens are collected as rest values."
  (let* ((app (clime-test--rest-with-opts-app))
         (result (clime-parse app '("run" "myscript" "--unknown-flag"))))
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("--unknown-flag")))))

(ert-deftest clime-test-parse/rest-double-dash-disables ()
  "-- during rest collection makes subsequent known options become rest values."
  (let* ((app (clime-test--rest-with-opts-app))
         (result (clime-parse app '("run" "myscript" "--" "--json"))))
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("--json")))
    (should-not (plist-get (clime-parse-result-params result) 'json))))

(ert-deftest clime-test-parse/rest-help-triggers ()
  "--help during rest collection triggers help."
  (let* ((app (clime-test--rest-with-opts-app)))
    (should-error (clime-parse app '("run" "myscript" "arg1" "--help"))
                  :type 'clime-help-requested)))

(ert-deftest clime-test-parse/rest-equals-syntax ()
  "--flag=value during rest collection is parsed correctly."
  (let* ((app (clime-test--rest-with-opts-app))
         (result (clime-parse app '("run" "myscript" "arg1" "--format=csv"))))
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("arg1")))
    (should (equal (plist-get (clime-parse-result-params result) 'format)
                   "csv"))))

(ert-deftest clime-test-parse/rest-cmd-option-during-rest ()
  "Command-level option during rest collection is parsed."
  (let* ((app (clime-test--rest-with-opts-app))
         (result (clime-parse app '("run" "myscript" "arg1" "--force"))))
    (should (equal (plist-get (clime-parse-result-params result) 'args)
                   '("arg1")))
    (should (eq (plist-get (clime-parse-result-params result) 'force) t))))

;;; ─── Default Group ──────────────────────────────────────────────────

(defun clime-test--default-group-app ()
  "Build an app with a default group whose children are promoted."
  (let* ((add-cmd (clime-make-command :name "add" :handler #'ignore
                                       :args (list (clime-make-arg :name 'name))))
         (rm-cmd (clime-make-command :name "remove" :handler #'ignore
                                      :args (list (clime-make-arg :name 'name))))
         (grp (clime-make-group :name "repo" :inline t
                                :children (list (cons "add" add-cmd)
                                                (cons "remove" rm-cmd))))
         (other-cmd (clime-make-command :name "info" :handler #'ignore)))
    (clime-make-app :name "t" :version "1"
                    :children (list (cons "repo" grp)
                                    (cons "info" other-cmd)))))

(ert-deftest clime-test-parse/default-group-promoted-dispatch ()
  "Commands in a :default group are accessible at parent level."
  (let* ((app (clime-test--default-group-app))
         (result (clime-parse app '("add" "myrepo"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "myrepo"))))

(ert-deftest clime-test-parse/default-group-direct-still-works ()
  "The group name still works as a prefix."
  (let* ((app (clime-test--default-group-app))
         (result (clime-parse app '("repo" "add" "myrepo"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "myrepo"))))

(ert-deftest clime-test-parse/default-group-parent-child-wins ()
  "Parent's own children take priority over default group's."
  (let* ((add-cmd (clime-make-command :name "info" :handler #'ignore
                                       :args (list (clime-make-arg :name 'topic))))
         (grp (clime-make-group :name "repo" :inline t
                                :children (list (cons "info" add-cmd))))
         (parent-info (clime-make-command :name "info" :handler #'ignore))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "repo" grp)
                                              (cons "info" parent-info))))
         (result (clime-parse app '("info"))))
    ;; Should dispatch to parent's info (no args), not repo's info (has topic arg)
    (should-not (plist-get (clime-parse-result-params result) 'topic))))

(ert-deftest clime-test-parse/default-group-non-default-unaffected ()
  "Non-default groups don't promote their children."
  (let* ((app (clime-test--group-app)))
    ;; "add" without "dep" prefix should fail
    (should-error (clime-parse app '("add" "foo" "https://example.com"))
                  :type 'clime-usage-error)))

(ert-deftest clime-test-parse/inline-group-child-alias ()
  "Inline group child commands are found by alias at parent level."
  (let* ((add-cmd (clime-make-command :name "add" :handler #'ignore
                                       :aliases '("a")
                                       :args (list (clime-make-arg :name 'name))))
         (grp (clime-make-group :name "repo" :inline t
                                :children (list (cons "add" add-cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "repo" grp))))
         (result (clime-parse app '("a" "myrepo"))))
    (should (equal (plist-get (clime-parse-result-params result) 'name)
                   "myrepo"))))

;;; ─── Inline Group Path ──────────────────────────────────────────────────

(ert-deftest clime-test-parse/inline-group-path-includes-group ()
  "Parse path includes inline group name when dispatching through it."
  (let* ((add-cmd (clime-make-command :name "add" :handler #'ignore
                                       :args (list (clime-make-arg :name 'name))))
         (grp (clime-make-group :name "repo" :inline t
                                :children (list (cons "add" add-cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "repo" grp))))
         (result (clime-parse app '("add" "myrepo"))))
    (should (equal (clime-parse-result-path result)
                   '("t" "repo" "add")))))

(ert-deftest clime-test-parse/inline-group-direct-path ()
  "Direct dispatch through group name also includes it in path."
  (let* ((add-cmd (clime-make-command :name "add" :handler #'ignore
                                       :args (list (clime-make-arg :name 'name))))
         (grp (clime-make-group :name "repo" :inline t
                                :children (list (cons "add" add-cmd))))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "repo" grp))))
         (result (clime-parse app '("repo" "add" "myrepo"))))
    (should (equal (clime-parse-result-path result)
                   '("t" "repo" "add")))))

(provide 'clime-parse-tests)
;;; clime-parse-tests.el ends here
