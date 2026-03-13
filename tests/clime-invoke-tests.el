;;; clime-invoke-test-transients.el --- Tests for clime-invoke  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the transient-based interactive invoker.
;; Focuses on pure-logic helpers (key assignment, spec generation,
;; argv building) that can run in batch mode without transient.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-test-helpers)

;; clime-invoke requires transient; skip all tests if unavailable.
(defvar clime-invoke--available
  (condition-case nil
      (progn (require 'clime-invoke) t)
    (error nil))
  "Non-nil if clime-invoke loaded successfully.")

(defmacro clime-invoke-test-transient (&rest body)
  "Execute BODY only when transient is available, else skip."
  (declare (indent 0) (debug t))
  `(if clime-invoke--available
       (progn ,@body)
     (ert-skip "transient not available")))

;;; ─── Test Fixtures ──────────────────────────────────────────────────────

(defun clime-test--invoke-simple-app ()
  "Build a simple app for invoke tests."
  (let* ((opt-verbose (clime-make-option :name 'verbose
                                          :flags '("--verbose" "-v")
                                          :nargs 0
                                          :help "Be verbose"))
         (opt-output (clime-make-option :name 'output
                                         :flags '("--output" "-o")
                                         :help "Output file"))
         (opt-format (clime-make-option :name 'format
                                         :flags '("--format" "-f")
                                         :choices '("json" "text" "csv")
                                         :help "Output format"))
         (opt-tag (clime-make-option :name 'tag
                                      :flags '("--tag" "-t")
                                      :multiple t
                                      :help "Tags"))
         (arg-name (clime-make-arg :name 'name :help "Resource name"))
         (arg-file (clime-make-arg :name 'file :help "File path"
                                    :required nil))
         (cmd-show (clime-make-command :name "show"
                                       :help "Show a resource"
                                       :handler #'ignore
                                       :options (list opt-format)
                                       :args (list arg-name)))
         (cmd-create (clime-make-command :name "create"
                                          :help "Create a resource"
                                          :handler #'ignore
                                          :options (list opt-tag)
                                          :args (list arg-name arg-file)))
         (cmd-hidden (clime-make-command :name "debug"
                                          :help "Debug"
                                          :handler #'ignore
                                          :hidden t))
         (grp-admin (clime-make-group :name "admin"
                                       :help "Admin commands"
                                       :children (list (cons "show" cmd-show)
                                                       (cons "create" cmd-create)
                                                       (cons "debug" cmd-hidden)))))
    (clime-make-app :name "myapp"
                    :version "1.0.0"
                    :help "A test app"
                    :options (list opt-verbose opt-output)
                    :children (list (cons "admin" grp-admin)))))

(defun clime-test--invoke-categorized-app ()
  "Build an app with categorized options."
  (let* ((opt-verbose (clime-make-option :name 'verbose
                                          :flags '("--verbose" "-v")
                                          :nargs 0
                                          :category "Output"
                                          :help "Be verbose"))
         (opt-quiet (clime-make-option :name 'quiet
                                        :flags '("--quiet" "-q")
                                        :nargs 0
                                        :category "Output"
                                        :help "Be quiet"))
         (opt-config (clime-make-option :name 'config
                                         :flags '("--config" "-c")
                                         :category "Config"
                                         :help "Config file"))
         (cmd-run (clime-make-command :name "run"
                                       :help "Run something"
                                       :handler #'ignore)))
    (clime-make-app :name "catapp"
                    :options (list opt-verbose opt-quiet opt-config)
                    :children (list (cons "run" cmd-run)))))

(defun clime-test--invoke-inline-app ()
  "Build an app with inline groups."
  (let* ((cmd-start (clime-make-command :name "start"
                                         :help "Start service"
                                         :handler #'ignore))
         (cmd-stop (clime-make-command :name "stop"
                                        :help "Stop service"
                                        :handler #'ignore))
         (grp-svc (clime-make-group :name "service"
                                     :help "Service commands"
                                     :inline t
                                     :children (list (cons "start" cmd-start)
                                                     (cons "stop" cmd-stop))))
         (cmd-info (clime-make-command :name "info"
                                        :help "Show info"
                                        :handler #'ignore)))
    (clime-make-app :name "inlineapp"
                    :children (list (cons "service" grp-svc)
                                    (cons "info" cmd-info)))))

;;; ─── Key Assignment ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/key-from-short-flag ()
  "Short flag -v produces key \"v\"."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'verbose
                                   :flags '("--verbose" "-v")
                                   :nargs 0)))
      (should (equal "v" (clime-invoke--preferred-key opt))))))

(ert-deftest clime-test-invoke/key-from-long-flag ()
  "Long flag --output (no short) produces key \"o\"."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'output
                                   :flags '("--output"))))
      (should (equal "o" (clime-invoke--preferred-key opt))))))

(ert-deftest clime-test-invoke/assign-keys-no-collision ()
  "Assign unique keys to a set of options."
  (clime-invoke-test-transient
    (let* ((opts (list (clime-make-option :name 'verbose :flags '("--verbose" "-v") :nargs 0)
                       (clime-make-option :name 'output :flags '("--output" "-o"))
                       (clime-make-option :name 'format :flags '("--format" "-f"))))
           (keys (clime-invoke--assign-keys opts)))
      ;; Should get 3 unique keys
      (should (= 3 (length keys)))
      (should (= 3 (length (delete-dups (mapcar #'cdr keys))))))))

(ert-deftest clime-test-invoke/assign-keys-collision ()
  "When two options want the same key, second gets a different one."
  (clime-invoke-test-transient
    (let* ((opts (list (clime-make-option :name 'verbose :flags '("--verbose" "-v") :nargs 0)
                       (clime-make-option :name 'version :flags '("--version") :nargs 0)))
           (keys (clime-invoke--assign-keys opts)))
      (should (= 2 (length keys)))
      (let ((k1 (cdr (assq 'verbose keys)))
            (k2 (cdr (assq 'version keys))))
        (should-not (equal k1 k2))))))

;;; ─── Argv Building ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/argv-boolean-flag ()
  "Boolean flag set to non-nil adds the flag string to argv."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'verbose
                                   :flags '("--verbose" "-v")
                                   :nargs 0)))
      (should (equal '("--verbose")
                     (clime-invoke--option-to-argv opt t))))))

(ert-deftest clime-test-invoke/argv-boolean-flag-off ()
  "Boolean flag set to nil adds nothing."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'verbose
                                   :flags '("--verbose" "-v")
                                   :nargs 0)))
      (should (null (clime-invoke--option-to-argv opt nil))))))

(ert-deftest clime-test-invoke/argv-value-option ()
  "Value option adds --flag VALUE pair."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'output
                                   :flags '("--output" "-o"))))
      (should (equal '("--output" "file.txt")
                     (clime-invoke--option-to-argv opt "file.txt"))))))

(ert-deftest clime-test-invoke/argv-multiple-option ()
  "Multiple-value option adds repeated --flag VALUE pairs."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'tag
                                   :flags '("--tag" "-t")
                                   :multiple t)))
      (should (equal '("--tag" "a" "--tag" "b")
                     (clime-invoke--option-to-argv opt '("a" "b")))))))

(ert-deftest clime-test-invoke/argv-empty-value ()
  "Value option with empty string adds nothing."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'output
                                   :flags '("--output"))))
      (should (null (clime-invoke--option-to-argv opt nil))))))

(ert-deftest clime-test-invoke/build-full-argv ()
  "Build a complete argv from command path, options, and args."
  (clime-invoke-test-transient
    (should (equal '("admin" "show" "--verbose" "--format" "json" "myresource")
                   (clime-invoke--build-argv
                    '("admin" "show")
                    '(("--verbose") ("--format" "json"))
                    '("myresource"))))))

;;; ─── Option Collection ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/collect-options-includes-ancestors ()
  "Options from ancestor nodes are included for a leaf command."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (opts (clime-invoke--collect-options show)))
      ;; show has --format, app has --verbose and --output
      (should (>= (length opts) 3))
      (should (cl-find 'format opts :key #'clime-option-name))
      (should (cl-find 'verbose opts :key #'clime-option-name))
      (should (cl-find 'output opts :key #'clime-option-name)))))

(ert-deftest clime-test-invoke/collect-options-excludes-hidden ()
  "Hidden options are excluded from the collected set."
  (clime-invoke-test-transient
    (let* ((opt-hidden (clime-make-option :name 'secret
                                           :flags '("--secret")
                                           :hidden t))
           (opt-visible (clime-make-option :name 'name
                                            :flags '("--name")))
           (cmd (clime-make-command :name "test"
                                    :handler #'ignore
                                    :options (list opt-hidden opt-visible)))
           (opts (clime-invoke--collect-options cmd)))
      (should (= 1 (length opts)))
      (should (eq 'name (clime-option-name (car opts)))))))

;;; ─── Visible Children ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/visible-children-excludes-hidden ()
  "Hidden commands are excluded from visible children."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (children (clime-invoke--visible-children admin)))
      ;; show and create visible, debug hidden
      (should (= 2 (length children)))
      (should-not (assoc "debug" children)))))

(ert-deftest clime-test-invoke/visible-children-promotes-inline ()
  "Inline group children are promoted to parent level."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-inline-app))
           (children (clime-invoke--visible-children app)))
      ;; service is inline, so start+stop promoted alongside info
      (should (= 3 (length children)))
      (should (assoc "start" children))
      (should (assoc "stop" children))
      (should (assoc "info" children)))))

;;; ─── Category Grouping ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/group-by-category ()
  "Options are grouped by :category, nil-category first."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-categorized-app))
           (cmd (cdr (assoc "run" (clime-group-children app))))
           (opts (clime-invoke--collect-options cmd))
           (groups (clime-invoke--group-by-category opts)))
      ;; Should have groups: nil (uncategorized), "Output", "Config"
      ;; or "Output" and "Config" (no uncategorized opts on cmd itself)
      (should (>= (length groups) 2))
      (let ((output-grp (assoc "Output" groups))
            (config-grp (assoc "Config" groups)))
        (should output-grp)
        (should config-grp)
        (should (= 2 (length (cdr output-grp))))  ; verbose + quiet
        (should (= 1 (length (cdr config-grp)))))))) ; config

;;; ─── Symbol Naming ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/prefix-symbol-name ()
  "Prefix symbol encodes the app name and command path."
  (clime-invoke-test-transient
    (should (equal 'clime-invoke/myapp
                   (clime-invoke--prefix-symbol "myapp" nil)))
    (should (equal 'clime-invoke/myapp/admin
                   (clime-invoke--prefix-symbol "myapp" '("admin"))))
    (should (equal 'clime-invoke/myapp/admin/show
                   (clime-invoke--prefix-symbol "myapp" '("admin" "show"))))))

;;; ─── End-to-End: argv round-trip ────────────────────────────────────────

(ert-deftest clime-test-invoke/round-trip-parse ()
  "Argv built by invoke helpers parses correctly through clime-parse."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (argv (clime-invoke--build-argv
                  '("admin" "show")
                  '(("--verbose") ("--format" "json"))
                  '("myresource")))
           (result (clime-parse app argv))
           (params (clime-parse-result-params result)))
      (should (equal t (plist-get params 'verbose)))
      (should (equal "json" (plist-get params 'format)))
      (should (equal "myresource" (plist-get params 'name))))))

;;; ─── Edge Cases ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/build-argv-no-options ()
  "Build argv with no options, just path and args."
  (clime-invoke-test-transient
    (should (equal '("run" "myfile")
                   (clime-invoke--build-argv '("run") nil '("myfile"))))))

(ert-deftest clime-test-invoke/build-argv-no-args ()
  "Build argv with options but no positional args."
  (clime-invoke-test-transient
    (should (equal '("run" "--verbose")
                   (clime-invoke--build-argv '("run") '(("--verbose")) nil)))))

(ert-deftest clime-test-invoke/assign-keys-empty ()
  "Empty options list returns empty key map."
  (clime-invoke-test-transient
    (should (null (clime-invoke--assign-keys nil)))))

(ert-deftest clime-test-invoke/visible-children-empty-group ()
  "Group with no children returns empty list."
  (clime-invoke-test-transient
    (let ((grp (clime-make-group :name "empty" :children nil)))
      (should (null (clime-invoke--visible-children grp))))))

(ert-deftest clime-test-invoke/group-by-category-nil-cats ()
  "Options with no category group under nil key."
  (clime-invoke-test-transient
    (let* ((opts (list (clime-make-option :name 'a :flags '("--aaa"))
                       (clime-make-option :name 'b :flags '("--bbb"))))
           (groups (clime-invoke--group-by-category opts)))
      (should (= 1 (length groups)))
      (should (null (caar groups)))
      (should (= 2 (length (cdar groups)))))))

(ert-deftest clime-test-invoke/collect-options-no-ancestors ()
  "Command with no parent returns only its own options."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'foo :flags '("--foo")))
           (cmd (clime-make-command :name "test"
                                    :handler #'ignore
                                    :options (list opt)))
           (opts (clime-invoke--collect-options cmd)))
      (should (= 1 (length opts)))
      (should (eq 'foo (clime-option-name (car opts)))))))

(ert-deftest clime-test-invoke/assign-child-keys ()
  "Child keys are unique and based on command names."
  (clime-invoke-test-transient
    (let* ((children '(("start" . nil) ("stop" . nil) ("status" . nil)))
           (keys (clime-invoke--assign-child-keys children)))
      (should (= 3 (length keys)))
      ;; All keys should be unique
      (should (= 3 (length (delete-dups (mapcar #'cdr keys))))))))

;;; ─── Transient Generation (when transient available) ────────────────────

(ert-deftest clime-test-invoke/make-command-prefix ()
  "Generate a transient prefix for a leaf command."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (sym (clime-invoke--make-command-prefix app show '("admin" "show"))))
      (should (eq sym 'clime-invoke/myapp/admin/show))
      (should (fboundp sym))
      (should (get sym 'transient--prefix)))))

(ert-deftest clime-test-invoke/make-group-prefix ()
  "Generate a transient prefix for a group."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (sym (clime-invoke--make-group-prefix app app nil)))
      (should (eq sym 'clime-invoke/myapp))
      (should (fboundp sym))
      (should (get sym 'transient--prefix)))))

(ert-deftest clime-test-invoke/option-to-spec-switch ()
  "Boolean option generates a switch spec (argument without =)."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'verbose
                                    :flags '("--verbose" "-v")
                                    :nargs 0
                                    :help "Be verbose"))
           (spec (clime-invoke--option-to-spec opt "v" 'test-prefix)))
      (should (equal "v" (car spec)))
      (should (equal "Be verbose" (cadr spec)))
      ;; Third element is the argument string (no "=" means switch)
      (should (equal "--verbose " (caddr spec))))))

(ert-deftest clime-test-invoke/option-to-spec-with-choices ()
  "Option with choices generates spec with :choices."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'format
                                    :flags '("--format")
                                    :choices '("json" "text")
                                    :help "Format"))
           (spec (clime-invoke--option-to-spec opt "f" 'test-prefix)))
      ;; Third element ends with "=" (value option)
      (should (equal "--format=" (caddr spec)))
      (should (equal '("json" "text") (plist-get (cdddr spec) :choices))))))

;;; ─── Transient Parse Validation ──────────────────────────────────────────

;; transient-parse-suffixes returns internal specs as (LEVEL CLASS PLIST).
;; These tests verify our generated specs are valid transient input.

(defun clime-test--parsed-class (parsed-entry)
  "Extract the class symbol from a transient parsed spec entry.
PARSED-ENTRY is (LEVEL CLASS PLIST)."
  (cadr parsed-entry))

(ert-deftest clime-test-invoke/transient-parses-switch-spec ()
  "Transient can parse a switch spec generated by option-to-spec."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'verbose
                                    :flags '("--verbose" "-v")
                                    :nargs 0
                                    :help "Be verbose"))
           (spec (clime-invoke--option-to-spec opt "v" 'test-parse-switch))
           (parsed (transient-parse-suffixes 'test-parse-switch (list spec))))
      (should (= 1 (length parsed)))
      (should (eq 'transient-switch
                  (clime-test--parsed-class (car parsed)))))))

(ert-deftest clime-test-invoke/transient-parses-option-spec ()
  "Transient can parse a value option spec."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output
                                    :flags '("--output" "-o")
                                    :help "Output file"))
           (spec (clime-invoke--option-to-spec opt "o" 'test-parse-option))
           (parsed (transient-parse-suffixes 'test-parse-option (list spec))))
      (should (= 1 (length parsed)))
      (should (eq 'transient-option
                  (clime-test--parsed-class (car parsed)))))))

(ert-deftest clime-test-invoke/transient-parses-choices-spec ()
  "Transient can parse an option spec with :choices."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'format
                                    :flags '("--format")
                                    :choices '("json" "text")
                                    :help "Format"))
           (spec (clime-invoke--option-to-spec opt "f" 'test-parse-choices))
           (parsed (transient-parse-suffixes 'test-parse-choices (list spec))))
      (should (= 1 (length parsed)))
      (should (eq 'transient-option
                  (clime-test--parsed-class (car parsed)))))))

(ert-deftest clime-test-invoke/transient-parses-suffix-spec ()
  "Transient can parse a command suffix spec (used for group children)."
  (clime-invoke-test-transient
    (let* ((cmd (clime-make-command :name "run"
                                     :help "Run something"
                                     :handler #'ignore))
           (app (clime-make-app :name "tapp"
                                :children (list (cons "run" cmd))))
           ;; Generate a command prefix so the suffix symbol exists
           (child-sym (clime-invoke--make-command-prefix app cmd '("run")))
           (spec `("r" "Run something" ,child-sym
                       :transient transient--do-stack))
           (parsed (transient-parse-suffixes 'test-parse-suffix (list spec))))
      (should (= 1 (length parsed)))
      (should (eq 'transient-suffix
                  (clime-test--parsed-class (car parsed)))))))

(provide 'clime-invoke-tests)
;;; clime-invoke-test-transients.el ends here
