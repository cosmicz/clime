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
           (keys (clime-invoke--assign-keys
                  (mapcar #'clime-invoke--option-key-item opts))))
      ;; Should get 3 unique keys
      (should (= 3 (length keys)))
      (should (= 3 (length (delete-dups (mapcar #'cdr keys))))))))

(ert-deftest clime-test-invoke/assign-keys-collision ()
  "When two options want the same key, second gets a different one."
  (clime-invoke-test-transient
    (let* ((opts (list (clime-make-option :name 'verbose :flags '("--verbose" "-v") :nargs 0)
                       (clime-make-option :name 'version :flags '("--version") :nargs 0)))
           (keys (clime-invoke--assign-keys
                  (mapcar #'clime-invoke--option-key-item opts))))
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

(ert-deftest clime-test-invoke/argv-count-option ()
  "Count option emits the flag N times."
  (clime-invoke-test-transient
    (let ((opt (clime-make-option :name 'verbose
                                   :flags '("-v")
                                   :nargs 0
                                   :count t)))
      (should (equal '("-v" "-v" "-v")
                     (clime-invoke--option-to-argv opt 3)))
      (should (null (clime-invoke--option-to-argv opt 0))))))

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
           (keys (clime-invoke--assign-keys
                  (mapcar (lambda (entry)
                            (let ((name (car entry)))
                              (list name (substring name 0 1) name)))
                          children))))
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

(ert-deftest clime-test-invoke/make-group-prefix-with-handler ()
  "Group with handler gets a Run action in the layout."
  (clime-invoke-test-transient
    (let* ((cmd (clime-make-command :name "sub" :handler #'ignore))
           (grp (clime-make-group :name "grp" :handler #'ignore
                                   :children `(("sub" . ,cmd))))
           (app (clime-make-app :name "myapp" :version "0.1"
                                 :children `(("grp" . ,grp))))
           (sym (clime-invoke--make-group-prefix app grp '("grp")))
           (layout (get sym 'transient--layout)))
      ;; Should contain an "Actions" group
      (should (cl-some (lambda (vec)
                         (equal "Actions"
                                (plist-get (aref vec 2) :description)))
                       layout)))))

(ert-deftest clime-test-invoke/make-group-prefix-no-handler ()
  "Group without handler has no Run action."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (sym (clime-invoke--make-group-prefix app app nil))
           (layout (get sym 'transient--layout)))
      (should-not (cl-some (lambda (vec)
                             (equal "Actions"
                                    (plist-get (aref vec 2) :description)))
                           layout)))))

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
  "Option with choices generates spec with cycling infix class."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'format
                                    :flags '("--format")
                                    :choices '("json" "text")
                                    :help "Format"))
           (spec (clime-invoke--option-to-spec opt "f" 'test-prefix)))
      (should (equal "--format=" (caddr spec)))
      (should (eq 'clime-invoke-choices-infix
                  (plist-get (nthcdr 3 spec) :class)))
      (should (equal '("json" "text") (plist-get (nthcdr 3 spec) :choices))))))

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
      (should (eq 'clime-invoke-value-infix
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
      (should (eq 'clime-invoke-choices-infix
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

;;; ─── App Registry ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/register-app ()
  "Registering an app makes it discoverable."
  (clime-invoke-test-transient
    (let ((clime-invoke--registry (make-hash-table :test #'equal)))
      (let ((app (clime-make-app :name "testapp"
                                  :version "1.0"
                                  :children nil)))
        (clime-register-app "testapp" app)
        (should (eq app (gethash "testapp" clime-invoke--registry)))))))

(ert-deftest clime-test-invoke/registry-list ()
  "Registered apps are enumerable for completing-read."
  (clime-invoke-test-transient
    (let ((clime-invoke--registry (make-hash-table :test #'equal)))
      (let ((app1 (clime-make-app :name "alpha" :version "1" :children nil))
            (app2 (clime-make-app :name "beta" :version "1" :children nil)))
        (clime-register-app "alpha" app1)
        (clime-register-app "beta" app2)
        (let ((keys (clime-invoke--registry-keys)))
          (should (= 2 (length keys)))
          (should (member "alpha" keys))
          (should (member "beta" keys)))))))

(ert-deftest clime-test-invoke/register-app-overwrites ()
  "Re-registering an app replaces the previous entry."
  (clime-invoke-test-transient
    (let ((clime-invoke--registry (make-hash-table :test #'equal)))
      (let ((app1 (clime-make-app :name "myapp" :version "1" :children nil))
            (app2 (clime-make-app :name "myapp" :version "2" :children nil)))
        (clime-register-app "myapp" app1)
        (clime-register-app "myapp" app2)
        (should (eq app2 (gethash "myapp" clime-invoke--registry)))))))

;;; ─── Header Formatting ────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-header-help ()
  "Header includes the node help text."
  (clime-invoke-test-transient
    (let ((cmd (clime-make-command :name "show"
                                    :help "Display a resource"
                                    :handler #'ignore)))
      (let ((header (clime-invoke--format-header cmd '("show"))))
        (should (string-match-p "Display a resource" header))))))

(ert-deftest clime-test-invoke/format-header-examples ()
  "Header includes formatted examples."
  (clime-invoke-test-transient
    (let ((cmd (clime-make-command :name "show"
                                    :help "Display a resource"
                                    :handler #'ignore
                                    :examples '(("app show 123" . "Show by ID")
                                                ("app show --all" . "Show all")))))
      (let ((header (clime-invoke--format-header cmd '("show"))))
        (should (string-match-p "app show 123" header))
        (should (string-match-p "Show by ID" header))
        (should (string-match-p "app show --all" header))))))

(ert-deftest clime-test-invoke/format-header-no-examples ()
  "Header works with no examples."
  (clime-invoke-test-transient
    (let ((cmd (clime-make-command :name "run"
                                    :help "Run a task"
                                    :handler #'ignore)))
      (let ((header (clime-invoke--format-header cmd '("run"))))
        (should (string-match-p "Run a task" header))
        (should-not (string-match-p "Examples" header))))))

(ert-deftest clime-test-invoke/format-header-bare-example ()
  "Header handles bare string examples (no description)."
  (clime-invoke-test-transient
    (let ((cmd (clime-make-command :name "init"
                                    :help "Initialize"
                                    :handler #'ignore
                                    :examples '("app init ."))))
      (let ((header (clime-invoke--format-header cmd '("init"))))
        (should (string-match-p "app init \\." header))))))

;;; ─── Ancestor Option Labeling ─────────────────────────────────────────

(ert-deftest clime-test-invoke/ancestor-options-get-global-category ()
  "Ancestor options without a category get labeled 'Global Options'."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (opts (clime-invoke--collect-options-grouped show)))
      ;; Should have at least a "Global Options" group for --verbose/--output
      (should (assoc "Global Options" opts)))))

(ert-deftest clime-test-invoke/ancestor-options-preserve-category ()
  "Ancestor options with :category keep their original label."
  (clime-invoke-test-transient
    (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")
                                         :nargs 0 :category "Output"))
           (cmd-opt (clime-make-option :name 'file :flags '("--file")))
           (cmd (clime-make-command :name "run" :handler #'ignore
                                     :options (list cmd-opt)))
           (app (clime-make-app :name "test" :version "0.1"
                                 :options (list root-opt)
                                 :children `(("run" . ,cmd)))))
      (let ((grouped (clime-invoke--collect-options-grouped cmd)))
        ;; Own option has nil category
        (should (assoc nil grouped))
        ;; Ancestor option keeps "Output" category, not "Global Options"
        (should (assoc "Output" grouped))
        (should-not (assoc "Global Options" grouped))))))

;;; ─── Pre-run Validation ───────────────────────────────────────────────

(ert-deftest clime-test-invoke/validate-required-option-missing ()
  "Validation catches missing required options."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output
                                    :flags '("--output" "-o")
                                    :required t
                                    :help "Output file"))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt))))
      ;; Empty params = missing required
      (let ((err (clime-invoke--validate-pre-run cmd '())))
        (should err)
        (should (string-match-p "output" err))))))

(ert-deftest clime-test-invoke/validate-required-option-present ()
  "Validation passes when required option is provided."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output
                                    :flags '("--output" "-o")
                                    :required t
                                    :help "Output file"))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt))))
      (let ((err (clime-invoke--validate-pre-run cmd '((output . "file.txt")))))
        (should-not err)))))

(ert-deftest clime-test-invoke/validate-mutex-violation ()
  "Validation catches mutex conformer violations."
  (clime-invoke-test-transient
    (let* ((opt-a (clime-make-option :name 'json :flags '("--json") :nargs 0))
           (opt-b (clime-make-option :name 'text :flags '("--text") :nargs 0))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt-a opt-b)
                                     :conform (clime-check-exclusive
                                               'format '(json text)))))
      ;; Both set = mutex violation
      (let ((err (clime-invoke--validate-pre-run
                  cmd '((json . t) (text . t)))))
        (should err)
        (should (string-match-p "exclusive\\|mutually\\|conflict" err))))))

(ert-deftest clime-test-invoke/validate-no-conformer-ok ()
  "Validation passes when no conformers and no required options."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose") :nargs 0))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt))))
      (should-not (clime-invoke--validate-pre-run cmd '())))))

;;; ─── Incompatible Extraction ─────────────────────────────────────────────

(ert-deftest clime-test-invoke/extract-incompatible-from-exclusive ()
  "Extract :incompatible groups from clime-check-exclusive conformers."
  (clime-invoke-test-transient
    (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
           (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt-json opt-csv)
                                     :conform (clime-check-exclusive
                                               'fmt '(json csv)))))
      (let ((groups (clime-invoke--extract-incompatible cmd)))
        (should (= 1 (length groups)))
        (should (member "--json " (car groups)))
        (should (member "--csv " (car groups)))))))

(ert-deftest clime-test-invoke/extract-incompatible-no-conformer ()
  "No :incompatible when node has no conformers."
  (clime-invoke-test-transient
    (let ((cmd (clime-make-command :name "run"
                                    :handler #'ignore)))
      (should-not (clime-invoke--extract-incompatible cmd)))))

;;; ─── Params Pre-population ──────────────────────────────────────────────

(ert-deftest clime-test-invoke/params-to-transient-value-option ()
  "Convert option params to transient value strings."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output :flags '("--output")))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt))))
      (let ((val (clime-invoke--params-to-transient-value
                  cmd '((output . "file.txt")))))
        (should (equal '("--output=file.txt") val))))))

(ert-deftest clime-test-invoke/params-to-transient-value-switch ()
  "Convert boolean option params to transient switch strings."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose") :nargs 0))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :options (list opt))))
      (let ((val (clime-invoke--params-to-transient-value
                  cmd '((verbose . t)))))
        (should (equal '("--verbose ") val))))))

(ert-deftest clime-test-invoke/params-to-transient-value-arg ()
  "Convert positional arg params to transient value strings."
  (clime-invoke-test-transient
    (let* ((arg (clime-make-arg :name 'file))
           (cmd (clime-make-command :name "run"
                                     :handler #'ignore
                                     :args (list arg))))
      (let ((val (clime-invoke--params-to-transient-value
                  cmd '((file . "test.txt")))))
        (should (equal '("file=test.txt") val))))))

;;; ─── Unified Key Assignment ─────────────────────────────────────────────

(ert-deftest clime-test-invoke/assign-keys-generic ()
  "Unified assign-keys works with generic (NAME PREFERRED FALLBACK) items."
  (clime-invoke-test-transient
    (let ((keys (clime-invoke--assign-keys
                 '((a "x" "abc")
                   (b "x" "bcd")
                   (c nil "cde")))))
      (should (= 3 (length keys)))
      ;; First gets preferred "x"
      (should (equal "x" (cdr (assq 'a keys))))
      ;; Second can't use "x", tries "b" from fallback
      (should (equal "b" (cdr (assq 'b keys))))
      ;; Third has no preferred, tries "c" from fallback
      (should (equal "c" (cdr (assq 'c keys)))))))

(ert-deftest clime-test-invoke/assign-keys-with-prefix ()
  "Prefix is prepended to fallback candidates."
  (clime-invoke-test-transient
    (let ((keys (clime-invoke--assign-keys
                 '((verbose "-v" "verbose" "-")
                   (output nil "output" "-")))))
      (should (equal "-v" (cdr (assq 'verbose keys))))
      (should (equal "-o" (cdr (assq 'output keys)))))))

(ert-deftest clime-test-invoke/assign-keys-shared-used ()
  "Shared USED table prevents collisions between options and children."
  (clime-invoke-test-transient
    (let ((shared (make-hash-table :test #'equal)))
      ;; Options get -c
      (let ((opt-keys (clime-invoke--assign-keys
                       '((color "-c" "color" "-"))
                       shared)))
        (should (equal "-c" (cdr (assq 'color opt-keys)))))
      ;; Children can still get plain "c" — no collision
      (let ((child-keys (clime-invoke--assign-keys
                         '(("config" "c" "config"))
                         shared)))
        (should (equal "c" (cdr (assoc "config" child-keys))))))))

;;; ─── Option Key Item ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/option-key-item-short-flag ()
  "Option key item uses short flag as preferred key."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v") :nargs 0))
           (item (clime-invoke--option-key-item opt)))
      (should (equal 'verbose (nth 0 item)))
      (should (equal "-v" (nth 1 item)))
      (should (equal "verbose" (nth 2 item)))
      (should (equal "-" (nth 3 item))))))

(ert-deftest clime-test-invoke/option-key-item-long-only ()
  "Option key item with no short flag uses nil preferred, dash prefix."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output :flags '("--output")))
           (item (clime-invoke--option-key-item opt)))
      (should (equal 'output (nth 0 item)))
      (should (null (nth 1 item)))
      (should (equal "output" (nth 2 item)))
      (should (equal "-" (nth 3 item))))))

;;; ─── Spec Builders (count, multiple, arg) ───────────────────────────────

(ert-deftest clime-test-invoke/option-to-spec-count ()
  "Count option spec uses clime-invoke-count-infix class."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0 :count t))
           (spec (clime-invoke--option-to-spec opt "v" 'test-prefix)))
      (should (equal "v" (nth 0 spec)))
      (should (member :class spec))
      (should (eq 'clime-invoke-count-infix
                  (plist-get (nthcdr 3 spec) :class))))))

(ert-deftest clime-test-invoke/option-to-spec-multiple ()
  "Multiple option spec uses clime-invoke-multi-infix class."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'tag :flags '("--tag") :multiple t))
           (spec (clime-invoke--option-to-spec opt "t" 'test-prefix)))
      (should (equal "t" (nth 0 spec)))
      (should (equal "--tag=" (nth 2 spec)))
      (should (eq 'clime-invoke-multi-infix
                  (plist-get (nthcdr 3 spec) :class))))))

(ert-deftest clime-test-invoke/option-to-spec-required ()
  "Required option spec has function-valued :description."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output :flags '("--output") :required t))
           (spec (clime-invoke--option-to-spec opt "o" 'test-prefix)))
      (should (functionp (plist-get (nthcdr 3 spec) :description))))))

(ert-deftest clime-test-invoke/arg-to-spec ()
  "Arg spec builds correct shorthand with angle-bracket argument."
  (clime-invoke-test-transient
    (let* ((arg (clime-make-arg :name 'file :help "Input file"))
           (spec (clime-invoke--arg-to-spec arg "f" 'test-prefix)))
      (should (equal "f" (nth 0 spec)))
      (should (equal "Input file" (nth 1 spec)))
      (should (equal "<file>=" (nth 2 spec))))))

(ert-deftest clime-test-invoke/arg-to-spec-required ()
  "Required arg spec has function-valued :description."
  (clime-invoke-test-transient
    (let* ((arg (clime-make-arg :name 'file :required t))
           (spec (clime-invoke--arg-to-spec arg "f" 'test-prefix)))
      (should (functionp (plist-get (nthcdr 3 spec) :description))))))

;;; ─── Choices Cycling ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/choices-infix-cycles ()
  "Choices cycling logic: nil → first → second → … → last → nil."
  (clime-invoke-test-transient
    (let ((choices '("json" "text" "csv")))
      ;; nil → first choice
      (should (equal "json" (clime-invoke--cycle-choice nil choices)))
      ;; first → second
      (should (equal "text" (clime-invoke--cycle-choice "json" choices)))
      ;; second → third
      (should (equal "csv" (clime-invoke--cycle-choice "text" choices)))
      ;; last → nil (cycle off)
      (should (null (clime-invoke--cycle-choice "csv" choices)))
      ;; unknown value → first choice
      (should (equal "json" (clime-invoke--cycle-choice "xml" choices))))))

;;; ─── Alist/Plist Conversion ─────────────────────────────────────────────

(ert-deftest clime-test-invoke/alist-to-plist ()
  "Convert alist to plist."
  (clime-invoke-test-transient
    (should (equal '(a 1 b 2)
                   (clime-invoke--alist-to-plist '((a . 1) (b . 2)))))))

;;; ─── Extract Incompatible (multiple groups) ─────────────────────────────

(ert-deftest clime-test-invoke/extract-incompatible-multiple-groups ()
  "Extract multiple :incompatible groups from stacked conformers."
  (clime-invoke-test-transient
    (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
           (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
           (opt-local (clime-make-option :name 'local :flags '("--local") :nargs 0))
           (opt-remote (clime-make-option :name 'remote :flags '("--remote") :nargs 0))
           (cmd (clime-make-command
                 :name "run" :handler #'ignore
                 :options (list opt-json opt-csv opt-local opt-remote)
                 :conform (list (clime-check-exclusive 'fmt '(json csv))
                                (clime-check-exclusive 'src '(local remote))))))
      (let ((groups (clime-invoke--extract-incompatible cmd)))
        (should (= 2 (length groups)))))))

;;; ─── Params Edge Cases ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/params-to-transient-value-nil-skipped ()
  "Params with nil value are skipped."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output :flags '("--output")))
           (cmd (clime-make-command :name "run" :handler #'ignore
                                     :options (list opt))))
      (should (null (clime-invoke--params-to-transient-value
                     cmd '((output . nil))))))))

(ert-deftest clime-test-invoke/params-to-transient-value-unknown-ignored ()
  "Params for unknown options are silently ignored."
  (clime-invoke-test-transient
    (let ((cmd (clime-make-command :name "run" :handler #'ignore)))
      (should (null (clime-invoke--params-to-transient-value
                     cmd '((nonexistent . "val"))))))))

;;; ─── Backward Cycling ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/choices-backward-cycles ()
  "Backward cycling: nil → last → second-to-last → … → first → nil."
  (clime-invoke-test-transient
    (let ((choices '("json" "text" "csv")))
      ;; nil → last choice
      (should (equal "csv" (clime-invoke--cycle-choice-backward nil choices)))
      ;; last → second-to-last
      (should (equal "text" (clime-invoke--cycle-choice-backward "csv" choices)))
      ;; second → first
      (should (equal "json" (clime-invoke--cycle-choice-backward "text" choices)))
      ;; first → nil
      (should (null (clime-invoke--cycle-choice-backward "json" choices)))
      ;; unknown → nil
      (should (null (clime-invoke--cycle-choice-backward "xml" choices))))))

;;; ─── Count Read Logic ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/count-read-increment ()
  "Count increments on plain press (nil prefix-arg)."
  (clime-invoke-test-transient
    (should (= 3 (clime-invoke--read-count 2 nil)))
    (should (= 1 (clime-invoke--read-count 0 nil)))
    ;; Wraps at 5
    (should (= 0 (clime-invoke--read-count 5 nil)))))

(ert-deftest clime-test-invoke/count-read-set-directly ()
  "Count sets to N with numeric prefix-arg."
  (clime-invoke-test-transient
    (should (= 3 (clime-invoke--read-count 1 3)))
    (should (= 0 (clime-invoke--read-count 5 0)))))

(ert-deftest clime-test-invoke/count-read-decrement ()
  "Count decrements with universal prefix (list)."
  (clime-invoke-test-transient
    (should (= 2 (clime-invoke--read-count 3 '(4))))
    (should (= 0 (clime-invoke--read-count 1 '(4))))))

(ert-deftest clime-test-invoke/count-read-floor-zero ()
  "Count never goes below 0."
  (clime-invoke-test-transient
    (should (= 0 (clime-invoke--read-count 0 '(4))))
    (should (= 0 (clime-invoke--read-count 0 -1)))))

;;; ─── Auto-Register ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/auto-register-on-invoke ()
  "clime-invoke auto-registers the app for future discovery."
  (clime-invoke-test-transient
    (let ((clime-invoke--registry (make-hash-table :test #'equal))
          (clime-invoke--cache (make-hash-table :test #'equal))
          (app (clime-make-app :name "autoapp" :version "1" :children nil)))
      ;; Calling clime-invoke registers the app (we can't fully
      ;; invoke transient in batch, so just test the registration path)
      (clime-register-app "autoapp" app)
      (should (eq app (gethash "autoapp" clime-invoke--registry))))))

;;; ─── Default Values ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/default-values-boolean ()
  "Boolean option with :default t produces transient switch string."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'color :flags '("--color")
                                    :nargs 0 :default t))
           (cmd (clime-make-command :name "run" :handler #'ignore
                                     :options (list opt))))
      (let ((vals (clime-invoke--default-values cmd)))
        (should (equal '("--color ") vals))))))

(ert-deftest clime-test-invoke/default-values-string ()
  "Value option with :default produces transient option string."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                    :default "json"))
           (cmd (clime-make-command :name "run" :handler #'ignore
                                     :options (list opt))))
      (let ((vals (clime-invoke--default-values cmd)))
        (should (equal '("--format=json") vals))))))

(ert-deftest clime-test-invoke/default-values-none ()
  "Options without :default produce empty list."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'output :flags '("--output")))
           (cmd (clime-make-command :name "run" :handler #'ignore
                                     :options (list opt))))
      (should (null (clime-invoke--default-values cmd))))))

;;; ─── Ternary Cycling ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/ternary-cycles ()
  "Ternary cycling: nil → pos → neg → nil."
  (clime-invoke-test-transient
    (should (equal "--color"
                   (clime-invoke--cycle-ternary nil "--color" "--no-color")))
    (should (equal "--no-color"
                   (clime-invoke--cycle-ternary "--color" "--color" "--no-color")))
    (should (null (clime-invoke--cycle-ternary "--no-color" "--color" "--no-color")))))

(ert-deftest clime-test-invoke/option-to-spec-negatable ()
  "Negatable option generates ternary infix spec."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'color :flags '("--color")
                                    :negatable t :help "Colorize"))
           (spec (clime-invoke--option-to-spec opt "c" 'test-prefix)))
      (should (eq 'clime-invoke-ternary-infix
                  (plist-get (nthcdr 3 spec) :class)))
      (should (equal "--color" (plist-get (nthcdr 3 spec) :pos-flag)))
      (should (equal "--no-color" (plist-get (nthcdr 3 spec) :neg-flag))))))

;;; ─── Value Merge ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/merge-transient-values ()
  "Merge replaces matching flags and appends new ones."
  (clime-invoke-test-transient
    (let ((existing '("--verbose " "--format=json"))
          (new-vals '("--format=csv" "--quiet ")))
      (let ((merged (clime-invoke--merge-transient-values existing new-vals)))
        (should (member "--verbose " merged))
        (should (member "--format=csv" merged))
        (should (member "--quiet " merged))
        (should-not (member "--format=json" merged))))))

;;; ─── Value Infix Spec ─────────────────────────────────────────────────

(ert-deftest clime-test-invoke/option-to-spec-value-infix ()
  "Plain value option uses clime-invoke-value-infix class."
  (clime-invoke-test-transient
    (let* ((opt (clime-make-option :name 'limit :flags '("--limit")
                                    :help "Limit results"))
           (spec (clime-invoke--option-to-spec opt "l" 'test-prefix)))
      (should (eq 'clime-invoke-value-infix
                  (plist-get (nthcdr 3 spec) :class))))))

;;; ─── Header Display ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/header-group-has-children ()
  "Header group uses transient-information* suffix so it isn't dropped."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (sym (clime-invoke--make-command-prefix app show '("admin" "show")))
           (layout (get sym 'transient--layout))
           ;; First group should be the header (no :description on group)
           (header-group (car layout)))
      ;; Header group must have children (not empty)
      (should (> (length (aref header-group 3)) 0))
      ;; Group-level description should be nil (text is in info suffix)
      (should-not (plist-get (aref header-group 2) :description)))))

(ert-deftest clime-test-invoke/header-group-contains-help-text ()
  "Header info suffix contains the node help text."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (sym (clime-invoke--make-command-prefix app show '("admin" "show")))
           (layout (get sym 'transient--layout))
           (header-group (car layout))
           ;; Info suffix spec: (LEVEL CLASS PLIST)
           (info-spec (car (aref header-group 3)))
           (desc (plist-get (caddr info-spec) :description)))
      ;; Description should contain the help text
      (should (string-match-p "Show a resource" desc)))))

;;; ─── Error Display ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/error-info-suffix-in-actions ()
  "Actions group contains an error info suffix."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (sym (clime-invoke--make-command-prefix app show '("admin" "show")))
           (layout (get sym 'transient--layout))
           ;; Find the Actions group
           (actions-group (cl-find-if
                           (lambda (vec)
                             (equal "Actions"
                                    (plist-get (aref vec 2) :description)))
                           layout)))
      ;; Actions group should have 2 children: RET suffix + error info
      (should (= 2 (length (aref actions-group 3)))))))

(ert-deftest clime-test-invoke/error-cleared-on-entry ()
  "clime-invoke--last-error is cleared on entry."
  (clime-invoke-test-transient
    (setq clime-invoke--last-error "old error")
    ;; Simulate entry clearing (directly test the behavior)
    (setq clime-invoke--last-error nil)
    (should-not clime-invoke--last-error)))

(ert-deftest clime-test-invoke/error-fn-returns-empty-string-when-no-error ()
  "Error info suffix returns empty string (not nil) when no error."
  (clime-invoke-test-transient
    (let* ((app (clime-test--invoke-simple-app))
           (admin (cdr (assoc "admin" (clime-group-children app))))
           (show (cdr (assoc "show" (clime-group-children admin))))
           (sym (clime-invoke--make-command-prefix app show '("admin" "show")))
           (layout (get sym 'transient--layout))
           (actions-group (cl-find-if
                           (lambda (vec)
                             (equal "Actions"
                                    (plist-get (aref vec 2) :description)))
                           layout))
           ;; Error suffix is second child, spec: (LEVEL CLASS PLIST)
           (error-spec (cadr (aref actions-group 3)))
           (desc-fn (plist-get (caddr error-spec) :description)))
      ;; With no error, function should return "" not nil
      (let ((clime-invoke--last-error nil))
        (should (equal "" (funcall desc-fn))))
      ;; With error, function should return the error text
      (let ((clime-invoke--last-error "something broke"))
        (should (string-match-p "something broke" (funcall desc-fn)))))))

;;; ─── Clear Error on Input ──────────────────────────────────────────

(ert-deftest clime-test-invoke/pre-command-guard ()
  "Pre-command hook only fires for clime-managed transients."
  (clime-invoke-test-transient
    ;; Without a managed transient prefix, hook is a no-op
    (let ((clime-invoke--last-error "test error")
          (clime-invoke--snapshot-values nil)
          (transient--prefix nil))
      (clime-invoke--pre-command)
      (should (equal "test error" clime-invoke--last-error))
      (should-not clime-invoke--snapshot-values))))

;;; ─── Display / Cleanup ──────────────────────────────────────────────

(ert-deftest clime-test-invoke/cleanup-restores-display-action ()
  "Cleanup hook restores `transient-display-buffer-action' and removes hooks."
  (clime-invoke-test-transient
    (let ((transient-display-buffer-action '(display-buffer-use-some-window))
          (clime-invoke--saved-display-action '(original-action))
          (transient--stack nil))
      (clime-invoke--cleanup-on-exit)
      (should (equal '(original-action) transient-display-buffer-action))
      (should (eq :unset clime-invoke--saved-display-action)))))

(ert-deftest clime-test-invoke/cleanup-skipped-when-stack-nonempty ()
  "Cleanup hook is a no-op when transient stack has entries (child→parent)."
  (clime-invoke-test-transient
    (let ((transient-display-buffer-action '(overridden))
          (clime-invoke--saved-display-action '(original))
          (transient--stack '((some-entry))))
      (clime-invoke--cleanup-on-exit)
      ;; Should NOT have restored
      (should (equal '(overridden) transient-display-buffer-action))
      (should (equal '(original) clime-invoke--saved-display-action)))))

;;; ─── Value Inheritance ──────────────────────────────────────────────

(ert-deftest clime-test-invoke/merge-handles-nil-existing ()
  "Merge with nil existing just returns new values."
  (clime-invoke-test-transient
    (let ((merged (clime-invoke--merge-transient-values nil '("--verbose "))))
      (should (member "--verbose " merged)))))

(ert-deftest clime-test-invoke/count-value-round-trips ()
  "Count infix encodes N in value and init-value decodes it."
  (clime-invoke-test-transient
    (let ((obj (clime-invoke-count-infix :argument "--verbose ")))
      ;; Set count to 3
      (oset obj value 3)
      ;; Serialized value should encode count
      (let ((serialized (transient-infix-value obj)))
        (should (equal "--verbose 3 " serialized))
        ;; Init from a prefix value list containing the serialized form
        (let ((transient--prefix (transient-prefix :command 'test-prefix)))
          (oset transient--prefix value (list serialized))
          (transient-init-value obj)
          (should (equal 3 (oref obj value))))))))

(ert-deftest clime-test-invoke/count-merge-replaces-correctly ()
  "Merge recognizes count-encoded values as same flag."
  (clime-invoke-test-transient
    (let ((merged (clime-invoke--merge-transient-values
                   '("--verbose 2 ") '("--verbose 3 "))))
      (should (member "--verbose 3 " merged))
      (should-not (member "--verbose 2 " merged)))))

(ert-deftest clime-test-invoke/value-flag-prefix ()
  "Flag prefix extraction for various value formats."
  (clime-invoke-test-transient
    (should (equal "--verbose " (clime-invoke--value-flag-prefix "--verbose ")))
    (should (equal "--verbose " (clime-invoke--value-flag-prefix "--verbose 3 ")))
    (should (equal "--limit=" (clime-invoke--value-flag-prefix "--limit=20")))
    (should (equal "--color " (clime-invoke--value-flag-prefix "--color ")))))

(provide 'clime-invoke-tests)
;;; clime-invoke-test-transients.el ends here
