;;; clime-invoke-tests.el --- Tests for clime-invoke  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the interactive menu invoker.
;; Covers: key assignment, params model, option cycling,
;; rendering, navigation, and run integration.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-test-helpers)

(require 'clime-invoke)

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

(defun clime-test--invoke-full-app ()
  "Build an app exercising all option types."
  (let* ((opt-verbose (clime-make-option :name 'verbose :flags '("-v")
                                          :nargs 0 :count t
                                          :help "Verbosity level"))
         (opt-color (clime-make-option :name 'color :flags '("--color")
                                        :negatable t
                                        :help "Colorize output"))
         (opt-format (clime-make-option :name 'format :flags '("--format")
                                         :choices '("json" "text" "csv")
                                         :help "Output format"))
         (opt-limit (clime-make-option :name 'limit :flags '("--limit")
                                        :help "Limit results"))
         (opt-tag (clime-make-option :name 'tag :flags '("--tag")
                                      :multiple t
                                      :help "Tags"))
         (cmd-list (clime-make-command :name "list" :handler #'ignore
                                        :help "List items"
                                        :options (list opt-limit)))
         (cmd-show (clime-make-command :name "show" :handler #'ignore
                                        :help "Show item")))
    (clime-make-app :name "testapp" :version "1.0"
                    :help "Test application"
                    :options (list opt-verbose opt-color opt-format opt-tag)
                    :children `(("list" . ,cmd-list)
                                ("show" . ,cmd-show)))))

;;; ─── Key Assignment ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/key-from-short-flag ()
  "Short flag -v produces key \"v\"."
  (let ((opt (clime-make-option :name 'verbose
                                 :flags '("--verbose" "-v")
                                 :nargs 0)))
    (should (equal "v" (clime-invoke--preferred-key opt)))))

(ert-deftest clime-test-invoke/key-from-long-flag ()
  "Long flag --output (no short) produces key \"o\"."
  (let ((opt (clime-make-option :name 'output
                                 :flags '("--output"))))
    (should (equal "o" (clime-invoke--preferred-key opt)))))

(ert-deftest clime-test-invoke/assign-keys-no-collision ()
  "Assign unique keys to a set of options."
  (let* ((opts (list (clime-make-option :name 'verbose :flags '("--verbose" "-v") :nargs 0)
                     (clime-make-option :name 'output :flags '("--output" "-o"))
                     (clime-make-option :name 'format :flags '("--format" "-f"))))
         (keys (clime-invoke--assign-keys
                (mapcar #'clime-invoke--option-key-item opts))))
    (should (= 3 (length keys)))
    (should (= 3 (length (delete-dups (mapcar #'cdr keys)))))))

(ert-deftest clime-test-invoke/assign-keys-collision ()
  "When two options want the same key, second gets a different one."
  (let* ((opts (list (clime-make-option :name 'verbose :flags '("--verbose" "-v") :nargs 0)
                     (clime-make-option :name 'version :flags '("--version") :nargs 0)))
         (keys (clime-invoke--assign-keys
                (mapcar #'clime-invoke--option-key-item opts))))
    (should (= 2 (length keys)))
    (should-not (equal (cdr (assq 'verbose keys))
                       (cdr (assq 'version keys))))))

(ert-deftest clime-test-invoke/assign-keys-empty ()
  "Empty options list returns empty key map."
  (should (null (clime-invoke--assign-keys nil))))

(ert-deftest clime-test-invoke/assign-keys-generic ()
  "Unified assign-keys works with generic (NAME PREFERRED FALLBACK) items."
  (let ((keys (clime-invoke--assign-keys
               '((a "x" "abc")
                 (b "x" "bcd")
                 (c nil "cde")))))
    (should (= 3 (length keys)))
    (should (equal "x" (cdr (assq 'a keys))))
    (should (equal "b" (cdr (assq 'b keys))))
    (should (equal "c" (cdr (assq 'c keys))))))

;;; ─── Choices Cycling ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/choices-cycles ()
  "Choices cycling: nil → first → second → … → last → nil."
  (let ((choices '("json" "text" "csv")))
    (should (equal "json" (clime-invoke--cycle-choice nil choices)))
    (should (equal "text" (clime-invoke--cycle-choice "json" choices)))
    (should (equal "csv" (clime-invoke--cycle-choice "text" choices)))
    (should (null (clime-invoke--cycle-choice "csv" choices)))
    (should (equal "json" (clime-invoke--cycle-choice "xml" choices)))))

(ert-deftest clime-test-invoke/choices-backward-cycles ()
  "Backward cycling: nil → last → second-to-last → … → first → nil."
  (let ((choices '("json" "text" "csv")))
    (should (equal "csv" (clime-invoke--cycle-choice-backward nil choices)))
    (should (equal "text" (clime-invoke--cycle-choice-backward "csv" choices)))
    (should (equal "json" (clime-invoke--cycle-choice-backward "text" choices)))
    (should (null (clime-invoke--cycle-choice-backward "json" choices)))))

;;; ─── Ternary Cycling ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/ternary-cycles ()
  "Ternary cycling: nil → pos → neg → nil."
  (should (equal "--color"
                 (clime-invoke--cycle-ternary nil "--color" "--no-color")))
  (should (equal "--no-color"
                 (clime-invoke--cycle-ternary "--color" "--color" "--no-color")))
  (should (null (clime-invoke--cycle-ternary "--no-color" "--color" "--no-color"))))

;;; ─── Count Read Logic ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/count-read-increment ()
  "Count increments on plain press, wraps at 5."
  (should (= 3 (clime-invoke--read-count 2 nil)))
  (should (= 1 (clime-invoke--read-count 0 nil)))
  (should (= 0 (clime-invoke--read-count 5 nil))))

(ert-deftest clime-test-invoke/count-read-set-directly ()
  "Count sets to N with numeric prefix-arg."
  (should (= 3 (clime-invoke--read-count 1 3)))
  (should (= 0 (clime-invoke--read-count 5 0))))

(ert-deftest clime-test-invoke/count-read-decrement ()
  "Count decrements with universal prefix."
  (should (= 2 (clime-invoke--read-count 3 '(4))))
  (should (= 0 (clime-invoke--read-count 1 '(4)))))

(ert-deftest clime-test-invoke/count-read-floor-zero ()
  "Count never goes below 0."
  (should (= 0 (clime-invoke--read-count 0 '(4))))
  (should (= 0 (clime-invoke--read-count 0 -1))))

;;; ─── App Registry ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/register-app ()
  "Registering an app makes it discoverable."
  (let ((clime-invoke--registry (make-hash-table :test #'equal)))
    (let ((app (clime-make-app :name "testapp" :version "1.0" :children nil)))
      (clime-register-app "testapp" app)
      (should (eq app (gethash "testapp" clime-invoke--registry))))))

(ert-deftest clime-test-invoke/registry-list ()
  "Registered apps are enumerable."
  (let ((clime-invoke--registry (make-hash-table :test #'equal)))
    (let ((app1 (clime-make-app :name "alpha" :version "1" :children nil))
          (app2 (clime-make-app :name "beta" :version "1" :children nil)))
      (clime-register-app "alpha" app1)
      (clime-register-app "beta" app2)
      (let ((keys (clime-invoke--registry-keys)))
        (should (= 2 (length keys)))
        (should (member "alpha" keys))
        (should (member "beta" keys))))))

;;; ─── Params Model ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/params-shared-across-navigation ()
  "Params set at any node persist — single plist, no push/pop."
  (let ((params '(verbose 3 limit 20)))
    ;; Both visible regardless of which node we're rendering
    (should (equal 3 (plist-get params 'verbose)))
    (should (equal 20 (plist-get params 'limit)))))

(ert-deftest clime-test-invoke/params-unset-not-present ()
  "Unset options are not in the params plist."
  (let ((params '(verbose 2)))
    (should (equal 2 (plist-get params 'verbose)))
    (should-not (plist-member params 'limit))))

;;; ─── Rendering ─────────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/render-shows-options ()
  "Rendered buffer contains option keys, descriptions, and values."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                  :nargs 0 :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'verbose t 'user))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "Be verbose" content))))

(ert-deftest clime-test-invoke/render-shows-children ()
  "Rendered buffer contains child names."
  (let* ((cmd (clime-make-command :name "show" :help "Show resource"
                                   :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,cmd))))
         (content (clime-invoke--render-to-string app nil nil)))
    (should (string-match-p "show" content))
    (should (string-match-p "Show resource" content))))

(ert-deftest clime-test-invoke/render-shows-count-level ()
  "Count option displays ×N."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v")
                                  :nargs 0 :count t :help "Verbosity"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'verbose 3 'user))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "×3" content))))

(ert-deftest clime-test-invoke/render-shows-error ()
  "Error message appears in rendered output."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string
                   cmd nil "Exit 2: unknown flag")))
    (should (string-match-p "Exit 2" content))))

(ert-deftest clime-test-invoke/render-shows-help-text ()
  "Node help text appears at the top."
  (let* ((cmd (clime-make-command :name "deploy" :handler #'ignore
                                   :help "Deploy to production"))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "Deploy to production" content))))

(ert-deftest clime-test-invoke/render-hides-hidden-children ()
  "Hidden children are not rendered."
  (let* ((app (clime-test--invoke-simple-app))
         (admin (cdr (assoc "admin" (clime-group-children app))))
         (content (clime-invoke--render-to-string admin nil nil)))
    (should (string-match-p "show" content))
    (should (string-match-p "create" content))
    (should-not (string-match-p "debug" content))))

;;; ─── Display Key ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/display-key-option ()
  "Option key \"- v\" displays as \"-v\"."
  (should (equal "-v" (clime-invoke--display-key "- v"))))

(ert-deftest clime-test-invoke/display-key-plain ()
  "Plain key passes through."
  (should (equal "s" (clime-invoke--display-key "s")))
  (should (equal "RET" (clime-invoke--display-key "RET"))))

;;; ─── Sub-Builders ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/build-child-actions ()
  "Child actions use plain letter keys."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,cmd))))
         (used (make-hash-table :test #'equal))
         (actions (clime-invoke--build-child-actions app used)))
    (should (= 1 (length actions)))
    (should (eq :child (car (cdr (car actions)))))
    ;; Key should be reserved in used table
    (should (gethash (caar actions) used))))

(ert-deftest clime-test-invoke/build-arg-actions ()
  "Arg actions get keys from arg name."
  (let* ((arg (clime-make-arg :name 'file :help "File"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (used (make-hash-table :test #'equal))
         (actions (clime-invoke--build-arg-actions cmd used)))
    (should (= 1 (length actions)))
    (should (eq :arg (car (cdr (car actions)))))))

(ert-deftest clime-test-invoke/build-option-actions ()
  "Option actions use \"- X\" namespace."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (actions (clime-invoke--build-option-actions cmd)))
    (should (= 1 (length actions)))
    (should (string-prefix-p "- " (caar actions)))
    (should (eq :option (car (cdr (car actions)))))))

(ert-deftest clime-test-invoke/build-option-actions-hidden ()
  "Hidden options are excluded."
  (let* ((opt (clime-make-option :name 'debug :flags '("--debug")
                                  :nargs 0 :hidden t))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (actions (clime-invoke--build-option-actions cmd)))
    (should (null actions))))

;;; ─── Prefix Key ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/read-prefixed-key-timeout ()
  "Prefix read returns nil on timeout."
  (let ((clime-invoke-prefix-timeout 0.01))
    (should (null (clime-invoke--read-prefixed-key "-")))))

;;; ─── Build Key Map ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/keymap-children-first ()
  "Children get plain letter keys, options use \"- X\" namespace."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (cmd (clime-make-command :name "view" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :options (list opt)
                               :children `(("view" . ,cmd))))
         (km (clime-invoke--build-key-map app)))
    ;; "v" goes to the child (priority), not the option
    (should (eq :child (cadr (assoc "v" km))))
    ;; Option gets "- v" namespace
    (should (assoc "- v" km))))

(ert-deftest clime-test-invoke/keymap-ret-for-handler ()
  "RET action present only when node has a handler."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (grp (clime-make-group :name "grp" :children `(("run" . ,cmd)))))
    (should (assoc "RET" (clime-invoke--build-key-map cmd)))
    (should-not (assoc "RET" (clime-invoke--build-key-map grp)))))

(ert-deftest clime-test-invoke/keymap-args-included ()
  "Positional args get key assignments."
  (let* ((arg (clime-make-arg :name 'file :help "File"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (km (clime-invoke--build-key-map cmd)))
    (should (cl-find-if (lambda (e) (eq (car e) :arg)) (mapcar #'cdr km)))))

;;; ─── Handle Option ───────────────────────────────────────────────────

(ert-deftest clime-test-invoke/handle-boolean-toggle ()
  "Boolean option toggles on/off."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values '()))
    ;; nil → t
    (clime-invoke--handle-option opt)
    (should (eq t (clime-values-value clime-invoke--values 'verbose)))
    ;; t → nil
    (clime-invoke--handle-option opt)
    (should-not (clime-values-value clime-invoke--values 'verbose))))

(ert-deftest clime-test-invoke/handle-count-increment ()
  "Count option increments."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t))
        (clime-invoke--values '()))
    ;; nil → 1
    (clime-invoke--handle-option opt)
    (should (= 1 (clime-values-value clime-invoke--values 'verbose)))
    ;; 1 → 2 → 3 (set to 2 then increment)
    (setq clime-invoke--values (clime-values-set clime-invoke--values 'verbose 2 'user))
    (clime-invoke--handle-option opt)
    (should (= 3 (clime-values-value clime-invoke--values 'verbose)))))

(ert-deftest clime-test-invoke/handle-choices-cycle ()
  "Choices option cycles through values."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text")))
        (clime-invoke--values '()))
    ;; nil → "json"
    (clime-invoke--handle-option opt)
    (should (equal "json" (clime-values-value clime-invoke--values 'format)))
    ;; "json" → "text"
    (clime-invoke--handle-option opt)
    (should (equal "text" (clime-values-value clime-invoke--values 'format)))
    ;; "text" → nil
    (clime-invoke--handle-option opt)
    (should-not (clime-values-value clime-invoke--values 'format))))

(ert-deftest clime-test-invoke/handle-ternary-cycle ()
  "Negatable option cycles nil → flag → no-flag → nil."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t))
        (clime-invoke--values '()))
    ;; nil → "--color"
    (clime-invoke--handle-option opt)
    (should (equal "--color" (clime-values-value clime-invoke--values 'color)))
    ;; "--color" → "--no-color"
    (clime-invoke--handle-option opt)
    (should (equal "--no-color" (clime-values-value clime-invoke--values 'color)))
    ;; "--no-color" → nil
    (clime-invoke--handle-option opt)
    (should-not (clime-values-value clime-invoke--values 'color))))

(ert-deftest clime-test-invoke/handle-ternary-short-flag-first ()
  "Negatable option works when short flag listed before long flag."
  (let ((opt (clime-make-option :name 'color :flags '("-c" "--color")
                                 :negatable t))
        (clime-invoke--values '()))
    ;; nil → "--color"
    (clime-invoke--handle-option opt)
    (should (equal "--color" (clime-values-value clime-invoke--values 'color)))
    ;; "--color" → "--no-color"
    (clime-invoke--handle-option opt)
    (should (equal "--no-color" (clime-values-value clime-invoke--values 'color)))
    ;; "--no-color" → nil
    (clime-invoke--handle-option opt)
    (should-not (clime-values-value clime-invoke--values 'color))))

;;; ─── Handle Option Direct ────────────────────────────────────────────

(ert-deftest clime-test-invoke/direct-choices-completing-read ()
  "Direct input on choices option uses completing-read."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text" "csv")))
        (clime-invoke--values '()))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _) (car (last coll)))))
      (clime-invoke--handle-option-direct opt)
      (should (equal "csv" (clime-values-value clime-invoke--values 'format))))))

(ert-deftest clime-test-invoke/direct-choices-empty-clears ()
  "Direct input on choices with empty string clears value."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text")))
        (clime-invoke--values (clime-values-set '() 'format "json" 'user)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "")))
      (clime-invoke--handle-option-direct opt)
      (should-not (clime-values-value clime-invoke--values 'format)))))

(ert-deftest clime-test-invoke/direct-count-sets-value ()
  "Direct input on count option sets specific number."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t))
        (clime-invoke--values '()))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "3")))
      (clime-invoke--handle-option-direct opt)
      (should (= 3 (clime-values-value clime-invoke--values 'verbose))))))

(ert-deftest clime-test-invoke/direct-count-zero-clears ()
  "Direct input on count with 0 clears value."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t))
        (clime-invoke--values (clime-values-set '() 'verbose 3 'user)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "0")))
      (clime-invoke--handle-option-direct opt)
      (should-not (clime-values-value clime-invoke--values 'verbose)))))

(ert-deftest clime-test-invoke/direct-count-invalid-errors ()
  "Direct input on count with non-numeric input signals error."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "abc")))
      (should-error (clime-invoke--handle-option-direct opt)))))

(ert-deftest clime-test-invoke/direct-boolean-delegates ()
  "Direct input on boolean delegates to cycling handler."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values '()))
    (clime-invoke--handle-option-direct opt)
    (should (eq t (clime-values-value clime-invoke--values 'verbose)))))

(ert-deftest clime-test-invoke/direct-negatable-delegates ()
  "Direct input on negatable delegates to cycling handler."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t))
        (clime-invoke--values '()))
    (clime-invoke--handle-option-direct opt)
    (should (equal "--color" (clime-values-value clime-invoke--values 'color)))))

(ert-deftest clime-test-invoke/direct-count-negative-errors ()
  "Direct input on count rejects negative numbers."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "-1")))
      (should-error (clime-invoke--handle-option-direct opt)))))

(ert-deftest clime-test-invoke/direct-count-over-max-errors ()
  "Direct input on count rejects values > 5."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "6")))
      (should-error (clime-invoke--handle-option-direct opt)))))

(ert-deftest clime-test-invoke/direct-multiple-delegates ()
  "Direct input on multiple option delegates to cycling handler."
  (let ((opt (clime-make-option :name 'tag :flags '("--tag")
                                 :multiple t))
        (clime-invoke--values '()))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "foo")))
      (clime-invoke--handle-option-direct opt)
      (should (equal '("foo") (clime-values-value clime-invoke--values 'tag))))))

(ert-deftest clime-test-invoke/direct-plain-delegates ()
  "Direct input on plain value option delegates to cycling handler."
  (let ((opt (clime-make-option :name 'output :flags '("--output")))
        (clime-invoke--values '()))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "/tmp/out")))
      (clime-invoke--handle-option-direct opt)
      (should (equal "/tmp/out" (clime-values-value clime-invoke--values 'output))))))

;;; ─── Format Value ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-value-boolean ()
  "Boolean shows on/off."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values (clime-values-set '() 'verbose t 'user)))
    (should (string-match-p "on" (clime-invoke--format-value opt))))
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values '()))
    (should (string-match-p "off" (clime-invoke--format-value opt)))))

(ert-deftest clime-test-invoke/format-value-ternary-short-first ()
  "Ternary display works when short flag listed first."
  (let ((opt (clime-make-option :name 'color :flags '("-c" "--color")
                                 :negatable t))
        (clime-invoke--values (clime-values-set '() 'color "--color" 'user)))
    (should (string-match-p "on" (clime-invoke--format-value opt))))
  (let ((opt (clime-make-option :name 'color :flags '("-c" "--color")
                                 :negatable t))
        (clime-invoke--values (clime-values-set '() 'color "--no-color" 'user)))
    (should (string-match-p "off" (clime-invoke--format-value opt))))
  (let ((opt (clime-make-option :name 'color :flags '("-c" "--color")
                                 :negatable t))
        (clime-invoke--values '()))
    (should (string-match-p "auto" (clime-invoke--format-value opt)))))

(ert-deftest clime-test-invoke/format-value-count ()
  "Count shows ×N or off."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t))
        (clime-invoke--values (clime-values-set '() 'verbose 3 'user)))
    (should (string-match-p "×3" (clime-invoke--format-value opt))))
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t))
        (clime-invoke--values '()))
    (should (string-match-p "off" (clime-invoke--format-value opt)))))

(ert-deftest clime-test-invoke/format-value-ternary ()
  "Ternary shows on/off/auto."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t))
        (clime-invoke--values (clime-values-set '() 'color "--color" 'user)))
    (should (string-match-p "on" (clime-invoke--format-value opt))))
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t))
        (clime-invoke--values (clime-values-set '() 'color "--no-color" 'user)))
    (should (string-match-p "off" (clime-invoke--format-value opt))))
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t))
        (clime-invoke--values '()))
    (should (string-match-p "auto" (clime-invoke--format-value opt)))))

(ert-deftest clime-test-invoke/format-value-choices ()
  "Choices show all options inline; selected highlighted."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text")))
        (clime-invoke--values (clime-values-set '() 'format "json" 'user)))
    (let ((with-val (clime-invoke--format-value opt)))
      (should (string-match-p "json" with-val))
      (should (string-match-p "text" with-val))))
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text")))
        (clime-invoke--values '()))
    (let ((without (clime-invoke--format-value opt)))
      (should (string-match-p "json" without))
      (should (string-match-p "text" without)))))

(ert-deftest clime-test-invoke/format-value-choices-inline ()
  "Choices displayed inline with all options visible."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text" "csv")))
        (clime-invoke--values (clime-values-set '() 'format "json" 'user)))
    (let ((val-str (clime-invoke--format-value opt)))
      ;; All choices appear
      (should (string-match-p "json" val-str))
      (should (string-match-p "text" val-str))
      (should (string-match-p "csv" val-str)))))

(ert-deftest clime-test-invoke/format-value-choices-default-parens ()
  "Default value in choices list shown in parens."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text" "csv")
                                 :default "text")))
    (let ((val-str (clime-invoke--format-value opt)))
      ;; Default shown in parens, not highlighted
      (should (string-match-p "(text)" val-str)))))

(ert-deftest clime-test-invoke/format-value-default-no-choices ()
  "Default value without choices shown in parens."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")
                                 :default "10")))
    (let ((val-str (clime-invoke--format-value opt)))
      (should (string-match-p "(10)" val-str)))))

(ert-deftest clime-test-invoke/format-value-explicit-has-active-face ()
  "Explicitly set value has clime-invoke-active face."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")))
        (clime-invoke--values (clime-values-set '() 'limit "20" 'user)))
    (let ((val-str (clime-invoke--format-value opt)))
      (should (string-match-p "20" val-str))
      ;; Check the face property
      (let ((pos (string-match "20" val-str)))
        (should (eq 'clime-invoke-active
                    (get-text-property pos 'face val-str)))))))

(ert-deftest clime-test-invoke/format-value-default-no-active-face ()
  "Default value does NOT have clime-invoke-active face."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")
                                 :default "10")))
    (let ((val-str (clime-invoke--format-value opt)))
      (let ((pos (string-match "10" val-str)))
        (should-not (eq 'clime-invoke-active
                        (get-text-property pos 'face val-str)))))))

(ert-deftest clime-test-invoke/format-value-choices-truncated ()
  "Large choices list truncated around selected value."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :choices '("red" "orange" "yellow" "green"
                                            "blue" "indigo" "violet")))
        (clime-invoke--values (clime-values-set '() 'color "blue" 'user)))
    (let ((val-str (clime-invoke--format-value opt)))
      ;; Selected value visible
      (should (string-match-p "blue" val-str))
      ;; Ellipsis indicates truncation
      (should (string-match-p "\\.\\.\\." val-str)))))

(ert-deftest clime-test-invoke/format-value-choices-no-value-no-default ()
  "Choices with no value and no default shows all unhighlighted."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text"))))
    (let ((val-str (clime-invoke--format-value opt)))
      (should (string-match-p "json" val-str))
      (should (string-match-p "text" val-str))
      ;; No active face on either
      (let ((pos (string-match "json" val-str)))
        (should-not (eq 'clime-invoke-active
                        (get-text-property pos 'face val-str)))))))

(ert-deftest clime-test-invoke/format-value-choices-five-no-truncation ()
  "Exactly 5 choices: no truncation."
  (let ((opt (clime-make-option :name 'c :flags '("--c")
                                 :choices '("a" "b" "c" "d" "e")))
        (clime-invoke--values (clime-values-set '() 'c "c" 'user)))
    (let ((val-str (clime-invoke--format-value opt)))
      (should (string-match-p "a" val-str))
      (should (string-match-p "e" val-str))
      (should-not (string-match-p "\\.\\.\\." val-str)))))

(ert-deftest clime-test-invoke/format-desc-arg ()
  "Format-desc works for args (not just options)."
  (let ((arg (clime-make-arg :name 'file :help "File path")))
    (let ((desc (clime-invoke--format-desc arg)))
      (should (string-match-p "File path" desc)))))

(ert-deftest clime-test-invoke/format-desc-arg-required ()
  "Format-desc shows required annotation for unset required arg."
  (let ((arg (clime-make-arg :name 'file :help "File path")))
    (let ((desc (clime-invoke--format-desc arg)))
      (should (string-match-p "(required)" desc)))))

(ert-deftest clime-test-invoke/header-root-no-version ()
  "Root without version shows name only."
  (let* ((app (clime-make-app :name "myapp" :children nil))
         (content (clime-invoke--render-to-string app nil nil t)))
    (should (string-match-p "myapp" content))))

;;; ─── Format Desc ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-desc-help-and-long-flag ()
  "Desc column shows help text with long flag in parens."
  (let ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                 :nargs 0 :help "Be verbose")))
    (let ((desc (clime-invoke--format-desc opt)))
      (should (string-match-p "Be verbose" desc))
      (should (string-match-p "(--verbose)" desc)))))

(ert-deftest clime-test-invoke/format-desc-no-help ()
  "Desc column shows long flag when no help text."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit"))))
    (let ((desc (clime-invoke--format-desc opt)))
      (should (string-match-p "--limit" desc)))))

(ert-deftest clime-test-invoke/format-desc-required ()
  "Desc column shows required annotation."
  (let ((opt (clime-make-option :name 'output :flags '("--output")
                                 :required t :help "Output file")))
    (let ((desc (clime-invoke--format-desc opt)))
      (should (string-match-p "(required)" desc)))))

(ert-deftest clime-test-invoke/format-desc-deprecated ()
  "Desc column shows deprecated annotation."
  (let ((opt (clime-make-option :name 'old :flags '("--old")
                                 :deprecated t :help "Old option")))
    (let ((desc (clime-invoke--format-desc opt)))
      (should (string-match-p "(deprecated)" desc)))))

(ert-deftest clime-test-invoke/format-desc-type-hint ()
  "Desc column shows type hint for non-string types."
  (let ((opt (clime-make-option :name 'count :flags '("--count")
                                 :type 'integer :help "Count")))
    (let ((desc (clime-invoke--format-desc opt)))
      (should (string-match-p "(integer)" desc)))))

(ert-deftest clime-test-invoke/format-desc-multi-hint ()
  "Desc column shows multi hint for multiple options."
  (let ((opt (clime-make-option :name 'tag :flags '("--tag")
                                 :multiple t :help "Tags")))
    (let ((desc (clime-invoke--format-desc opt)))
      (should (string-match-p "(multi)" desc)))))

;;; ─── Format Env ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-env-with-value ()
  "Env annotation shows just var name, no resolved value."
  (let ((opt (clime-make-option :name 'home :flags '("--home")
                                 :env "HOME")))
    (let ((env-str (clime-invoke--format-env opt)))
      (should (string-match-p "\\$HOME" env-str))
      ;; Simplified: no resolved value in env annotation
      (should-not (string-match-p "=" env-str)))))

(ert-deftest clime-test-invoke/format-env-empty ()
  "Env column shows just var name when env is unset."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo")
                                 :env "CLIME_TEST_NONEXISTENT_VAR_XYZ")))
    (let ((env-str (clime-invoke--format-env opt)))
      (should (string-match-p "\\$CLIME_TEST_NONEXISTENT_VAR_XYZ" env-str))
      (should-not (string-match-p "=" env-str)))))

(ert-deftest clime-test-invoke/format-env-always-shadow ()
  "Env annotation is always in shadow face (no active highlighting)."
  (let ((opt (clime-make-option :name 'home :flags '("--home")
                                 :env "HOME")))
    (let ((env-str (clime-invoke--format-env opt)))
      (should (eq 'shadow (get-text-property 0 'face env-str))))))

(ert-deftest clime-test-invoke/format-value-env-derived ()
  "Unset option with env var shows ($=value) in value column."
  (let* ((opt (clime-make-option :name 'home :flags '("--home")
                                  :env "HOME"))
         (app (clime-make-app :name "test" :version "1" :children nil))
         (clime-invoke--values '()))
    (let ((val-str (clime-invoke--format-value opt app)))
      ;; Should show env-derived value with ($=...)
      (should (string-match-p "(\\$=" val-str))
      (should (string-match-p "/" val-str)))))

(ert-deftest clime-test-invoke/format-env-nil-when-no-env ()
  "No env column when option has no :env slot."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (should (null (clime-invoke--format-env opt)))))

;;; ─── Breadcrumb Header ─────────────────────────────────────────────

(ert-deftest clime-test-invoke/header-root-shows-name-version ()
  "Root node header shows app name and version."
  (let* ((app (clime-make-app :name "myapp" :version "1.2.3"
                               :help "My app" :children nil))
         (content (clime-invoke--render-to-string app nil nil t)))
    (should (string-match-p "myapp" content))
    (should (string-match-p "1\\.2\\.3" content))))

(ert-deftest clime-test-invoke/header-breadcrumb-path ()
  "Nested node header shows breadcrumb path."
  (let* ((cmd (clime-make-command :name "show" :help "Show resource"
                                   :handler #'ignore))
         (grp (clime-make-group :name "admin" :help "Admin commands"
                                 :children `(("show" . ,cmd))))
         (app (clime-make-app :name "myapp" :version "1.0"
                               :children `(("admin" . ,grp))))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "myapp" content))
    (should (string-match-p "admin" content))
    (should (string-match-p "show" content))
    (should (string-match-p "Show resource" content))))

;;; ─── Actions Footer ────────────────────────────────────────────────

(ert-deftest clime-test-invoke/actions-section ()
  "Actions section groups RET and q on the same or adjacent lines."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil t)))
    (should (string-match-p "Actions" content))
    (should (string-match-p "RET" content))
    (should (string-match-p "Run" content))
    (should (string-match-p "Quit" content))))

;;; ─── Key Faces ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/render-option-key-face ()
  "Option keys use clime-invoke-option-key face."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (let ((pos (string-match "-v" content)))
      (should pos)
      (should (eq 'clime-invoke-option-key
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-command-key-face ()
  "Command keys use clime-invoke-command-key face."
  (let* ((cmd (clime-make-command :name "show" :handler #'ignore
                                   :help "Show"))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,cmd))))
         (content (clime-invoke--render-to-string app nil nil)))
    ;; Find "s" after "Commands" heading to avoid matching header text
    (let* ((cmds-pos (string-match "Commands" content))
           (pos (and cmds-pos (string-match "\\bs\\b" content (1+ cmds-pos)))))
      (should pos)
      (should (eq 'clime-invoke-command-key
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-action-key-face ()
  "Action keys (RET, ESC) use clime-invoke-action-key face."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil t)))
    (let ((pos (string-match "RET" content)))
      (should pos)
      (should (eq 'clime-invoke-action-key
                  (get-text-property pos 'face content))))
    (let ((pos (string-match "ESC" content)))
      (should pos)
      (should (eq 'clime-invoke-action-key
                  (get-text-property pos 'face content))))))

;;; ─── Visible Children ───────────────────────────────────────────────

(ert-deftest clime-test-invoke/visible-children-inline ()
  "Inline group children are promoted."
  (let* ((cmd1 (clime-make-command :name "a" :handler #'ignore))
         (cmd2 (clime-make-command :name "b" :handler #'ignore))
         (inline (clime-make-group :name "inl" :inline t
                                    :children `(("b" . ,cmd2))))
         (grp (clime-make-group :name "top"
                                 :children `(("a" . ,cmd1)
                                             ("inl" . ,inline))))
         (visible (clime-invoke--visible-children grp)))
    (should (= 2 (length visible)))
    (should (assoc "a" visible))
    (should (assoc "b" visible))))

;;; ─── Render Extras ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/render-shows-args ()
  "Positional args appear in rendered output."
  (let* ((arg (clime-make-arg :name 'file :help "File path"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "Arguments" content))
    (should (string-match-p "File path" content))))

(ert-deftest clime-test-invoke/render-shows-required ()
  "Required marker shown for required args without values."
  (let* ((arg (clime-make-arg :name 'file :help "File path"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "(required)" content))))

(ert-deftest clime-test-invoke/render-required-dimmed-when-set ()
  "Required marker stays visible but dimmed when arg has a value."
  (let* ((arg (clime-make-arg :name 'file :help "File path"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (clime-invoke--values (clime-values-set '() 'file "test.txt" 'user))
         (content (clime-invoke--render-to-string cmd nil nil)))
    ;; Still shows (required) text
    (should (string-match-p "(required)" content))
    ;; But in shadow face, not warning
    (let ((pos (string-match "(required)" content)))
      (should (eq 'shadow (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-quit-at-root ()
  "Root shows 'ESC Quit', non-root shows 'ESC Quit' and 'DEL Back'."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (at-root (clime-invoke--render-to-string cmd nil nil t))
         (not-root (clime-invoke--render-to-string cmd nil nil nil)))
    (should (string-match-p "ESC" at-root))
    (should (string-match-p "Quit" at-root))
    (should-not (string-match-p "\\bq\\b" at-root))
    (should (string-match-p "ESC" not-root))
    (should (string-match-p "DEL" not-root))
    (should (string-match-p "Back" not-root))))

(ert-deftest clime-test-invoke/render-shows-run ()
  "RET → Run shown for commands with handlers."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (should (string-match-p "RET" content))
    (should (string-match-p "Run" content))))

;;; ─── Run Handler ──────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/run-calls-handler ()
  "Run action builds context from params and calls handler."
  (let* ((called-with nil)
         (handler (lambda (ctx)
                    (setq called-with (clime-context-params ctx))))
         (opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1"
                               :options (list opt)
                               :children `(("run" . ,cmd)))))
    (let ((clime-invoke--values (clime-values-set '() 'verbose t 'user)))
      (clime-invoke--run-handler app cmd '("run"))
      (should called-with)
      (should (eq t (plist-get called-with 'verbose))))))

(ert-deftest clime-test-invoke/run-captures-output ()
  "Handler output is captured in the result."
  (let* ((handler (lambda (_ctx) (princ "hello world")))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--values '())
         (result (clime-invoke--run-handler app cmd '("run"))))
    (should (= 0 (car result)))
    (should (string-match-p "hello world" (cdr result)))))

(ert-deftest clime-test-invoke/run-validates-required ()
  "Missing required args produce exit code 2."
  (let* ((arg (clime-make-arg :name 'file :help "File"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--values '())
         (result (clime-invoke--run-handler app cmd '("run"))))
    (should (= 2 (car result)))
    (should (string-match-p "required" (cdr result)))))

(ert-deftest clime-test-invoke/run-applies-defaults ()
  "Parse-finalize applies defaults before running handler."
  (let* ((called-with nil)
         (handler (lambda (ctx)
                    (setq called-with (clime-context-params ctx))))
         (opt (clime-make-option :name 'limit :flags '("--limit")
                                  :default "10"))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1"
                               :options (list opt)
                               :children `(("run" . ,cmd)))))
    (let ((clime-invoke--values '()))
      (clime-invoke--run-handler app cmd '("run"))
      (should called-with)
      (should (equal "10" (plist-get called-with 'limit))))))

(ert-deftest clime-test-invoke/run-handler-error ()
  "Handler error produces exit code 1."
  (let* ((handler (lambda (_ctx) (error "boom")))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--values '())
         (result (clime-invoke--run-handler app cmd '("run"))))
    (should (= 1 (car result)))))

(ert-deftest clime-test-invoke/run-help-requested ()
  "Handler signaling help-requested returns exit 0 with help text."
  (let* ((handler (lambda (ctx)
                    (signal 'clime-help-requested
                            (list :node (clime-context-command ctx)
                                  :path '("run")))))
         (cmd (clime-make-command :name "run" :handler handler
                                   :help "Run something"))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--values '())
         (result (clime-invoke--run-handler app cmd '("run"))))
    (should (= 0 (car result)))
    (should (string-match-p "Run something" (cdr result)))))

(ert-deftest clime-test-invoke/run-buffered-errors-exit-1 ()
  "Handler that emits buffered errors returns exit code 1."
  (let* ((handler (lambda (_ctx)
                    (clime-out-error "something went wrong")))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1" :json-mode t
                               :children `(("run" . ,cmd))))
         ;; Bind a buffered (non-streaming) format so errors accumulate
         (clime-out--active-format
          (car (clime-app-output-formats app)))
         (clime-invoke--values '())
         (result (clime-invoke--run-handler app cmd '("run"))))
    (should (= 1 (car result)))))

;;; ─── Validation Tests ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/validate-param-valid-string ()
  "Valid string value returns nil."
  (let ((opt (clime-make-option :name 'name :flags '("--name")))
        (clime-invoke--values (clime-values-set '() 'name "hello" 'user)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-valid-integer ()
  "Valid integer string returns nil."
  (let ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer))
        (clime-invoke--values (clime-values-set '() 'count "42" 'user)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-invalid-integer ()
  "Non-numeric string for integer type returns error string."
  (let ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer))
        (clime-invoke--values (clime-values-set '() 'count "abc" 'user)))
    (should (stringp (clime-invoke--validate-param opt)))))

(ert-deftest clime-test-invoke/validate-param-valid-choice ()
  "Value matching choices returns nil."
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices '("json" "text")))
        (clime-invoke--values (clime-values-set '() 'fmt "json" 'user)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-invalid-choice ()
  "Value not in choices returns error string."
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices '("json" "text")))
        (clime-invoke--values (clime-values-set '() 'fmt "xml" 'user)))
    (should (stringp (clime-invoke--validate-param opt)))))

(ert-deftest clime-test-invoke/validate-param-unset-skipped ()
  "Unset param returns nil (no validation)."
  (let ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-non-string-skipped ()
  "Non-string value (e.g. boolean t) is not validated."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values (clime-values-set '() 'verbose t 'user)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-dynamic-choices ()
  "Dynamic choice function is resolved and validated."
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices (lambda () '("json" "text"))))
        (clime-invoke--values (clime-values-set '() 'fmt "json" 'user)))
    (should-not (clime-invoke--validate-param opt)))
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices (lambda () '("json" "text"))))
        (clime-invoke--values (clime-values-set '() 'fmt "xml" 'user)))
    (should (stringp (clime-invoke--validate-param opt)))))

(ert-deftest clime-test-invoke/run-conformer-checks-pass ()
  "Conformers that pass return nil."
  (let* ((conform (lambda (_params _node) nil))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform)))
         (clime-invoke--values '()))
    (should-not (clime-invoke--run-conformer-checks cmd))))

(ert-deftest clime-test-invoke/run-conformer-checks-single-fn ()
  "Single conformer function (not wrapped in list) works."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("single fn error"))))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform conform))
         (clime-invoke--values '()))
    (should (equal '(("single fn error"))
                   (clime-invoke--run-conformer-checks cmd)))))

(ert-deftest clime-test-invoke/run-conformer-checks-fail ()
  "Conformer signaling usage-error returns error strings."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("mutex violated"))))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform)))
         (clime-invoke--values '()))
    (should (equal '(("mutex violated"))
                   (clime-invoke--run-conformer-checks cmd)))))

(ert-deftest clime-test-invoke/run-conformer-checks-copies-params ()
  "Conformers receive a copy of values; original values map is not mutated."
  (let* ((original (clime-values-set '() 'foo "bar" 'user))
         (conform (lambda (values _node)
                    (clime-values-set values 'foo "mutated" 'conform)))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform)))
         (clime-invoke--values original))
    (clime-invoke--run-conformer-checks cmd)
    (should (equal "bar" (clime-values-value clime-invoke--values 'foo)))))

(ert-deftest clime-test-invoke/validate-all-no-errors ()
  "Clean params produce empty error lists."
  (let* ((opt (clime-make-option :name 'fmt :flags '("--format")
                                 :choices '("json" "text")))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'fmt "json" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should-not (car result))
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/validate-all-param-error ()
  "Invalid param value appears in param-errors alist."
  (let* ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'count "abc" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (assq 'count (car result)))
      (should (stringp (cdr (assq 'count (car result))))))))

(ert-deftest clime-test-invoke/validate-all-conformer-error ()
  "Conformer error appears in general-errors."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("bad combo"))))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform))))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (member "bad combo" (cdr result))))))

(ert-deftest clime-test-invoke/validate-all-requires-error ()
  "Unmet requires constraint appears in general-errors."
  (let* ((opt-a (clime-make-option :name 'key :flags '("--key")))
         (opt-b (clime-make-option :name 'cert :flags '("--cert")
                                   :requires '(key)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-a opt-b)))
         (clime-invoke--values (clime-values-set '() 'cert "foo" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (cl-some (lambda (e) (string-match-p "requires" e))
                       (cdr result))))))

;; Render with validation errors

(ert-deftest clime-test-invoke/render-param-error-inline ()
  "Param validation error shows inline with arrow marker."
  (let* ((opt (clime-make-option :name 'count :flags '("--count" "-c")
                                 :type 'integer))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (clime-invoke--values (clime-values-set '() 'count "abc" 'user))
         (validation '(((count . "invalid integer")) . nil))
         (content (clime-invoke--render-to-string
                   cmd nil nil nil validation)))
    (should (string-match-p "← invalid integer" content))))

(ert-deftest clime-test-invoke/render-general-error-in-header ()
  "General validation errors appear in the error area."
  (let* ((cmd (clime-make-command :name "test" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (validation '(nil . ("mutex violated")))
         (content (clime-invoke--render-to-string
                   cmd nil nil nil validation)))
    (should (string-match-p "mutex violated" content))))

(ert-deftest clime-test-invoke/render-no-validation-no-errors ()
  "Without validation-result, no error markers appear."
  (let* ((opt (clime-make-option :name 'count :flags '("--count" "-c")
                                 :type 'integer))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (clime-invoke--values (clime-values-set '() 'count "abc" 'user))
         (content (clime-invoke--render-to-string
                   cmd nil nil nil nil)))
    (should-not (string-match-p "←" content))))

(ert-deftest clime-test-invoke/render-combined-errors ()
  "Both user error-msg and general validation errors merge."
  (let* ((cmd (clime-make-command :name "test" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (validation '(nil . ("req unmet")))
         (content (clime-invoke--render-to-string
                   cmd nil "user error" nil validation)))
    (should (string-match-p "user error" content))
    (should (string-match-p "req unmet" content))))

(ert-deftest clime-test-invoke/validate-param-arg-invalid ()
  "Arg with type constraint returns error for invalid value."
  (let ((arg (clime-make-arg :name 'port :type 'integer))
        (clime-invoke--values (clime-values-set '() 'port "abc" 'user)))
    (should (stringp (clime-invoke--validate-param arg)))))

(ert-deftest clime-test-invoke/validate-param-arg-choices ()
  "Arg with choices validates against them."
  (let ((arg (clime-make-arg :name 'env :choices '("dev" "prod")))
        (clime-invoke--values (clime-values-set '() 'env "dev" 'user)))
    (should-not (clime-invoke--validate-param arg)))
  (let ((arg (clime-make-arg :name 'env :choices '("dev" "prod")))
        (clime-invoke--values (clime-values-set '() 'env "staging" 'user)))
    (should (stringp (clime-invoke--validate-param arg)))))

(ert-deftest clime-test-invoke/validate-param-conform-string-pass ()
  "String value passing conformer returns nil."
  (let ((opt (clime-make-option :name 'sexp :flags '("--sexp")
                                :conform (lambda (v) (read v))))
        (clime-invoke--values (clime-values-set '() 'sexp "(+ 1 2)" 'user)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-conform-string-fail ()
  "String value failing conformer returns error string."
  (let ((opt (clime-make-option :name 'sexp :flags '("--sexp")
                                :conform (lambda (v) (read v))))
        (clime-invoke--values (clime-values-set '() 'sexp "(bad sexp" 'user)))
    (should (stringp (clime-invoke--validate-param opt)))))

(ert-deftest clime-test-invoke/validate-param-conform-non-string ()
  "Non-string pre-filled value also runs through conformer."
  (let ((opt (clime-make-option :name 'val :flags '("--val")
                                :conform (lambda (v)
                                           (unless (> v 0)
                                             (error "Must be positive"))
                                           v)))
        (clime-invoke--values (clime-values-set '() 'val 5 'user)))
    (should-not (clime-invoke--validate-param opt))
    (setq clime-invoke--values (clime-values-set clime-invoke--values 'val -1 'user))
    (should (stringp (clime-invoke--validate-param opt)))))

(ert-deftest clime-test-invoke/validate-param-conform-after-coercion ()
  "Conformer receives the coerced value, not the raw string."
  (let* ((received nil)
         (opt (clime-make-option :name 'count :flags '("--count")
                                 :type 'integer
                                 :conform (lambda (v)
                                            (setq received v)
                                            v)))
         (clime-invoke--values (clime-values-set '() 'count "42" 'user)))
    (clime-invoke--validate-param opt)
    (should (equal received 42))))

(ert-deftest clime-test-invoke/validate-param-conform-arg ()
  "Arg conformer validates pre-filled value."
  (let ((arg (clime-make-arg :name 'file
                             :conform (lambda (v)
                                        (unless (string-suffix-p ".org" v)
                                          (error "Must be .org file"))
                                        v)))
        (clime-invoke--values (clime-values-set '() 'file "test.org" 'user)))
    (should-not (clime-invoke--validate-param arg)))
  (let ((arg (clime-make-arg :name 'file
                             :conform (lambda (v)
                                        (unless (string-suffix-p ".org" v)
                                          (error "Must be .org file"))
                                        v)))
        (clime-invoke--values (clime-values-set '() 'file "test.txt" 'user)))
    (should (stringp (clime-invoke--validate-param arg)))))

(ert-deftest clime-test-invoke/validate-param-no-conform-non-string ()
  "Non-string value without conformer still returns nil."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values (clime-values-set '() 'verbose t 'user)))
    (should-not (clime-invoke--validate-param opt))))

(ert-deftest clime-test-invoke/validate-param-conform-2-arg ()
  "Two-arg conformer receives both value and param."
  (let* ((received-param nil)
         (opt (clime-make-option :name 'val :flags '("--val")
                                 :conform (lambda (v param)
                                            (setq received-param param)
                                            v)))
         (clime-invoke--values (clime-values-set '() 'val "hello" 'user)))
    (clime-invoke--validate-param opt)
    (should (clime-option-p received-param))))

(ert-deftest clime-test-invoke/validate-all-locked-skips-conform ()
  "Locked option with conformer is not validated."
  (let* ((opt (clime-make-option :name 'fmt :flags '("--fmt")
                                 :locked t
                                 :conform (lambda (_v)
                                            (error "Should not be called"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'fmt "json" 'app)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should-not (car result)))))

(ert-deftest clime-test-invoke/validate-all-ancestor-options ()
  "Ancestor option errors are included in param-errors."
  (let* ((parent-opt (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
         (cmd (clime-make-command :name "serve" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :options (list parent-opt)
                              :children `(("serve" . ,cmd))))
         (clime-invoke--values (clime-values-set '() 'port "abc" 'user)))
    (setf (clime-node-parent cmd) app)
    (let ((result (clime-invoke--validate-all cmd)))
      (should (assq 'port (car result))))))

(ert-deftest clime-test-invoke/validate-all-empty-node ()
  "Node with no options, args, or conformers returns empty results."
  (let ((cmd (clime-make-command :name "test" :handler #'ignore)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should-not (car result))
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/validate-all-multiple-conformer-errors ()
  "Multiple conformers each contribute their own errors."
  (let* ((c1 (lambda (_p _n) (signal 'clime-usage-error '("error one"))))
         (c2 (lambda (_p _n) (signal 'clime-usage-error '("error two"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :conform (list c1 c2))))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (member "error one" (cdr result)))
      (should (member "error two" (cdr result))))))

(ert-deftest clime-test-invoke/validate-all-mixed-errors ()
  "Both param-errors and general-errors can coexist."
  (let* ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer))
         (conform (lambda (_p _n) (signal 'clime-usage-error '("bad state"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)
                                  :conform (list conform)))
         (clime-invoke--values (clime-values-set '() 'count "abc" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (assq 'count (car result)))
      (should (member "bad state" (cdr result))))))

;;; ─── Prefix State Rendering ─────────────────────────────────────────

(ert-deftest clime-test-invoke/render-prefix-dash-shows-dash-keys ()
  "With prefix-state \"-\", option keys show as \"-X\"."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil "-")))
    (should (string-match-p "-v" content))))

(ert-deftest clime-test-invoke/render-prefix-eq-shows-eq-keys ()
  "With prefix-state \"=\", option keys show as \"=X\"."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil "=")))
    (should (string-match-p "=v" content))))

(ert-deftest clime-test-invoke/render-prefix-dims-command-keys ()
  "During prefix state, command keys are dimmed."
  (let* ((child (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,child))))
         (content (clime-invoke--render-to-string app nil nil nil nil "-")))
    ;; Find "s" key after "Commands" heading
    (let* ((cmds-pos (string-match "Commands" content))
           (pos (and cmds-pos (string-match "\\bs\\b" content (1+ cmds-pos)))))
      (should pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-prefix-dims-arg-keys ()
  "During prefix state, arg keys are dimmed."
  (let* ((arg (clime-make-arg :name 'file :help "Input file"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil "-")))
    (let* ((args-pos (string-match "Arguments" content))
           (pos (and args-pos (string-match "\\bf\\b" content (1+ args-pos)))))
      (should pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-prefix-dims-action-keys ()
  "During prefix state, action keys (RET, ESC) are dimmed."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil t nil "-")))
    (let ((ret-pos (string-match "RET" content)))
      (should ret-pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property ret-pos 'face content))))
    (let ((esc-pos (string-match "ESC" content)))
      (should esc-pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property esc-pos 'face content))))))

(ert-deftest clime-test-invoke/render-prefix-option-keys-not-dimmed ()
  "During prefix state, option keys keep their option-key face."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil "-")))
    (let ((pos (string-match "-v" content)))
      (should pos)
      (should (eq 'clime-invoke-option-key
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-no-prefix-no-dimming ()
  "Without prefix state, no dimming occurs."
  (let* ((child (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,child))))
         (content (clime-invoke--render-to-string app nil nil)))
    (let* ((cmds-pos (string-match "Commands" content))
           (pos (and cmds-pos (string-match "\\bs\\b" content (1+ cmds-pos)))))
      (should pos)
      (should (eq 'clime-invoke-command-key
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-prefix-dims-non-option-headings ()
  "During prefix state, non-option section headings are dimmed."
  (let* ((arg (clime-make-arg :name 'file :help "Input file"))
         (child (clime-make-command :name "sub" :handler #'ignore))
         (grp (clime-make-group :name "top" :handler #'ignore
                                 :args (list arg)
                                 :children `(("sub" . ,child))))
         (content (clime-invoke--render-to-string grp nil nil nil nil "-")))
    ;; Arguments heading dimmed
    (let ((pos (string-match "Arguments" content)))
      (should pos)
      (should (eq 'clime-invoke-dimmed (get-text-property pos 'face content))))
    ;; Commands heading dimmed
    (let ((pos (string-match "Commands" content)))
      (should pos)
      (should (eq 'clime-invoke-dimmed (get-text-property pos 'face content))))
    ;; Actions heading dimmed
    (let ((pos (string-match "Actions" content)))
      (should pos)
      (should (eq 'clime-invoke-dimmed (get-text-property pos 'face content))))
    ;; Options heading NOT dimmed
    (let ((pos (string-match "Options" content)))
      ;; No own options on this group, so Options heading shouldn't appear
      ;; But if it did, it would keep its normal face
      (should-not pos))))

;;; ─── Inline Group Conformer Discovery (clime-swl) ──────────────────

(ert-deftest clime-test-invoke/run-conformer-checks-inline-group ()
  "Conformers on inline group children are discovered and run."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("inline group error"))))
         (grp (clime-group--create :name "fmt" :inline t :conform (list conform)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "fmt" grp))))
         (clime-invoke--values '()))
    (should (equal '(("inline group error"))
                   (clime-invoke--run-conformer-checks cmd)))))

(ert-deftest clime-test-invoke/run-conformer-checks-inline-group-pass ()
  "Inline group conformers that pass produce no errors."
  (let* ((conform (lambda (params _node) params))
         (grp (clime-group--create :name "fmt" :inline t :conform (list conform)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "fmt" grp))))
         (clime-invoke--values '()))
    (should-not (clime-invoke--run-conformer-checks cmd))))

(ert-deftest clime-test-invoke/option-actions-include-inline-group-options ()
  "Option actions include options from inline group children."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0))
         (grp (clime-group--create :name "fmt" :inline t
                                   :options (list opt-json opt-csv)))
         (opt-verbose (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-verbose)
                                  :children (list (cons "fmt" grp)))))
    (let* ((actions (clime-invoke--build-option-actions cmd))
           (opt-names (mapcar (lambda (a) (clime-option-name (cadr (cdr a)))) actions)))
      ;; All three options should have actions
      (should (= 3 (length actions)))
      (should (memq 'verbose opt-names))
      (should (memq 'json opt-names))
      (should (memq 'csv opt-names)))))

(ert-deftest clime-test-invoke/validate-all-includes-inline-group-options ()
  "Per-param validation covers options from inline group children."
  (let* ((opt (clime-make-option :name 'fmt :flags '("--format")
                                 :choices '("json" "text")))
         (grp (clime-group--create :name "output" :inline t
                                   :options (list opt)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "output" grp))))
         ;; Invalid choice — should produce a param error
         (clime-invoke--values (clime-values-set '() 'fmt "xml" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (assq 'fmt (car result))))))

(ert-deftest clime-test-invoke/render-shows-inline-group-options ()
  "Invoke render includes inline group options in the options section."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :help "JSON output"))
         (grp (clime-group--create :name "fmt" :inline t
                                   :options (list opt-json)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "fmt" grp))))
         (key-map (clime-invoke--build-key-map cmd))
         (buf (generate-new-buffer " *test-render*")))
    (unwind-protect
        (progn
          (clime-invoke--render cmd key-map nil buf nil nil)
          (let ((content (with-current-buffer buf (buffer-string))))
            (should (string-match-p "--json" content))))
      (kill-buffer buf))))

;;; ─── Conformer Param Attribution (clime-r6q) ────────────────────────

(ert-deftest clime-test-invoke/conformer-error-with-params ()
  "Conformer signaling :params propagates to param-errors."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error
                            '("Options a, b are mutually exclusive"
                              :params (a b)))))
         (opt-a (clime-make-option :name 'a :flags '("--a")))
         (opt-b (clime-make-option :name 'b :flags '("--b")))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-a opt-b)
                                  :conform (list conform)))
         (clime-invoke--values (clime-values-set
                                (clime-values-set '() 'a "1" 'user)
                                'b "2" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      ;; Both params should have inline errors
      (should (assq 'a (car result)))
      (should (assq 'b (car result)))
      ;; Attributed errors should NOT appear in general-errors
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/conformer-error-without-params ()
  "Conformer without :params still works (general-errors only)."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("custom error"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :conform (list conform))))
    (let ((result (clime-invoke--validate-all cmd)))
      ;; No param-errors
      (should-not (car result))
      ;; General error present
      (should (member "custom error" (cdr result))))))

(ert-deftest clime-test-invoke/run-conformer-returns-param-attribution ()
  "run-conformer-checks returns (MESSAGE . PARAM-NAMES) pairs."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error
                            '("bad combo" :params (x y)))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :conform (list conform)))
         (clime-invoke--values '()))
    (let ((errors (clime-invoke--run-conformer-checks cmd)))
      (should (= 1 (length errors)))
      (should (equal (caar errors) "bad combo"))
      (should (equal (cdar errors) '(x y))))))

(ert-deftest clime-test-invoke/run-conformer-no-params-returns-nil-cdr ()
  "Conformer without :params returns (MESSAGE . nil)."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("plain error"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :conform (list conform)))
         (clime-invoke--values '()))
    (let ((errors (clime-invoke--run-conformer-checks cmd)))
      (should (= 1 (length errors)))
      (should (equal (caar errors) "plain error"))
      (should-not (cdar errors)))))

(ert-deftest clime-test-invoke/mutex-conformer-attributes-params ()
  "clime-check-exclusive signals :params with conflicting option names."
  (let* ((exclusive (clime-check-exclusive 'mode '(a b)))
         (opt-a (clime-make-option :name 'a :flags '("--a")))
         (opt-b (clime-make-option :name 'b :flags '("--b")))
         (grp (clime-group--create :name "mode" :inline t
                                   :options (list opt-a opt-b)
                                   :conform (list exclusive)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "mode" grp))))
         (clime-invoke--values (clime-values-set
                                (clime-values-set '() 'a "1" 'user)
                                'b "2" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      ;; Both conflicting params get inline markers
      (should (assq 'a (car result)))
      (should (assq 'b (car result)))
      ;; Attributed: not in header
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/render-conformer-param-error-inline ()
  "Conformer with :params shows inline markers on attributed options."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--a") :help "Option A"))
         (opt-b (clime-make-option :name 'b :flags '("--b") :help "Option B"))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-a opt-b)))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (clime-invoke--values (clime-values-set
                                (clime-values-set '() 'a "1" 'user)
                                'b "2" 'user))
         ;; Simulate validation result: attributed errors go inline only
         (validation '(((a . "mutually exclusive") (b . "mutually exclusive"))))
         (content (clime-invoke--render-to-string
                   cmd nil nil nil validation)))
    ;; Both options should have inline error markers
    (should (string-match-p "← mutually exclusive" content))
    ;; No header error (attributed)
    (should-not (string-match-p "Options a, b are mutually exclusive" content))))

(ert-deftest clime-test-invoke/paired-conformer-attributes-params ()
  "clime-check-paired signals :params on cardinality mismatch."
  (let* ((paired (clime-check-paired 'mapping '(from to)))
         (opt-from (clime-make-option :name 'from :flags '("--from") :multiple t))
         (opt-to (clime-make-option :name 'to :flags '("--to") :multiple t))
         (grp (clime-group--create :name "mapping" :inline t
                                   :options (list opt-from opt-to)
                                   :conform (list paired)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "mapping" grp))))
         (clime-invoke--values (clime-values-set
                                (clime-values-set '() 'from '("a" "b") 'user)
                                'to '("x") 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      ;; Both paired params get inline markers
      (should (assq 'from (car result)))
      (should (assq 'to (car result)))
      ;; Attributed: not in header
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/signal-params-backwards-compatible ()
  "cadr on signal data still returns the message string (CLI path)."
  (let ((err (condition-case e
                 (signal 'clime-usage-error
                         '("msg here" :params (a b)))
               (clime-usage-error e))))
    (should (equal "msg here" (cadr err)))))

;;; ─── Locked Options Display (clime-99u) ─────────────────────────────

(ert-deftest clime-test-invoke/render-shows-locked-vals ()
  "Locked options from alias :vals appear in the menu as read-only rows."
  (let* ((opt (clime-make-option :name 'todo :flags '("--todo" "-t")
                                 :help "TODO keyword"
                                 :locked t))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :options (list opt)
                                  :value-entries '((todo :value "WAITING" :source app))))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("waiting" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (clime-invoke--values (clime-values-set '() 'todo "WAITING" 'app))
         (key-map (clime-invoke--build-key-map cmd))
         (buf (generate-new-buffer " *test-render*")))
    (unwind-protect
        (progn
          (clime-invoke--render cmd key-map nil buf nil nil)
          (let ((content (with-current-buffer buf (buffer-string))))
            (should (string-match-p "todo" content))
            (should (string-match-p "WAITING" content))
            (should (string-match-p "(locked)" content))
            ;; Locked option has no key binding
            (should-not (string-match-p " -t " content))))
      (kill-buffer buf))))

(ert-deftest clime-test-invoke/render-no-locked-vals ()
  "Commands without locked-vals show no locked rows."
  (let* ((cmd (clime-make-command :name "query" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("query" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (key-map (clime-invoke--build-key-map cmd))
         (buf (generate-new-buffer " *test-render*")))
    (unwind-protect
        (progn
          (clime-invoke--render cmd key-map nil buf nil nil)
          (let ((content (with-current-buffer buf (buffer-string))))
            (should-not (string-match-p "(locked)" content))))
      (kill-buffer buf))))

(ert-deftest clime-test-invoke/locked-option-mutex-validation ()
  "Setting a mutex sibling of a locked option triggers a validation error."
  (let* ((exclusive (clime-check-exclusive 'query-mode '(todo sexp)))
         (opt-todo (clime-make-option :name 'todo :flags '("--todo")
                                      :locked t))
         (opt-sexp (clime-make-option :name 'sexp :flags '("--sexp")))
         (mutex (clime-group--create :name "query-mode" :inline t
                                     :options (list opt-todo opt-sexp)
                                     :conform (list exclusive)))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :children (list (cons "query-mode" mutex))))
         ;; Locked todo from alias + user sets sexp
         (clime-invoke--values
          (clime-values-set
           (clime-values-set '() 'todo "WAITING" 'app)
           'sexp "(todo)" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      ;; Both params get inline error markers
      (should (assq 'todo (car result)))
      (should (assq 'sexp (car result))))))

(ert-deftest clime-test-invoke/locked-option-no-key-binding ()
  "Locked options get no key binding in the key map."
  (let* ((opt (clime-make-option :name 'todo :flags '("--todo" "-t")
                                 :locked t :default "WAITING"))
         (opt2 (clime-make-option :name 'sort :flags '("--sort")))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :options (list opt opt2)))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("waiting" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (key-map (clime-invoke--build-key-map cmd)))
    ;; sort gets a key binding, todo does not
    (should (cl-find-if (lambda (e) (and (eq (cadr e) :option)
                                         (eq (clime-option-name (caddr e)) 'sort)))
                        key-map))
    (should-not (cl-find-if (lambda (e) (and (eq (cadr e) :option)
                                              (eq (clime-option-name (caddr e)) 'todo)))
                            key-map))))

;;; ─── Key Rework (clime-cos) ─────────────────────────────────────────

(ert-deftest clime-test-invoke/q-available-for-children ()
  "Letter q is not reserved — child commands starting with q get it."
  (let* ((cmd (clime-make-command :name "query" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("query" . ,cmd))))
         (km (clime-invoke--build-key-map app)))
    (should (equal "q" (car (cl-find-if
                             (lambda (e) (and (eq (cadr e) :child)
                                              (equal (caddr e) "query")))
                             km))))))

(ert-deftest clime-test-invoke/render-no-q-action ()
  "Action bar does not show q as a key."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (at-root (clime-invoke--render-to-string cmd nil nil t))
         (not-root (clime-invoke--render-to-string cmd nil nil nil)))
    (should-not (string-match-p "\\bq\\b" at-root))
    (should-not (string-match-p "\\bq\\b" not-root))))

(ert-deftest clime-test-invoke/render-del-back-only-at-child ()
  "DEL Back shown at child nodes, not at root."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (at-root (clime-invoke--render-to-string cmd nil nil t))
         (not-root (clime-invoke--render-to-string cmd nil nil nil)))
    (should-not (string-match-p "DEL" at-root))
    (should (string-match-p "DEL" not-root))
    (should (string-match-p "Back" not-root))))

;;; ─── Group Lock Propagation Rendering (clime-di0) ───────────────────

(ert-deftest clime-test-invoke/render-locked-mutex-sibling-excluded ()
  "Locked mutex sibling (nil value) renders as (excluded) not nil."
  (let* ((exclusive (clime-check-exclusive 'qm '(todo sexp)))
         (opt-todo (clime-make-option :name 'todo :flags '("--todo")
                                      :locked t))
         (opt-sexp (clime-make-option :name 'sexp :flags '("--sexp")
                                      :locked t))
         (mutex (clime-group--create :name "qm" :inline t
                                     :options (list opt-todo opt-sexp)
                                     :conform (list exclusive)))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :children (list (cons "qm" mutex))
                                  :value-entries '((todo :value "WAITING" :source app))))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("waiting" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (clime-invoke--values (clime-values-set '() 'todo "WAITING" 'app))
         (key-map (clime-invoke--build-key-map cmd))
         (buf (generate-new-buffer " *test-render*")))
    (unwind-protect
        (progn
          (clime-invoke--render cmd key-map nil buf nil nil)
          (let ((content (with-current-buffer buf (buffer-string))))
            ;; todo shows its locked value
            (should (string-match-p "WAITING" content))
            (should (string-match-p "(locked)" content))
            ;; sexp shows (excluded), not "nil"
            (should (string-match-p "(excluded)" content))
            (should-not (string-match-p "\\bnil\\b" content))
            ;; Neither has a key binding
            (should-not (cl-find-if
                         (lambda (e) (and (eq (cadr e) :option)
                                          (memq (clime-option-name (caddr e))
                                                '(todo sexp))))
                         key-map))))
      (kill-buffer buf))))

(ert-deftest clime-test-invoke/locked-mutex-sibling-no-validation ()
  "Locked mutex siblings are skipped in per-param validation."
  (let* ((exclusive (clime-check-exclusive 'qm '(todo sexp)))
         (opt-todo (clime-make-option :name 'todo :flags '("--todo")
                                      :locked t :default "WAITING"))
         (opt-sexp (clime-make-option :name 'sexp :flags '("--sexp")
                                      :locked t :default nil :required t))
         (mutex (clime-group--create :name "qm" :inline t
                                     :options (list opt-todo opt-sexp)
                                     :conform (list exclusive)))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :children (list (cons "qm" mutex)))))
    ;; sexp is :required but locked — should not produce a validation error
    (let ((result (clime-invoke--validate-all cmd)))
      (should-not (assq 'sexp (car result))))))

;;; ─── Visibility Toggle ─────────────────────────────────────────────────

(ert-deftest clime-test-invoke/visible-children-normal-hides-hidden ()
  "In normal mode, hidden commands are excluded from visible children."
  (let* ((cmd-a (clime-make-command :name "visible" :handler #'ignore))
         (cmd-b (clime-make-command :name "secret" :handler #'ignore :hidden t))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("visible" . ,cmd-a)
                                           ("secret" . ,cmd-b))))
         (clime-invoke--show-mode 'normal))
    (let ((children (clime-invoke--visible-children app)))
      (should (= 1 (length children)))
      (should (equal "visible" (caar children))))))

(ert-deftest clime-test-invoke/visible-children-all-shows-hidden ()
  "In all mode, hidden commands are included in visible children."
  (let* ((cmd-a (clime-make-command :name "visible" :handler #'ignore))
         (cmd-b (clime-make-command :name "secret" :handler #'ignore :hidden t))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("visible" . ,cmd-a)
                                           ("secret" . ,cmd-b))))
         (clime-invoke--show-mode 'all))
    (let ((children (clime-invoke--visible-children app)))
      (should (= 2 (length children)))
      (should (assoc "secret" children)))))

(ert-deftest clime-test-invoke/visible-children-clean-hides-deprecated ()
  "In clean mode, deprecated commands are filtered out."
  (let* ((cmd-a (clime-make-command :name "current" :handler #'ignore))
         (cmd-b (clime-make-command :name "old" :handler #'ignore
                                     :deprecated "use current"))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("current" . ,cmd-a)
                                           ("old" . ,cmd-b))))
         (clime-invoke--show-mode 'clean))
    (let ((children (clime-invoke--visible-children app)))
      (should (= 1 (length children)))
      (should (equal "current" (caar children))))))

(ert-deftest clime-test-invoke/option-actions-all-includes-hidden ()
  "In all mode, hidden options get key bindings."
  (let* ((opt-v (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (opt-h (clime-make-option :name 'debug :flags '("--debug") :nargs 0
                                    :hidden t))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt-v opt-h)))
         (clime-invoke--show-mode 'all))
    (let ((actions (clime-invoke--build-option-actions cmd)))
      (should (cl-some (lambda (a) (eq 'debug (clime-option-name (caddr a))))
                       actions)))))

(ert-deftest clime-test-invoke/option-actions-normal-excludes-hidden ()
  "In normal mode, hidden options have no key bindings."
  (let* ((opt-v (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (opt-h (clime-make-option :name 'debug :flags '("--debug") :nargs 0
                                    :hidden t))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt-v opt-h)))
         (clime-invoke--show-mode 'normal))
    (let ((actions (clime-invoke--build-option-actions cmd)))
      (should-not (cl-some (lambda (a) (eq 'debug (clime-option-name (caddr a))))
                           actions)))))

(ert-deftest clime-test-invoke/render-all-shows-hidden-annotation ()
  "In all mode, hidden options render with (hidden) annotation."
  (let* ((opt (clime-make-option :name 'debug :flags '("--debug") :nargs 0
                                  :hidden t :help "Debug mode"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--show-mode 'all))
    (setf (clime-node-parent cmd) app)
    (let ((output (clime-invoke--render-to-string cmd nil nil)))
      (should (string-match-p "hidden" output)))))

(ert-deftest clime-test-invoke/render-clean-hides-deprecated-options ()
  "In clean mode, deprecated options are excluded from render."
  (let* ((opt-a (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                    :help "Verbosity"))
         (opt-b (clime-make-option :name 'old-flag :flags '("--old") :nargs 0
                                    :deprecated t :help "Old flag"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt-a opt-b)))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--show-mode 'clean))
    (setf (clime-node-parent cmd) app)
    (let ((output (clime-invoke--render-to-string cmd nil nil)))
      (should (string-match-p "Verbosity" output))
      (should-not (string-match-p "Old flag" output)))))

;;; ─── Structured Return Value ────────────────────────────────────────────

(ert-deftest clime-test-invoke/returns-structured-result-with-output ()
  "clime-invoke returns (:params :exit :output) when handler ran."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (app (clime-make-app :name "test" :version "1"
                               :options (list opt)))
         (output '(0 . "hello")))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (list output nil)))
              ((symbol-function 'clime-invoke--display-output)
               #'ignore))
      (let ((result (clime-invoke app nil '(verbose t))))
        (should (plistp result))
        (should (eq t (plist-get (plist-get result :params) 'verbose)))
        (should (= 0 (plist-get result :exit)))
        (should (equal "hello" (plist-get result :output)))))))

(ert-deftest clime-test-invoke/returns-nil-exit-output-on-quit ()
  "clime-invoke returns nil :exit and :output when user quit without running."
  (let* ((app (clime-make-app :name "test" :version "1")))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (list nil nil)))
              ((symbol-function 'clime-invoke--display-output)
               #'ignore))
      (let ((result (clime-invoke app)))
        (should (plistp result))
        (should-not (plist-get result :exit))
        (should-not (plist-get result :output))))))

(ert-deftest clime-test-invoke/returns-nonzero-exit-on-error ()
  "clime-invoke returns nonzero :exit when handler errored."
  (let* ((app (clime-make-app :name "test" :version "1"))
         (output '(1 . "boom")))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (list output nil)))
              ((symbol-function 'clime-invoke--display-output)
               #'ignore))
      (let ((result (clime-invoke app)))
        (should (= 1 (plist-get result :exit)))
        (should (equal "boom" (plist-get result :output)))))))

(ert-deftest clime-test-invoke/still-displays-output-buffer ()
  "clime-invoke still calls display-output as a side effect."
  (let* ((app (clime-make-app :name "test" :version "1"))
         (displayed nil))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (list '(0 . "shown") nil)))
              ((symbol-function 'clime-invoke--display-output)
               (lambda (output &optional exit-code _display)
                 (setq displayed (cons exit-code output)))))
      (clime-invoke app)
      (should displayed)
      (should (= 0 (car displayed)))
      (should (equal "shown" (cdr displayed))))))

;;; ─── Output Display Tests ───────────────────────────────────────────

(ert-deftest clime-test-invoke/display-output-short-uses-message-only ()
  "Short output (≤3 lines) only calls message, not display-buffer."
  (let ((displayed nil)
        (messaged nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq displayed t) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq messaged (apply #'format fmt args)))))
      (clime-invoke--display-output "short output" 0 t))
    (should-not displayed)
    (should (equal "short output" messaged))))

(ert-deftest clime-test-invoke/display-output-long-calls-display-buffer ()
  "Long output (>3 lines) opens a window via display-buffer."
  (let ((displayed nil)
        (action-used nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (_buf action) (setq displayed t action-used action) nil)))
      (let ((clime-invoke-output-display-action '(display-buffer-reuse-window)))
        (clime-invoke--display-output "line1\nline2\nline3\nline4\n" 0 t)))
    (should displayed)
    (should (equal '(display-buffer-reuse-window) action-used))))

(ert-deftest clime-test-invoke/display-output-custom-action ()
  "Output display respects clime-invoke-output-display-action."
  (let ((action-used nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (_buf action) (setq action-used action) nil)))
      (let ((clime-invoke-output-display-action
             '(display-buffer-in-side-window (side . right))))
        (clime-invoke--display-output "a\nb\nc\nd\ne\n" 0 t)))
    (should (equal '(display-buffer-in-side-window (side . right)) action-used))))

(ert-deftest clime-test-invoke/display-output-silent-suppresses-all ()
  "Display mode silent suppresses all output display."
  (let ((displayed nil)
        (messaged nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq displayed t) nil))
              ((symbol-function 'message)
               (lambda (&rest _) (setq messaged t))))
      (clime-invoke--display-output "hello" 0 'silent))
    (should-not displayed)
    (should-not messaged)
    ;; Buffer should still be populated
    (should (equal "hello"
                   (with-current-buffer "*clime-output*"
                     (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest clime-test-invoke/display-output-message-mode ()
  "Display mode message always messages, never opens a window."
  (let ((displayed nil)
        (messaged nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq displayed t) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq messaged (apply #'format fmt args)))))
      (clime-invoke--display-output "line1\nline2\nline3\nline4\n" 0 'message))
    (should-not displayed)
    (should (equal "line1\nline2\nline3\nline4" messaged))))

(ert-deftest clime-test-invoke/display-keyword-threads-to-display-output ()
  "clime-invoke :display keyword is passed to display-output."
  (let* ((app (clime-make-app :name "test" :version "1"))
         (display-arg nil))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (list '(0 . "out") nil)))
              ((symbol-function 'clime-invoke--display-output)
               (lambda (_output &optional _exit-code display)
                 (setq display-arg display))))
      (clime-invoke app nil nil :display 'silent)
      (should (eq 'silent display-arg)))))

(ert-deftest clime-test-invoke/display-keyword-defaults-to-nil ()
  "clime-invoke without :display passes nil (which defaults to t in display-output)."
  (let* ((app (clime-make-app :name "test" :version "1"))
         (display-arg 'unset))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (list '(0 . "out") nil)))
              ((symbol-function 'clime-invoke--display-output)
               (lambda (_output &optional _exit-code display)
                 (setq display-arg display))))
      (clime-invoke app)
      (should-not display-arg))))

;;; ─── Tree-Only Write Specs (clime-nqo) ─────────────────────────────

(ert-deftest clime-test-invoke/handle-option-sets-values-map ()
  "handle-option sets value in the values map."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values '()))
    (clime-invoke--handle-option opt)
    (should (eq t (clime-values-value clime-invoke--values 'verbose)))
    (should (eq 'user (clime-values-source clime-invoke--values 'verbose)))))

(ert-deftest clime-test-invoke/handle-option-unset-clears-values-map ()
  "Toggling a set boolean clears the entry from values map."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values (clime-values-set '() 'verbose t 'user)))
    (clime-invoke--handle-option opt)
    (should-not (clime-values-value clime-invoke--values 'verbose))
    (should-not (clime-values-source clime-invoke--values 'verbose))))

(ert-deftest clime-test-invoke/handle-count-sets-values-map ()
  "Count option increments value in values map."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t))
        (clime-invoke--values (clime-values-set '() 'verbose 2 'user)))
    (clime-invoke--handle-option opt)
    (should (= 3 (clime-values-value clime-invoke--values 'verbose)))
    (should (eq 'user (clime-values-source clime-invoke--values 'verbose)))))

(ert-deftest clime-test-invoke/handle-choices-cycle-values-map ()
  "Choices option cycles value in values map."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text")))
        (clime-invoke--values '()))
    (clime-invoke--handle-option opt)
    (should (equal "json" (clime-values-value clime-invoke--values 'format)))
    (should (eq 'user (clime-values-source clime-invoke--values 'format)))))

(ert-deftest clime-test-invoke/format-value-reads-struct ()
  "format-value reads from the values map."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
        (clime-invoke--values (clime-values-set '() 'verbose t 'user)))
    (let ((str (clime-invoke--format-value opt)))
      (should (equal "on" (substring-no-properties str))))))

(ert-deftest clime-test-invoke/seed-values-populates-map ()
  "seed-values builds values map from tree value-entries and params plist."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (arg (clime-make-arg :name 'file))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)
                                   :args (list arg)))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "run" cmd))))
         (clime-invoke--values nil))
    (clime--set-parent-refs app)
    (clime-invoke--seed-values app '(verbose t file "a.txt"))
    (should (eq t (clime-values-value clime-invoke--values 'verbose)))
    (should (eq 'user (clime-values-source clime-invoke--values 'verbose)))
    (should (equal "a.txt" (clime-values-value clime-invoke--values 'file)))
    (should (eq 'user (clime-values-source clime-invoke--values 'file)))))

(ert-deftest clime-test-invoke/seed-values-includes-unknown-params ()
  "seed-values puts all params into the values map, even unmatched ones."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values nil))
    (clime-invoke--seed-values cmd '(verbose t unknown "val"))
    (should (eq t (clime-values-value clime-invoke--values 'verbose)))
    ;; unknown param is also in the values map (user source)
    (should (equal "val" (clime-values-value clime-invoke--values 'unknown)))))

(ert-deftest clime-test-invoke/invoke-does-not-mutate-original-tree ()
  "clime-invoke deep-copies; original app tree is untouched."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (cmd (clime-make-command :name "cmd" :handler #'ignore
                                   :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "cmd" cmd)))))
    (clime--set-parent-refs app)
    ;; Invoke with params — this should seed onto the deep copy only
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top) (list nil nil))))
      (clime-invoke app '("cmd") '(verbose t)))
    ;; Original struct metadata is untouched
    (should-not (clime-param-help opt))))

;;; ─── Values Map Coverage (clime-33d2) ────────────────────────────────

(ert-deftest clime-test-invoke/seed-values-collects-value-entries ()
  "seed-values picks up value-entries from nested nodes."
  (let* ((opt (clime-make-option :name 'todo :flags '("--todo") :locked t))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :options (list opt)
                                  :value-entries '((todo :value "WAITING" :source app))))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "waiting" cmd))))
         (clime-invoke--values nil))
    (clime--set-parent-refs app)
    (clime-invoke--seed-values app nil)
    (should (equal "WAITING" (clime-values-value clime-invoke--values 'todo)))
    (should (eq 'app (clime-values-source clime-invoke--values 'todo)))))

(ert-deftest clime-test-invoke/seed-values-user-overrides-app ()
  "User params in seed-values override app value-entries."
  (let* ((opt (clime-make-option :name 'fmt :flags '("--fmt")))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)
                                  :value-entries '((fmt :value "json" :source app))))
         (app (clime-make-app :name "test" :version "1"
                              :children (list (cons "run" cmd))))
         (clime-invoke--values nil))
    (clime--set-parent-refs app)
    (clime-invoke--seed-values app '(fmt "text"))
    (should (equal "text" (clime-values-value clime-invoke--values 'fmt)))
    (should (eq 'user (clime-values-source clime-invoke--values 'fmt)))))

(ert-deftest clime-test-invoke/set-param-writes-user-source ()
  "set-param writes user source to values map."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")))
        (clime-invoke--values '()))
    (clime-invoke--set-param opt "42")
    (should (equal "42" (clime-values-value clime-invoke--values 'limit)))
    (should (eq 'user (clime-values-source clime-invoke--values 'limit)))))

(ert-deftest clime-test-invoke/clear-param-removes-entry ()
  "clear-param removes the entry from the values map entirely."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")))
        (clime-invoke--values (clime-values-set '() 'limit "42" 'user)))
    (clime-invoke--clear-param opt)
    (should-not (clime-values-get clime-invoke--values 'limit))))

(ert-deftest clime-test-invoke/run-handler-does-not-mutate-values ()
  "run-handler copies values; original clime-invoke--values not mutated."
  (let* ((handler (lambda (_ctx) nil))
         (opt (clime-make-option :name 'limit :flags '("--limit") :default "10"))
         (cmd (clime-make-command :name "run" :handler handler
                                   :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--values '()))
    (clime-invoke--run-handler app cmd '("run"))
    ;; Default applied inside run-handler should not leak back
    (should-not (clime-values-get clime-invoke--values 'limit))))

(ert-deftest clime-test-invoke/validate-all-conformer-with-params-attribution ()
  "Conformer signaling :params puts errors in param-errors, not general-errors."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error
                            '("a and b conflict" :params (a b)))))
         (opt-a (clime-make-option :name 'a :flags '("--a")))
         (opt-b (clime-make-option :name 'b :flags '("--b")))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-a opt-b)
                                  :conform (list conform)))
         (clime-invoke--values (clime-values-set
                                (clime-values-set '() 'a "1" 'user)
                                'b "2" 'user)))
    (let ((result (clime-invoke--validate-all cmd)))
      (should (assq 'a (car result)))
      (should (assq 'b (car result)))
      ;; Attributed errors should NOT appear in general-errors
      (should-not (cdr result)))))

;;; ─── Stale Registry Refresh ─────────────────────────────────────────────

(ert-deftest clime-test-invoke/stale-flag-nil-by-default ()
  "The stale flag starts as nil."
  (let ((clime-invoke--stale nil))
    (should-not clime-invoke--stale)))

(ert-deftest clime-test-invoke/refresh-picks-up-rebound-symbol ()
  "After stale flag is set, refresh replaces registry entry from symbol value."
  (let ((clime-invoke--registry (make-hash-table :test #'equal))
        (clime-invoke--stale t))
    (let* ((old-app (clime-make-app :name "testapp" :version "1.0" :children nil))
           (new-app (clime-make-app :name "testapp" :version "2.0" :children nil))
           ;; Use an uninterned-like symbol in a known package for isolation
           (sym (intern "testapp")))
      (puthash "testapp" old-app clime-invoke--registry)
      ;; Simulate user re-evaluating their app definition
      (let ((old-val (and (boundp sym) (symbol-value sym))))
        (unwind-protect
            (progn
              (set sym new-app)
              (clime-invoke--refresh-registry)
              (should (eq new-app (gethash "testapp" clime-invoke--registry)))
              (should-not clime-invoke--stale))
          ;; Restore
          (if old-val (set sym old-val) (makunbound sym)))))))

(ert-deftest clime-test-invoke/refresh-keeps-entry-when-symbol-unbound ()
  "Refresh preserves cached entry when symbol is unbound."
  (let ((clime-invoke--registry (make-hash-table :test #'equal))
        (clime-invoke--stale t))
    (let ((app (clime-make-app :name "zzz-no-such-sym" :version "1.0" :children nil)))
      (puthash "zzz-no-such-sym" app clime-invoke--registry)
      (clime-invoke--refresh-registry)
      (should (eq app (gethash "zzz-no-such-sym" clime-invoke--registry)))
      (should-not clime-invoke--stale))))

(ert-deftest clime-test-invoke/refresh-keeps-entry-when-symbol-not-app ()
  "Refresh preserves cached entry when symbol is bound but not a clime-app."
  (let ((clime-invoke--registry (make-hash-table :test #'equal))
        (clime-invoke--stale t))
    (let* ((app (clime-make-app :name "testapp2" :version "1.0" :children nil))
           (sym (intern "testapp2")))
      (puthash "testapp2" app clime-invoke--registry)
      (let ((old-val (and (boundp sym) (symbol-value sym))))
        (unwind-protect
            (progn
              (set sym "not an app")
              (clime-invoke--refresh-registry)
              (should (eq app (gethash "testapp2" clime-invoke--registry))))
          (if old-val (set sym old-val) (makunbound sym)))))))

(ert-deftest clime-test-invoke/refresh-noop-when-not-stale ()
  "Refresh does nothing when stale flag is nil."
  (let ((clime-invoke--registry (make-hash-table :test #'equal))
        (clime-invoke--stale nil))
    (let* ((app (clime-make-app :name "testapp" :version "1.0" :children nil))
           (new-app (clime-make-app :name "testapp" :version "2.0" :children nil))
           (sym (intern "testapp")))
      (puthash "testapp" app clime-invoke--registry)
      (let ((old-val (and (boundp sym) (symbol-value sym))))
        (unwind-protect
            (progn
              (set sym new-app)
              (clime-invoke--refresh-registry)
              ;; Should NOT have refreshed — still old app
              (should (eq app (gethash "testapp" clime-invoke--registry))))
          (if old-val (set sym old-val) (makunbound sym)))))))

;;; ─── 3-Column Layout: Key | Value | Desc ───────────────────────────────

(ert-deftest clime-test-invoke/render-option-value-before-desc ()
  "Option value appears before help text (3-column: Key | Value | Desc)."
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                  :nargs 0 :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'verbose t 'user))
         (content (clime-invoke--render-to-string cmd nil nil)))
    ;; Find the option line (contains "-v" key)
    (let ((line (cl-find-if (lambda (l) (string-match-p "-v" l))
                            (split-string content "\n"))))
      (should line)
      ;; On that line, "on" (value) should appear before "Be verbose" (desc)
      (let ((val-pos (string-match-p "\\bon\\b" line))
            (desc-pos (string-match-p "Be verbose" line)))
        (should val-pos)
        (should desc-pos)
        (should (< val-pos desc-pos))))))

(ert-deftest clime-test-invoke/render-option-unset-value-before-desc ()
  "Unset option value marker appears before help text."
  (let* ((opt (clime-make-option :name 'output :flags '("--output" "-o")
                                  :help "Output file"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values '())
         (content (clime-invoke--render-to-string cmd nil nil)))
    ;; "(unset)" should appear before "Output file"
    (let ((val-pos (string-match-p "(unset)" content))
          (desc-pos (string-match-p "Output file" content)))
      (should val-pos)
      (should desc-pos)
      (should (< val-pos desc-pos)))))

(ert-deftest clime-test-invoke/render-arg-value-before-desc ()
  "Arg value appears before help text in 3-column layout."
  (let* ((arg (clime-make-arg :name 'file :help "The file to process"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (clime-invoke--values (clime-values-set '() 'file "test.txt" 'user))
         (content (clime-invoke--render-to-string cmd nil nil)))
    (let ((val-pos (string-match-p "test\\.txt" content))
          (desc-pos (string-match-p "The file to process" content)))
      (should val-pos)
      (should desc-pos)
      (should (< val-pos desc-pos)))))

(ert-deftest clime-test-invoke/render-env-in-desc-not-separate ()
  "Env annotation [$VAR] appears in desc column, env value in value column."
  (let* ((opt (clime-make-option :name 'home :flags '("--home")
                                  :env "HOME" :help "Home directory"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (clime-invoke--values '()))
    (setf (clime-node-parent cmd) app)
    (let ((content (clime-invoke--render-to-string cmd nil nil)))
      ;; Find the option line
      (let ((line (cl-find-if (lambda (l) (string-match-p "Home directory" l))
                              (split-string content "\n"))))
        (should line)
        ;; Value ($=...) should come before "Home directory"
        (let ((val-pos (string-match-p "(\\$=" line))
              (desc-pos (string-match-p "Home directory" line))
              (env-pos (string-match-p "\\[\\$HOME\\]" line)))
          (should val-pos)
          (should desc-pos)
          (should env-pos)
          ;; 3-column: env-value before desc, [$HOME] annotation after desc
          (should (< val-pos desc-pos))
          (should (< desc-pos env-pos)))))))

(ert-deftest clime-test-invoke/render-choices-value-before-desc ()
  "Choices render in value column, before desc."
  (let* ((opt (clime-make-option :name 'format :flags '("--format" "-f")
                                  :choices '("json" "csv" "html")
                                  :help "Output format"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'format "json" 'user))
         (content (clime-invoke--render-to-string cmd nil nil)))
    ;; The choices display (containing "json") should come before "Output format"
    (let ((val-pos (string-match-p "json" content))
          (desc-pos (string-match-p "Output format" content)))
      (should val-pos)
      (should desc-pos)
      (should (< val-pos desc-pos)))))

(ert-deftest clime-test-invoke/compute-value-width-basic ()
  "Value width is at least 5 and reflects actual formatted values."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0))
         (clime-invoke--values '()))
    ;; "off" is 3 chars, min is 5
    (should (>= (clime-invoke--compute-value-width (list opt) nil) 5))))

(ert-deftest clime-test-invoke/compute-value-width-capped ()
  "Value width is capped at `clime-invoke--max-value-width'."
  (let* ((opt (clime-make-option :name 'format :flags '("--format")
                                  :choices '("json" "csv" "html" "xml" "yaml" "toml")))
         (clime-invoke--values (clime-values-set '() 'format "json" 'user)))
    (should (<= (clime-invoke--compute-value-width (list opt) nil)
                clime-invoke--max-value-width))))

(ert-deftest clime-test-invoke/render-locked-value-before-desc ()
  "Locked option renders value before desc in 3-column layout."
  (let* ((opt (clime-make-option :name 'target :flags '("--target")
                                  :locked t :help "Target env"))
         (cmd (clime-make-command :name "deploy" :handler #'ignore
                                   :options (list opt)))
         (clime-invoke--values (clime-values-set '() 'target "prod" 'app)))
    (let* ((content (clime-invoke--render-to-string cmd nil nil))
           (line (cl-find-if (lambda (l) (string-match-p "Target env" l))
                             (split-string content "\n"))))
      (should line)
      ;; Value "prod" before desc "Target env"
      (let ((val-pos (string-match-p "prod" line))
            (desc-pos (string-match-p "Target env" line)))
        (should val-pos)
        (should desc-pos)
        (should (< val-pos desc-pos))))))

(ert-deftest clime-test-invoke/format-choices-compact ()
  "Choices use compact pipe-separated format without spaces."
  (let ((result (clime-invoke--format-choices '("json" "csv" "html") nil nil)))
    ;; No spaces around pipes
    (should (string-match-p "json|csv|html" (substring-no-properties result)))))

(ert-deftest clime-test-invoke/format-choices-inactive-shadow ()
  "Inactive choices are in shadow face."
  (let ((result (clime-invoke--format-choices '("json" "csv" "html") "json" nil)))
    ;; "csv" should be in shadow face
    (let ((csv-pos (string-match "csv" result)))
      (should csv-pos)
      (should (eq 'shadow (get-text-property csv-pos 'face result))))))

(ert-deftest clime-test-invoke/format-choices-default-parens ()
  "Default choice shown in parens when nothing selected."
  (let ((result (clime-invoke--format-choices '("json" "csv" "html") nil "csv")))
    (should (string-match-p "(csv)" (substring-no-properties result)))))

(ert-deftest clime-test-invoke/format-choices-env-valid ()
  "Env-derived value in choices shows as ($=val)."
  (let ((result (clime-invoke--format-choices '("json" "csv" "html") nil nil "csv")))
    (should (string-match-p "(\\$=csv)" (substring-no-properties result)))))

(ert-deftest clime-test-invoke/format-choices-env-invalid ()
  "Env-derived value not in choices is appended as ($=val)."
  (let ((result (clime-invoke--format-choices '("json" "csv" "html") nil nil "xml")))
    ;; All original choices present
    (should (string-match-p "json" (substring-no-properties result)))
    ;; Invalid env val appended
    (should (string-match-p "(\\$=xml)" (substring-no-properties result)))
    ;; Invalid env val in error face
    (let ((pos (string-match "xml" result)))
      (should pos)
      (should (eq 'clime-invoke-error (get-text-property pos 'face result))))))

(ert-deftest clime-test-invoke/format-choices-env-ignored-when-selected ()
  "Env-derived value ignored when explicit selection exists."
  (let ((result (clime-invoke--format-choices '("json" "csv" "html") "json" nil "csv")))
    ;; Should not show ($=csv)
    (should-not (string-match-p "(\\$=" (substring-no-properties result)))))

;;; ─── Ask / Immediate Tests ──────────────────────────────────────────

(defun clime-test--invoke-immediate-app ()
  "Build an app with required args for ask/immediate tests."
  (let* ((opt-verbose (clime-make-option :name 'verbose
                                          :flags '("--verbose" "-v")
                                          :nargs 0
                                          :help "Be verbose"))
         (opt-output (clime-make-option :name 'output
                                         :flags '("--output" "-o")
                                         :required t
                                         :help "Output file"))
         (opt-format (clime-make-option :name 'format
                                         :flags '("--format" "-f")
                                         :choices '("json" "text")
                                         :help "Output format"))
         (arg-name (clime-make-arg :name 'name :help "Resource name"))
         (handler-called nil)
         (cmd (clime-make-command :name "run"
                                   :help "Run it"
                                   :handler (lambda (_ctx)
                                              (setq handler-called t)
                                              (princ "ok")
                                              nil)
                                   :options (list opt-output opt-format)
                                   :args (list arg-name))))
    (list (clime-make-app :name "testapp"
                           :version "1"
                           :help "Test"
                           :options (list opt-verbose)
                           :children (list (cons "run" cmd)))
          (lambda () handler-called))))

;;; ─── Unit tests for ask/immediate helpers ──────────────────────────

(ert-deftest clime-test-invoke/collect-ask-params-t-returns-required ()
  "collect-ask-params with t returns required params only."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree nil)
      (let ((params (clime-invoke--collect-ask-params node t)))
        ;; output (required option) + name (required arg)
        (should (= 2 (length params)))
        (should (cl-find 'output params :key #'clime-param-name))
        (should (cl-find 'name params :key #'clime-param-name))
        ;; format (not required) should not be included
        (should-not (cl-find 'format params :key #'clime-param-name))))))

(ert-deftest clime-test-invoke/collect-ask-params-list-returns-named ()
  "collect-ask-params with a list returns only those params."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree nil)
      (let ((params (clime-invoke--collect-ask-params node '(format))))
        (should (= 1 (length params)))
        (should (eq 'format (clime-param-name (car params))))))))

(ert-deftest clime-test-invoke/collect-ask-params-nil-returns-empty ()
  "collect-ask-params with nil returns nil."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree nil)
      (should-not (clime-invoke--collect-ask-params node nil)))))

(ert-deftest clime-test-invoke/collect-ask-params-skips-user-seeded ()
  "collect-ask-params with t skips params already seeded by user."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree '(output "/pre"))
      (let ((params (clime-invoke--collect-ask-params node t)))
        ;; Only name (required arg) — output already seeded
        (should (= 1 (length params)))
        (should (eq 'name (clime-param-name (car params))))))))

(ert-deftest clime-test-invoke/collect-ask-params-unknown-ignored ()
  "collect-ask-params with unknown names returns empty."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree nil)
      (should-not (clime-invoke--collect-ask-params node '(nonexistent))))))

(ert-deftest clime-test-invoke/all-required-satisfied-p-true ()
  "all-required-satisfied-p returns t when all required params have values."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree '(output "/tmp" name "foo"))
      (should (clime-invoke--all-required-satisfied-p node)))))

(ert-deftest clime-test-invoke/all-required-satisfied-p-false ()
  "all-required-satisfied-p returns nil when a required param is missing."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree '(name "foo"))
      ;; output is required but missing
      (should-not (clime-invoke--all-required-satisfied-p node)))))

(ert-deftest clime-test-invoke/prompt-params-returns-t-on-completion ()
  "prompt-params returns t when all prompts complete."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree nil)
      (let ((params (clime-invoke--collect-ask-params node '(name))))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt &rest _) "test-val")))
          (should (eq t (clime-invoke--prompt-params params))))))))

(ert-deftest clime-test-invoke/prompt-params-returns-nil-on-quit ()
  "prompt-params returns nil when user quits."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let* ((tree (clime--prepare-tree app))
           (node (cdr (assoc "run" (clime-group-children tree))))
           (clime-invoke--values nil))
      (clime-invoke--seed-values tree nil)
      (let ((params (clime-invoke--collect-ask-params node '(name))))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (signal 'quit nil))))
          (should-not (clime-invoke--prompt-params params)))))))

;; ─── Integration: immediate with conformer error falls to menu ──────

(ert-deftest clime-test-invoke/immediate-conformer-error-falls-to-menu ()
  "`:immediate t' falls to menu when conformer check fails."
  (let* ((opt (clime-make-option :name 'output :flags '("--output" "-o")
                                  :required t :help "Output"))
         (cmd (clime-make-command
               :name "run" :help "Run"
               :handler #'ignore
               :options (list opt)
               :conform (lambda (_values _node)
                          (signal 'clime-usage-error
                                  '("bad value" :params (output))))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (loop-entered nil))
    (cl-letf (((symbol-function 'clime-invoke--loop)
               (lambda (_app _node _path &optional _top)
                 (setq loop-entered t)
                 (list nil nil)))
              ((symbol-function 'clime-invoke--display-output)
               #'ignore))
      (clime-invoke app '("run") '(output "/tmp/out") :immediate t)
      (should loop-entered))))

;; ─── Integration: :immediate with explicit :ask overrides default ───

(ert-deftest clime-test-invoke/immediate-explicit-ask-overrides-default ()
  "`:immediate t :ask (format)' asks only for format, not all required."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((prompted nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (push prompt prompted)
                   (car choices)))
                ((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (list nil nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        ;; :ask '(format) overrides the implied :ask t from :immediate
        (clime-invoke app '("run") '(output "/tmp" name "foo")
                      :immediate t :ask '(format))
        ;; Should only prompt for format
        (should (= 1 (length prompted)))))))

;; AC 1: :ask t prompts required params then opens menu
(ert-deftest clime-test-invoke/ask-t-prompts-required-params ()
  "`:ask t' prompts for all required params, then enters menu loop."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((prompted nil)
          (loop-entered nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (push prompt prompted)
                   "/tmp/out"))
                ((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (setq loop-entered t)
                   ;; Verify values were seeded by ask phase
                   (should (equal "/tmp/out"
                                  (clime-values-value
                                   clime-invoke--values 'output)))
                   (list '(0 . "done") nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        (clime-invoke app '("run") nil :ask t)
        (should loop-entered)
        ;; Should have prompted for output (required option) and name (required arg)
        (should (>= (length prompted) 2))))))

;; AC 2: :ask with specific param list prompts only those
(ert-deftest clime-test-invoke/ask-specific-params ()
  "`:ask (format)' prompts only for format, then enters menu loop."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((prompted-params nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (push prompt prompted-params)
                   (car choices)))
                ((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (list '(0 . "done") nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        (clime-invoke app '("run") nil :ask '(format))
        ;; Should have prompted exactly once (for format)
        (should (= 1 (length prompted-params)))))))

;; AC 3: :immediate t runs handler without showing menu
(ert-deftest clime-test-invoke/immediate-runs-without-menu ()
  "`:immediate t' prompts required params then runs handler, never enters loop."
  (cl-destructuring-bind (app handler-called-p) (clime-test--invoke-immediate-app)
    (let ((loop-entered nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &rest _) "/tmp/out"))
                ((symbol-function 'clime-invoke--loop)
                 (lambda (&rest _)
                   (setq loop-entered t)
                   (list nil nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        (let ((result (clime-invoke app '("run") '(name "foo")
                                    :immediate t)))
          (should-not loop-entered)
          (should (funcall handler-called-p))
          (should (= 0 (plist-get result :exit)))
          (should (equal "ok" (plist-get result :output))))))))

;; AC 4: :immediate with all params pre-filled asks y-or-n-p
(ert-deftest clime-test-invoke/immediate-all-filled-asks-confirmation ()
  "`:immediate t' with all required params pre-filled asks y-or-n-p."
  (cl-destructuring-bind (app handler-called-p) (clime-test--invoke-immediate-app)
    (let ((yn-called nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq yn-called t) t))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore)
                ((symbol-function 'clime-invoke--loop)
                 (lambda (&rest _) (list nil nil))))
        (let ((result (clime-invoke app '("run")
                                    '(output "/tmp/out" name "foo")
                                    :immediate t)))
          (should yn-called)
          (should (funcall handler-called-p))
          (should (= 0 (plist-get result :exit))))))))

;; AC 4b: y-or-n-p declined → returns nil exit
(ert-deftest clime-test-invoke/immediate-confirmation-declined ()
  "`:immediate t' with y-or-n-p declined returns nil exit, does not run."
  (cl-destructuring-bind (app handler-called-p) (clime-test--invoke-immediate-app)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'clime-invoke--display-output)
               #'ignore)
              ((symbol-function 'clime-invoke--loop)
               (lambda (&rest _) (list nil nil))))
      (let ((result (clime-invoke app '("run")
                                  '(output "/tmp/out" name "foo")
                                  :immediate t)))
        (should-not (funcall handler-called-p))
        (should-not (plist-get result :exit))))))

;; AC 5: :immediate falls into menu when required params remain unfilled
(ert-deftest clime-test-invoke/immediate-falls-to-menu-on-missing ()
  "`:immediate t' enters menu loop when required params still missing after ask."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((loop-entered nil))
      ;; Simulate user leaving output empty during ask
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &rest _) ""))
                ((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (setq loop-entered t)
                   (list '(0 . "done") nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        (clime-invoke app '("run") nil :immediate t)
        (should loop-entered)))))

;; AC 6: C-g during ask phase returns nil exit/output
(ert-deftest clime-test-invoke/ask-quit-returns-nil ()
  "C-g during ask phase returns nil :exit and :output."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'clime-invoke--loop)
               (lambda (&rest _) (error "Loop should not be entered")))
              ((symbol-function 'clime-invoke--display-output)
               #'ignore))
      (let ((result (clime-invoke app '("run") nil :ask t)))
        (should-not (plist-get result :exit))
        (should-not (plist-get result :output))))))

;; AC 7: :immediate on group node (no handler) enters menu normally
(ert-deftest clime-test-invoke/immediate-on-group-shows-menu ()
  "`:immediate t' on a group node (no handler) falls into menu loop."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((loop-entered nil))
      (cl-letf (((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (setq loop-entered t)
                   (list nil nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        ;; Invoke at root (a group, not a command with handler)
        (clime-invoke app nil nil :immediate t)
        (should loop-entered)))))

;; AC 8: :ask with unknown param names silently ignored
(ert-deftest clime-test-invoke/ask-unknown-params-ignored ()
  "`:ask' with unknown param names does not error."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((loop-entered nil))
      (cl-letf (((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (setq loop-entered t)
                   (list nil nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        (clime-invoke app '("run") nil :ask '(nonexistent-param))
        (should loop-entered)))))

;; AC 9: params already provided via plist are not re-prompted
(ert-deftest clime-test-invoke/ask-skips-preseeded-params ()
  "`:ask t' does not prompt for params already in the params plist."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((prompted nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (push prompt prompted)
                   "asked-value"))
                ((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (list '(0 . "done") nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        ;; Pre-seed output — should NOT be prompted for it
        (clime-invoke app '("run") '(output "/pre") :ask t)
        ;; Should only have been prompted for name (the required arg),
        ;; not output (already seeded)
        (should prompted)
        (should-not (cl-find "Output file" prompted :test #'string-match-p))))))

;; AC 10: backward compat — no :ask/:immediate behaves identically
(ert-deftest clime-test-invoke/no-ask-immediate-backward-compat ()
  "clime-invoke without :ask/:immediate enters loop directly."
  (cl-destructuring-bind (app _) (clime-test--invoke-immediate-app)
    (let ((loop-entered nil))
      (cl-letf (((symbol-function 'clime-invoke--loop)
                 (lambda (_app _node _path &optional _top)
                   (setq loop-entered t)
                   (list nil nil)))
                ((symbol-function 'clime-invoke--display-output)
                 #'ignore))
        (clime-invoke app '("run") nil)
        (should loop-entered)))))

(provide 'clime-invoke-tests)
;;; clime-invoke-tests.el ends here
