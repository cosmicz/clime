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
         (params '(verbose t))
         (content (clime-invoke--render-to-string cmd params nil nil)))
    (should (string-match-p "Be verbose" content))))

(ert-deftest clime-test-invoke/render-shows-children ()
  "Rendered buffer contains child names."
  (let* ((cmd (clime-make-command :name "show" :help "Show resource"
                                   :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,cmd))))
         (content (clime-invoke--render-to-string app nil nil nil)))
    (should (string-match-p "show" content))
    (should (string-match-p "Show resource" content))))

(ert-deftest clime-test-invoke/render-shows-count-level ()
  "Count option displays ×N."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v")
                                  :nargs 0 :count t :help "Verbosity"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (params '(verbose 3))
         (content (clime-invoke--render-to-string cmd params nil nil)))
    (should (string-match-p "×3" content))))

(ert-deftest clime-test-invoke/render-shows-error ()
  "Error message appears in rendered output."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string
                   cmd nil nil "Exit 2: unknown flag")))
    (should (string-match-p "Exit 2" content))))

(ert-deftest clime-test-invoke/render-shows-help-text ()
  "Node help text appears at the top."
  (let* ((cmd (clime-make-command :name "deploy" :handler #'ignore
                                   :help "Deploy to production"))
         (content (clime-invoke--render-to-string cmd nil nil nil)))
    (should (string-match-p "Deploy to production" content))))

(ert-deftest clime-test-invoke/render-hides-hidden-children ()
  "Hidden children are not rendered."
  (let* ((app (clime-test--invoke-simple-app))
         (admin (cdr (assoc "admin" (clime-group-children app))))
         (content (clime-invoke--render-to-string admin nil nil nil)))
    (should (string-match-p "show" content))
    (should (string-match-p "create" content))
    (should-not (string-match-p "debug" content))))

;;; ─── Plist Remove ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/plist-remove-existing ()
  "Removing an existing key returns plist without it."
  (should (equal '(a 1) (clime-invoke--plist-remove '(a 1 b 2) 'b))))

(ert-deftest clime-test-invoke/plist-remove-missing ()
  "Removing a missing key returns plist unchanged."
  (should (equal '(a 1) (clime-invoke--plist-remove '(a 1) 'b))))

(ert-deftest clime-test-invoke/plist-remove-empty ()
  "Removing from empty plist returns nil."
  (should (null (clime-invoke--plist-remove nil 'a))))

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
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (let ((p (clime-invoke--handle-option opt nil)))
      (should (eq t (plist-get p 'verbose))))
    (let ((p (clime-invoke--handle-option opt '(verbose t))))
      (should-not (plist-member p 'verbose)))))

(ert-deftest clime-test-invoke/handle-count-increment ()
  "Count option increments."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (let ((p (clime-invoke--handle-option opt nil)))
      (should (= 1 (plist-get p 'verbose))))
    (let ((p (clime-invoke--handle-option opt '(verbose 2))))
      (should (= 3 (plist-get p 'verbose))))))

(ert-deftest clime-test-invoke/handle-choices-cycle ()
  "Choices option cycles through values."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text"))))
    (let ((p (clime-invoke--handle-option opt nil)))
      (should (equal "json" (plist-get p 'format))))
    (let ((p (clime-invoke--handle-option opt '(format "json"))))
      (should (equal "text" (plist-get p 'format))))
    (let ((p (clime-invoke--handle-option opt '(format "text"))))
      (should-not (plist-member p 'format)))))

(ert-deftest clime-test-invoke/handle-ternary-cycle ()
  "Negatable option cycles nil → flag → no-flag → nil."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t)))
    (let ((p (clime-invoke--handle-option opt nil)))
      (should (equal "--color" (plist-get p 'color))))
    (let ((p (clime-invoke--handle-option opt '(color "--color"))))
      (should (equal "--no-color" (plist-get p 'color))))
    (let ((p (clime-invoke--handle-option opt '(color "--no-color"))))
      (should-not (plist-member p 'color)))))

(ert-deftest clime-test-invoke/handle-ternary-short-flag-first ()
  "Negatable option works when short flag listed before long flag."
  (let ((opt (clime-make-option :name 'color :flags '("-c" "--color")
                                 :negatable t)))
    (let ((p (clime-invoke--handle-option opt nil)))
      (should (equal "--color" (plist-get p 'color))))
    (let ((p (clime-invoke--handle-option opt '(color "--color"))))
      (should (equal "--no-color" (plist-get p 'color))))
    (let ((p (clime-invoke--handle-option opt '(color "--no-color"))))
      (should-not (plist-member p 'color)))))

;;; ─── Handle Option Direct ────────────────────────────────────────────

(ert-deftest clime-test-invoke/direct-choices-completing-read ()
  "Direct input on choices option uses completing-read."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text" "csv"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _) (car (last coll)))))
      (let ((p (clime-invoke--handle-option-direct opt nil)))
        (should (equal "csv" (plist-get p 'format)))))))

(ert-deftest clime-test-invoke/direct-choices-empty-clears ()
  "Direct input on choices with empty string clears value."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "")))
      (let ((p (clime-invoke--handle-option-direct opt '(format "json"))))
        (should-not (plist-member p 'format))))))

(ert-deftest clime-test-invoke/direct-count-sets-value ()
  "Direct input on count option sets specific number."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "3")))
      (let ((p (clime-invoke--handle-option-direct opt nil)))
        (should (= 3 (plist-get p 'verbose)))))))

(ert-deftest clime-test-invoke/direct-count-zero-clears ()
  "Direct input on count with 0 clears value."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "0")))
      (let ((p (clime-invoke--handle-option-direct opt '(verbose 3))))
        (should-not (plist-member p 'verbose))))))

(ert-deftest clime-test-invoke/direct-count-invalid-errors ()
  "Direct input on count with non-numeric input signals error."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "abc")))
      (should-error (clime-invoke--handle-option-direct opt nil)))))

(ert-deftest clime-test-invoke/direct-boolean-delegates ()
  "Direct input on boolean delegates to cycling handler."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (let ((p (clime-invoke--handle-option-direct opt nil)))
      (should (eq t (plist-get p 'verbose))))))

(ert-deftest clime-test-invoke/direct-negatable-delegates ()
  "Direct input on negatable delegates to cycling handler."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t)))
    (let ((p (clime-invoke--handle-option-direct opt nil)))
      (should (equal "--color" (plist-get p 'color))))))

(ert-deftest clime-test-invoke/direct-count-negative-errors ()
  "Direct input on count rejects negative numbers."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "-1")))
      (should-error (clime-invoke--handle-option-direct opt nil)))))

(ert-deftest clime-test-invoke/direct-count-over-max-errors ()
  "Direct input on count rejects values > 5."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "6")))
      (should-error (clime-invoke--handle-option-direct opt nil)))))

(ert-deftest clime-test-invoke/direct-multiple-delegates ()
  "Direct input on multiple option delegates to cycling handler."
  (let ((opt (clime-make-option :name 'tag :flags '("--tag")
                                 :multiple t)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "foo")))
      (let ((p (clime-invoke--handle-option-direct opt nil)))
        (should (equal '("foo") (plist-get p 'tag)))))))

(ert-deftest clime-test-invoke/direct-plain-delegates ()
  "Direct input on plain value option delegates to cycling handler."
  (let ((opt (clime-make-option :name 'output :flags '("--output"))))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) "/tmp/out")))
      (let ((p (clime-invoke--handle-option-direct opt nil)))
        (should (equal "/tmp/out" (plist-get p 'output)))))))

;;; ─── Format Value ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-value-boolean ()
  "Boolean shows on/off."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (should (string-match-p "on" (clime-invoke--format-value opt '(verbose t))))
    (should (string-match-p "off" (clime-invoke--format-value opt nil)))))

(ert-deftest clime-test-invoke/format-value-ternary-short-first ()
  "Ternary display works when short flag listed first."
  (let ((opt (clime-make-option :name 'color :flags '("-c" "--color")
                                 :negatable t)))
    (should (string-match-p "on" (clime-invoke--format-value opt '(color "--color"))))
    (should (string-match-p "off" (clime-invoke--format-value opt '(color "--no-color"))))
    (should (string-match-p "auto" (clime-invoke--format-value opt nil)))))

(ert-deftest clime-test-invoke/format-value-count ()
  "Count shows ×N or off."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v")
                                 :nargs 0 :count t)))
    (should (string-match-p "×3" (clime-invoke--format-value opt '(verbose 3))))
    (should (string-match-p "off" (clime-invoke--format-value opt nil)))))

(ert-deftest clime-test-invoke/format-value-ternary ()
  "Ternary shows on/off/auto."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :negatable t)))
    (should (string-match-p "on" (clime-invoke--format-value opt '(color "--color"))))
    (should (string-match-p "off" (clime-invoke--format-value opt '(color "--no-color"))))
    (should (string-match-p "auto" (clime-invoke--format-value opt nil)))))

(ert-deftest clime-test-invoke/format-value-choices ()
  "Choices show all options inline; selected highlighted."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text"))))
    (let ((with-val (clime-invoke--format-value opt '(format "json")))
          (without (clime-invoke--format-value opt nil)))
      ;; Both cases show all choices
      (should (string-match-p "json" with-val))
      (should (string-match-p "text" with-val))
      (should (string-match-p "json" without))
      (should (string-match-p "text" without)))))

(ert-deftest clime-test-invoke/format-value-choices-inline ()
  "Choices displayed inline with all options visible."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text" "csv"))))
    (let ((val-str (clime-invoke--format-value opt '(format "json"))))
      ;; All choices appear
      (should (string-match-p "json" val-str))
      (should (string-match-p "text" val-str))
      (should (string-match-p "csv" val-str)))))

(ert-deftest clime-test-invoke/format-value-choices-default-parens ()
  "Default value in choices list shown in parens."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text" "csv")
                                 :default "text")))
    (let ((val-str (clime-invoke--format-value opt nil)))
      ;; Default shown in parens, not highlighted
      (should (string-match-p "(text)" val-str)))))

(ert-deftest clime-test-invoke/format-value-default-no-choices ()
  "Default value without choices shown in parens."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")
                                 :default "10")))
    (let ((val-str (clime-invoke--format-value opt nil)))
      (should (string-match-p "(10)" val-str)))))

(ert-deftest clime-test-invoke/format-value-explicit-has-active-face ()
  "Explicitly set value has clime-invoke-active face."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit"))))
    (let ((val-str (clime-invoke--format-value opt '(limit "20"))))
      (should (string-match-p "20" val-str))
      ;; Check the face property
      (let ((pos (string-match "20" val-str)))
        (should (eq 'clime-invoke-active
                    (get-text-property pos 'face val-str)))))))

(ert-deftest clime-test-invoke/format-value-default-no-active-face ()
  "Default value does NOT have clime-invoke-active face."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit")
                                 :default "10")))
    (let ((val-str (clime-invoke--format-value opt nil)))
      (let ((pos (string-match "10" val-str)))
        (should-not (eq 'clime-invoke-active
                        (get-text-property pos 'face val-str)))))))

(ert-deftest clime-test-invoke/format-value-choices-truncated ()
  "Large choices list truncated around selected value."
  (let ((opt (clime-make-option :name 'color :flags '("--color")
                                 :choices '("red" "orange" "yellow" "green"
                                            "blue" "indigo" "violet"))))
    (let ((val-str (clime-invoke--format-value opt '(color "blue"))))
      ;; Selected value visible
      (should (string-match-p "blue" val-str))
      ;; Ellipsis indicates truncation
      (should (string-match-p "\\.\\.\\." val-str)))))

(ert-deftest clime-test-invoke/format-value-choices-no-value-no-default ()
  "Choices with no value and no default shows all unhighlighted."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text"))))
    (let ((val-str (clime-invoke--format-value opt nil)))
      (should (string-match-p "json" val-str))
      (should (string-match-p "text" val-str))
      ;; No active face on either
      (let ((pos (string-match "json" val-str)))
        (should-not (eq 'clime-invoke-active
                        (get-text-property pos 'face val-str)))))))

(ert-deftest clime-test-invoke/format-value-choices-five-no-truncation ()
  "Exactly 5 choices: no truncation."
  (let ((opt (clime-make-option :name 'c :flags '("--c")
                                 :choices '("a" "b" "c" "d" "e"))))
    (let ((val-str (clime-invoke--format-value opt '(c "c"))))
      (should (string-match-p "a" val-str))
      (should (string-match-p "e" val-str))
      (should-not (string-match-p "\\.\\.\\." val-str)))))

(ert-deftest clime-test-invoke/format-desc-arg ()
  "Format-desc works for args (not just options)."
  (let ((arg (clime-make-arg :name 'file :help "File path")))
    (let ((desc (clime-invoke--format-desc arg nil)))
      (should (string-match-p "File path" desc)))))

(ert-deftest clime-test-invoke/format-desc-arg-required ()
  "Format-desc shows required annotation for unset required arg."
  (let ((arg (clime-make-arg :name 'file :help "File path")))
    (let ((desc (clime-invoke--format-desc arg nil)))
      (should (string-match-p "(required)" desc)))))

(ert-deftest clime-test-invoke/header-root-no-version ()
  "Root without version shows name only."
  (let* ((app (clime-make-app :name "myapp" :children nil))
         (content (clime-invoke--render-to-string app nil nil nil t)))
    (should (string-match-p "myapp" content))))

;;; ─── Format Desc ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-desc-help-and-long-flag ()
  "Desc column shows help text with long flag in parens."
  (let ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                 :nargs 0 :help "Be verbose")))
    (let ((desc (clime-invoke--format-desc opt nil)))
      (should (string-match-p "Be verbose" desc))
      (should (string-match-p "(--verbose)" desc)))))

(ert-deftest clime-test-invoke/format-desc-no-help ()
  "Desc column shows long flag when no help text."
  (let ((opt (clime-make-option :name 'limit :flags '("--limit"))))
    (let ((desc (clime-invoke--format-desc opt nil)))
      (should (string-match-p "--limit" desc)))))

(ert-deftest clime-test-invoke/format-desc-required ()
  "Desc column shows required annotation."
  (let ((opt (clime-make-option :name 'output :flags '("--output")
                                 :required t :help "Output file")))
    (let ((desc (clime-invoke--format-desc opt nil)))
      (should (string-match-p "(required)" desc)))))

(ert-deftest clime-test-invoke/format-desc-deprecated ()
  "Desc column shows deprecated annotation."
  (let ((opt (clime-make-option :name 'old :flags '("--old")
                                 :deprecated t :help "Old option")))
    (let ((desc (clime-invoke--format-desc opt nil)))
      (should (string-match-p "(deprecated)" desc)))))

(ert-deftest clime-test-invoke/format-desc-type-hint ()
  "Desc column shows type hint for non-string types."
  (let ((opt (clime-make-option :name 'count :flags '("--count")
                                 :type 'integer :help "Count")))
    (let ((desc (clime-invoke--format-desc opt nil)))
      (should (string-match-p "(integer)" desc)))))

(ert-deftest clime-test-invoke/format-desc-multi-hint ()
  "Desc column shows multi hint for multiple options."
  (let ((opt (clime-make-option :name 'tag :flags '("--tag")
                                 :multiple t :help "Tags")))
    (let ((desc (clime-invoke--format-desc opt nil)))
      (should (string-match-p "(multi)" desc)))))

;;; ─── Format Env ─────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-env-with-value ()
  "Env column shows var name and resolved value."
  (let ((opt (clime-make-option :name 'home :flags '("--home")
                                 :env "HOME")))
    (let ((env-str (clime-invoke--format-env opt nil)))
      (should (string-match-p "\\$HOME=" env-str))
      ;; Should have a resolved value (HOME is always set)
      (should (string-match-p "\\$HOME=/" env-str)))))

(ert-deftest clime-test-invoke/format-env-empty ()
  "Env column shows just var name when env is unset."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo")
                                 :env "CLIME_TEST_NONEXISTENT_VAR_XYZ")))
    (let ((env-str (clime-invoke--format-env opt nil)))
      (should (string-match-p "\\$CLIME_TEST_NONEXISTENT_VAR_XYZ" env-str))
      (should-not (string-match-p "=" env-str)))))

(ert-deftest clime-test-invoke/format-env-active-source ()
  "Env value highlighted when it's the active source."
  (let ((opt (clime-make-option :name 'home :flags '("--home")
                                 :env "HOME")))
    ;; No explicit value set — env is the active source
    (let ((env-str (clime-invoke--format-env opt nil)))
      (let ((eq-pos (string-match "=." env-str)))
        (when eq-pos
          (should (eq 'clime-invoke-active
                      (get-text-property (1+ eq-pos) 'face env-str))))))))

(ert-deftest clime-test-invoke/format-env-not-active-when-explicit ()
  "Env value NOT highlighted when explicit value is set."
  (let ((opt (clime-make-option :name 'home :flags '("--home")
                                 :env "HOME")))
    ;; Explicit value set — env is not the active source
    (let ((env-str (clime-invoke--format-env opt '(home "/custom"))))
      (let ((eq-pos (string-match "=." env-str)))
        (when eq-pos
          (should-not (eq 'clime-invoke-active
                          (get-text-property (1+ eq-pos) 'face env-str))))))))

(ert-deftest clime-test-invoke/format-env-nil-when-no-env ()
  "No env column when option has no :env slot."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (should (null (clime-invoke--format-env opt nil)))))

;;; ─── Breadcrumb Header ─────────────────────────────────────────────

(ert-deftest clime-test-invoke/header-root-shows-name-version ()
  "Root node header shows app name and version."
  (let* ((app (clime-make-app :name "myapp" :version "1.2.3"
                               :help "My app" :children nil))
         (content (clime-invoke--render-to-string app nil nil nil t)))
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
         (content (clime-invoke--render-to-string cmd nil nil nil)))
    (should (string-match-p "myapp" content))
    (should (string-match-p "admin" content))
    (should (string-match-p "show" content))
    (should (string-match-p "Show resource" content))))

;;; ─── Actions Footer ────────────────────────────────────────────────

(ert-deftest clime-test-invoke/actions-section ()
  "Actions section groups RET and q on the same or adjacent lines."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil nil t)))
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
         (content (clime-invoke--render-to-string cmd nil nil nil)))
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
         (content (clime-invoke--render-to-string app nil nil nil)))
    ;; Find "s" after "Commands" heading to avoid matching header text
    (let* ((cmds-pos (string-match "Commands" content))
           (pos (and cmds-pos (string-match "\\bs\\b" content (1+ cmds-pos)))))
      (should pos)
      (should (eq 'clime-invoke-command-key
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-action-key-face ()
  "Action keys (RET, q) use clime-invoke-action-key face."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil nil t)))
    (let ((pos (string-match "RET" content)))
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
         (content (clime-invoke--render-to-string cmd nil nil nil)))
    (should (string-match-p "Arguments" content))
    (should (string-match-p "File path" content))))

(ert-deftest clime-test-invoke/render-shows-required ()
  "Required marker shown for required args without values."
  (let* ((arg (clime-make-arg :name 'file :help "File path"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (content (clime-invoke--render-to-string cmd nil nil nil)))
    (should (string-match-p "(required)" content))))

(ert-deftest clime-test-invoke/render-required-dimmed-when-set ()
  "Required marker stays visible but dimmed when arg has a value."
  (let* ((arg (clime-make-arg :name 'file :help "File path"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (content (clime-invoke--render-to-string cmd '(file "test.txt") nil nil)))
    ;; Still shows (required) text
    (should (string-match-p "(required)" content))
    ;; But in shadow face, not warning
    (let ((pos (string-match "(required)" content)))
      (should (eq 'shadow (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-quit-at-root ()
  "Root shows 'Quit', non-root shows 'Return'."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (at-root (clime-invoke--render-to-string cmd nil nil nil t))
         (not-root (clime-invoke--render-to-string cmd nil nil nil nil)))
    (should (string-match-p "Quit" at-root))
    (should (string-match-p "Return" not-root))))

(ert-deftest clime-test-invoke/render-shows-run ()
  "RET → Run shown for commands with handlers."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil nil)))
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
    (clime-invoke--run-handler app cmd '("run") '(verbose t))
    (should called-with)
    (should (eq t (plist-get called-with 'verbose)))))

(ert-deftest clime-test-invoke/run-captures-output ()
  "Handler output is captured in the result."
  (let* ((handler (lambda (_ctx) (princ "hello world")))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (result (clime-invoke--run-handler app cmd '("run") nil)))
    (should (= 0 (car result)))
    (should (string-match-p "hello world" (cdr result)))))

(ert-deftest clime-test-invoke/run-validates-required ()
  "Missing required args produce exit code 2."
  (let* ((arg (clime-make-arg :name 'file :help "File"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (result (clime-invoke--run-handler app cmd '("run") nil)))
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
    (clime-invoke--run-handler app cmd '("run") nil)
    (should called-with)
    (should (equal "10" (plist-get called-with 'limit)))))

(ert-deftest clime-test-invoke/run-handler-error ()
  "Handler error produces exit code 1."
  (let* ((handler (lambda (_ctx) (error "boom")))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("run" . ,cmd))))
         (result (clime-invoke--run-handler app cmd '("run") nil)))
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
         (result (clime-invoke--run-handler app cmd '("run") nil)))
    (should (= 0 (car result)))
    (should (string-match-p "Run something" (cdr result)))))

(ert-deftest clime-test-invoke/run-buffered-errors-exit-1 ()
  "Handler that emits buffered errors returns exit code 1."
  (let* ((handler (lambda (_ctx)
                    (clime-output-error "something went wrong")))
         (cmd (clime-make-command :name "run" :handler handler))
         (app (clime-make-app :name "test" :version "1" :json-mode t
                               :children `(("run" . ,cmd))))
         ;; Bind a buffered (non-streaming) format so errors accumulate
         (clime--active-output-format
          (car (clime-app-output-formats app)))
         (result (clime-invoke--run-handler app cmd '("run") nil)))
    (should (= 1 (car result)))))

;;; ─── Validation Tests ──────────────────────────────────────────────────

(ert-deftest clime-test-invoke/validate-param-valid-string ()
  "Valid string value returns nil."
  (let ((opt (clime-make-option :name 'name :flags '("--name"))))
    (should-not (clime-invoke--validate-param opt '(name "hello")))))

(ert-deftest clime-test-invoke/validate-param-valid-integer ()
  "Valid integer string returns nil."
  (let ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer)))
    (should-not (clime-invoke--validate-param opt '(count "42")))))

(ert-deftest clime-test-invoke/validate-param-invalid-integer ()
  "Non-numeric string for integer type returns error string."
  (let ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer)))
    (should (stringp (clime-invoke--validate-param opt '(count "abc"))))))

(ert-deftest clime-test-invoke/validate-param-valid-choice ()
  "Value matching choices returns nil."
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices '("json" "text"))))
    (should-not (clime-invoke--validate-param opt '(fmt "json")))))

(ert-deftest clime-test-invoke/validate-param-invalid-choice ()
  "Value not in choices returns error string."
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices '("json" "text"))))
    (should (stringp (clime-invoke--validate-param opt '(fmt "xml"))))))

(ert-deftest clime-test-invoke/validate-param-unset-skipped ()
  "Unset param returns nil (no validation)."
  (let ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer)))
    (should-not (clime-invoke--validate-param opt '()))))

(ert-deftest clime-test-invoke/validate-param-non-string-skipped ()
  "Non-string value (e.g. boolean t) is not validated."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (should-not (clime-invoke--validate-param opt '(verbose t)))))

(ert-deftest clime-test-invoke/validate-param-dynamic-choices ()
  "Dynamic choice function is resolved and validated."
  (let ((opt (clime-make-option :name 'fmt :flags '("--format")
                                :choices (lambda () '("json" "text")))))
    (should-not (clime-invoke--validate-param opt '(fmt "json")))
    (should (stringp (clime-invoke--validate-param opt '(fmt "xml"))))))

(ert-deftest clime-test-invoke/run-conformer-checks-pass ()
  "Conformers that pass return nil."
  (let* ((conform (lambda (_params _node) nil))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform))))
    (should-not (clime-invoke--run-conformer-checks cmd '()))))

(ert-deftest clime-test-invoke/run-conformer-checks-single-fn ()
  "Single conformer function (not wrapped in list) works."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("single fn error"))))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform conform)))
    (should (equal '(("single fn error"))
                   (clime-invoke--run-conformer-checks cmd '())))))

(ert-deftest clime-test-invoke/run-conformer-checks-fail ()
  "Conformer signaling usage-error returns error strings."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("mutex violated"))))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform))))
    (should (equal '(("mutex violated"))
                   (clime-invoke--run-conformer-checks cmd '())))))

(ert-deftest clime-test-invoke/run-conformer-checks-copies-params ()
  "Conformers receive a copy; original params are not mutated."
  (let* ((original '(foo "bar"))
         (conform (lambda (params _node)
                    (plist-put params 'foo "mutated")))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform))))
    (clime-invoke--run-conformer-checks cmd original)
    (should (equal "bar" (plist-get original 'foo)))))

(ert-deftest clime-test-invoke/validate-all-no-errors ()
  "Clean params produce empty error lists."
  (let* ((opt (clime-make-option :name 'fmt :flags '("--format")
                                 :choices '("json" "text")))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt))))
    (let ((result (clime-invoke--validate-all cmd '(fmt "json"))))
      (should-not (car result))
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/validate-all-param-error ()
  "Invalid param value appears in param-errors alist."
  (let* ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt))))
    (let ((result (clime-invoke--validate-all cmd '(count "abc"))))
      (should (assq 'count (car result)))
      (should (stringp (cdr (assq 'count (car result))))))))

(ert-deftest clime-test-invoke/validate-all-conformer-error ()
  "Conformer error appears in general-errors."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("bad combo"))))
         (cmd (clime-make-command :name "test" :handler #'ignore :conform (list conform))))
    (let ((result (clime-invoke--validate-all cmd '())))
      (should (member "bad combo" (cdr result))))))

(ert-deftest clime-test-invoke/validate-all-requires-error ()
  "Unmet requires constraint appears in general-errors."
  (let* ((opt-a (clime-make-option :name 'key :flags '("--key")))
         (opt-b (clime-make-option :name 'cert :flags '("--cert")
                                   :requires '(key)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt-a opt-b))))
    (let ((result (clime-invoke--validate-all cmd '(cert "foo"))))
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
         (validation '(((count . "invalid integer")) . nil))
         (content (clime-invoke--render-to-string
                   cmd '(count "abc") nil nil nil validation)))
    (should (string-match-p "← invalid integer" content))))

(ert-deftest clime-test-invoke/render-general-error-in-header ()
  "General validation errors appear in the error area."
  (let* ((cmd (clime-make-command :name "test" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (validation '(nil . ("mutex violated")))
         (content (clime-invoke--render-to-string
                   cmd '() nil nil nil validation)))
    (should (string-match-p "mutex violated" content))))

(ert-deftest clime-test-invoke/render-no-validation-no-errors ()
  "Without validation-result, no error markers appear."
  (let* ((opt (clime-make-option :name 'count :flags '("--count" "-c")
                                 :type 'integer))
         (cmd (clime-make-command :name "test" :handler #'ignore :options (list opt)))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (content (clime-invoke--render-to-string
                   cmd '(count "abc") nil nil nil nil)))
    (should-not (string-match-p "←" content))))

(ert-deftest clime-test-invoke/render-combined-errors ()
  "Both user error-msg and general validation errors merge."
  (let* ((cmd (clime-make-command :name "test" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("test" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (validation '(nil . ("req unmet")))
         (content (clime-invoke--render-to-string
                   cmd '() nil "user error" nil validation)))
    (should (string-match-p "user error" content))
    (should (string-match-p "req unmet" content))))

(ert-deftest clime-test-invoke/validate-param-arg-invalid ()
  "Arg with type constraint returns error for invalid value."
  (let ((arg (clime-make-arg :name 'port :type 'integer)))
    (should (stringp (clime-invoke--validate-param arg '(port "abc"))))))

(ert-deftest clime-test-invoke/validate-param-arg-choices ()
  "Arg with choices validates against them."
  (let ((arg (clime-make-arg :name 'env :choices '("dev" "prod"))))
    (should-not (clime-invoke--validate-param arg '(env "dev")))
    (should (stringp (clime-invoke--validate-param arg '(env "staging"))))))

(ert-deftest clime-test-invoke/validate-all-ancestor-options ()
  "Ancestor option errors are included in param-errors."
  (let* ((parent-opt (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
         (cmd (clime-make-command :name "serve" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :options (list parent-opt)
                              :children `(("serve" . ,cmd)))))
    (setf (clime-node-parent cmd) app)
    (let ((result (clime-invoke--validate-all cmd '(port "abc"))))
      (should (assq 'port (car result))))))

(ert-deftest clime-test-invoke/validate-all-empty-node ()
  "Node with no options, args, or conformers returns empty results."
  (let ((cmd (clime-make-command :name "test" :handler #'ignore)))
    (let ((result (clime-invoke--validate-all cmd '())))
      (should-not (car result))
      (should-not (cdr result)))))

(ert-deftest clime-test-invoke/validate-all-multiple-conformer-errors ()
  "Multiple conformers each contribute their own errors."
  (let* ((c1 (lambda (_p _n) (signal 'clime-usage-error '("error one"))))
         (c2 (lambda (_p _n) (signal 'clime-usage-error '("error two"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :conform (list c1 c2))))
    (let ((result (clime-invoke--validate-all cmd '())))
      (should (member "error one" (cdr result)))
      (should (member "error two" (cdr result))))))

(ert-deftest clime-test-invoke/validate-all-mixed-errors ()
  "Both param-errors and general-errors can coexist."
  (let* ((opt (clime-make-option :name 'count :flags '("--count") :type 'integer))
         (conform (lambda (_p _n) (signal 'clime-usage-error '("bad state"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :options (list opt)
                                  :conform (list conform))))
    (let ((result (clime-invoke--validate-all cmd '(count "abc"))))
      (should (assq 'count (car result)))
      (should (member "bad state" (cdr result))))))

;;; ─── Prefix State Rendering ─────────────────────────────────────────

(ert-deftest clime-test-invoke/render-prefix-dash-shows-dash-keys ()
  "With prefix-state \"-\", option keys show as \"-X\"."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil nil "-")))
    (should (string-match-p "-v" content))))

(ert-deftest clime-test-invoke/render-prefix-eq-shows-eq-keys ()
  "With prefix-state \"=\", option keys show as \"=X\"."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil nil "=")))
    (should (string-match-p "=v" content))))

(ert-deftest clime-test-invoke/render-prefix-dims-command-keys ()
  "During prefix state, command keys are dimmed."
  (let* ((child (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,child))))
         (content (clime-invoke--render-to-string app nil nil nil nil nil "-")))
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
         (content (clime-invoke--render-to-string cmd nil nil nil nil nil "-")))
    (let* ((args-pos (string-match "Arguments" content))
           (pos (and args-pos (string-match "\\bf\\b" content (1+ args-pos)))))
      (should pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-prefix-dims-action-keys ()
  "During prefix state, action keys (RET, q) are dimmed."
  (let* ((cmd (clime-make-command :name "run" :handler #'ignore))
         (content (clime-invoke--render-to-string cmd nil nil nil t nil "-")))
    (let ((ret-pos (string-match "RET" content)))
      (should ret-pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property ret-pos 'face content))))
    (let ((q-pos (string-match "\\bq\\b" content)))
      (should q-pos)
      (should (eq 'clime-invoke-dimmed
                  (get-text-property q-pos 'face content))))))

(ert-deftest clime-test-invoke/render-prefix-option-keys-not-dimmed ()
  "During prefix state, option keys keep their option-key face."
  (let* ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0
                                  :help "Be verbose"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :options (list opt)))
         (content (clime-invoke--render-to-string cmd nil nil nil nil nil "-")))
    (let ((pos (string-match "-v" content)))
      (should pos)
      (should (eq 'clime-invoke-option-key
                  (get-text-property pos 'face content))))))

(ert-deftest clime-test-invoke/render-no-prefix-no-dimming ()
  "Without prefix state, no dimming occurs."
  (let* ((child (clime-make-command :name "show" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                               :children `(("show" . ,child))))
         (content (clime-invoke--render-to-string app nil nil nil)))
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
         (content (clime-invoke--render-to-string grp nil nil nil nil nil "-")))
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
                                  :children (list (cons "fmt" grp)))))
    (should (equal '(("inline group error"))
                   (clime-invoke--run-conformer-checks cmd '())))))

(ert-deftest clime-test-invoke/run-conformer-checks-inline-group-pass ()
  "Inline group conformers that pass produce no errors."
  (let* ((conform (lambda (params _node) params))
         (grp (clime-group--create :name "fmt" :inline t :conform (list conform)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :children (list (cons "fmt" grp)))))
    (should-not (clime-invoke--run-conformer-checks cmd '()))))

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
                                  :children (list (cons "output" grp)))))
    ;; Invalid choice should produce a param error
    (let ((result (clime-invoke--validate-all cmd '(fmt "xml"))))
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
          (clime-invoke--render cmd '() key-map nil buf nil nil)
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
                                  :conform (list conform))))
    (let ((result (clime-invoke--validate-all cmd '(a "1" b "2"))))
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
    (let ((result (clime-invoke--validate-all cmd '())))
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
                                  :conform (list conform))))
    (let ((errors (clime-invoke--run-conformer-checks cmd '())))
      (should (= 1 (length errors)))
      (should (equal (caar errors) "bad combo"))
      (should (equal (cdar errors) '(x y))))))

(ert-deftest clime-test-invoke/run-conformer-no-params-returns-nil-cdr ()
  "Conformer without :params returns (MESSAGE . nil)."
  (let* ((conform (lambda (_params _node)
                    (signal 'clime-usage-error '("plain error"))))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                  :conform (list conform))))
    (let ((errors (clime-invoke--run-conformer-checks cmd '())))
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
                                  :children (list (cons "mode" grp)))))
    (let ((result (clime-invoke--validate-all cmd '(a "1" b "2"))))
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
         ;; Simulate validation result: attributed errors go inline only
         (validation '(((a . "mutually exclusive") (b . "mutually exclusive"))))
         (content (clime-invoke--render-to-string
                   cmd '(a "1" b "2") nil nil nil validation)))
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
                                  :children (list (cons "mapping" grp)))))
    (let ((result (clime-invoke--validate-all
                   cmd '(from ("a" "b") to ("x")))))
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
                                 :locked t :default "WAITING"))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :options (list opt)
                                  :locked-vals '((todo . "WAITING"))))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("waiting" . ,cmd))))
         (_ (setf (clime-node-parent cmd) app))
         (key-map (clime-invoke--build-key-map cmd))
         (buf (generate-new-buffer " *test-render*")))
    (unwind-protect
        (progn
          (clime-invoke--render cmd '() key-map nil buf nil nil)
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
          (clime-invoke--render cmd '() key-map nil buf nil nil)
          (let ((content (with-current-buffer buf (buffer-string))))
            (should-not (string-match-p "(locked)" content))))
      (kill-buffer buf))))

(ert-deftest clime-test-invoke/locked-option-mutex-validation ()
  "Setting a mutex sibling of a locked option triggers a validation error."
  (let* ((exclusive (clime-check-exclusive 'query-mode '(todo sexp)))
         (opt-todo (clime-make-option :name 'todo :flags '("--todo")
                                      :locked t :default "WAITING"))
         (opt-sexp (clime-make-option :name 'sexp :flags '("--sexp")))
         (mutex (clime-group--create :name "query-mode" :inline t
                                     :options (list opt-todo opt-sexp)
                                     :conform (list exclusive)))
         (cmd (clime-make-command :name "waiting" :handler #'ignore
                                  :children (list (cons "query-mode" mutex)))))
    ;; User sets sexp — mutex conformer should see locked todo + user sexp
    (let ((result (clime-invoke--validate-all cmd '(sexp "(todo)"))))
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

(provide 'clime-invoke-tests)
;;; clime-invoke-tests.el ends here
