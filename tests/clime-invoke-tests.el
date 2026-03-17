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

;;; ─── Format Value ────────────────────────────────────────────────────

(ert-deftest clime-test-invoke/format-value-boolean ()
  "Boolean shows on/off."
  (let ((opt (clime-make-option :name 'verbose :flags '("-v") :nargs 0)))
    (should (string-match-p "on" (clime-invoke--format-value opt '(verbose t))))
    (should (string-match-p "off" (clime-invoke--format-value opt nil)))))

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
  "Choices show selected value or unset."
  (let ((opt (clime-make-option :name 'format :flags '("--format")
                                 :choices '("json" "text"))))
    (should (string-match-p "json" (clime-invoke--format-value opt '(format "json"))))
    (should (string-match-p "unset" (clime-invoke--format-value opt nil)))))

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

(ert-deftest clime-test-invoke/render-no-required-when-set ()
  "Required marker hidden when arg has a value."
  (let* ((arg (clime-make-arg :name 'file :help "File path"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                   :args (list arg)))
         (content (clime-invoke--render-to-string cmd '(file "test.txt") nil nil)))
    (should-not (string-match-p "(required)" content))))

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

(provide 'clime-invoke-tests)
;;; clime-invoke-tests.el ends here
