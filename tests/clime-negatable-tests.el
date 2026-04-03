;;; clime-negatable-tests.el --- Tests for negatable boolean flags  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for :negatable slot on options.  Covers struct, parser,
;; help rendering, env override, mutex interaction, and DSL.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-help)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Struct Slots ───────────────────────────────────────────────────

(ert-deftest clime-test-negatable/slot-nil-by-default ()
  "Options have :negatable nil by default."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo"))))
    (should-not (clime-option-negatable opt))))

(ert-deftest clime-test-negatable/slot-accepts-t ()
  ":negatable accepts t."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :negatable t)))
    (should (eq (clime-option-negatable opt) t))))

(ert-deftest clime-test-negatable/implies-boolean ()
  ":negatable t implies boolean (nargs 0)."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :negatable t)))
    (should (clime-option-boolean-p opt))))

;;; ─── Construction Validation ───────────────────────────────────────

(ert-deftest clime-test-negatable/error-with-nargs ()
  ":negatable t with :nargs > 0 signals error."
  (should-error
   (clime-make-option :name 'foo :flags '("--foo") :negatable t :nargs 1)))

(ert-deftest clime-test-negatable/error-with-count ()
  ":negatable t with :count t signals error."
  (should-error
   (clime-make-option :name 'foo :flags '("--foo") :negatable t :count t)))

;;; ─── Parser: Positive Flag ────────────────────────────────────────

(ert-deftest clime-test-negatable/positive-flag-sets-t ()
  "Using --color on a negatable option sets value to t."
  (let* ((opt (clime-make-option :name 'color :flags '("--color") :negatable t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (plist-get (clime-context-params ctx) 'color)))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--color")))))
    (should (equal output "t\n"))))

;;; ─── Parser: Negated Flag ─────────────────────────────────────────

(ert-deftest clime-test-negatable/no-flag-sets-nil ()
  "Using --no-color on a negatable option explicitly sets value to nil."
  (let* ((opt (clime-make-option :name 'color :flags '("--color") :negatable t
                                  :default t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (let ((p (clime-context-params ctx)))
                                               (if (plist-member p 'color)
                                                   (format "set:%s" (plist-get p 'color))
                                                 "unset")))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--no-color")))))
    (should (equal output "set:nil\n"))))

(ert-deftest clime-test-negatable/no-flag-overrides-default ()
  "--no-X overrides :default t."
  (let* ((opt (clime-make-option :name 'color :flags '("--color") :negatable t
                                  :default t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (plist-get (clime-context-params ctx) 'color)))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--no-color")))))
    (should (equal output "nil\n"))))

;;; ─── Ternary State ─────────────────────────────────────────────────

(ert-deftest clime-test-negatable/ternary-states ()
  "Handler can distinguish all three states via clime-param with default."
  (let* ((opt (clime-make-option :name 'color :flags '("--color") :negatable t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (pcase (clime-param ctx 'color 'auto)
                                               ('auto "auto")
                                               ('t "on")
                                               (_ "off")))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; No flag → unset → default 'auto → "auto"
    (should (equal (with-output-to-string (clime-run app '("run")))
                   "auto\n"))
    ;; --color → t → "on"
    (should (equal (with-output-to-string (clime-run app '("run" "--color")))
                   "on\n"))
    ;; --no-color → explicit nil → "off"
    (should (equal (with-output-to-string (clime-run app '("run" "--no-color")))
                   "off\n"))))

;;; ─── Env Var Override ──────────────────────────────────────────────

(ert-deftest clime-test-negatable/no-flag-overrides-env ()
  "--no-X on CLI overrides truthy env var."
  (let* ((opt (clime-make-option :name 'color :flags '("--color") :negatable t :env t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (plist-get (clime-context-params ctx) 'color)))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_NEG"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_NEG_COLOR=1")
                                       process-environment)))
      (let ((output (with-output-to-string
                      (clime-run app '("run" "--no-color")))))
        (should (equal output "nil\n"))))))

;;; ─── Short Flags Not Negated ───────────────────────────────────────

(ert-deftest clime-test-negatable/short-flag-no-negation ()
  "Short flags don't get --no- variants."
  (let* ((opt (clime-make-option :name 'color :flags '("-c" "--color") :negatable t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    ;; -c works as positive
    (with-output-to-string
      (setq exit-code (clime-run app '("run" "-c"))))
    (should (= exit-code 0))
    ;; --no-c is not recognized (would be unknown option → exit 2)
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--no-c")))))
    (should (= exit-code 2))))

;;; ─── No Double Negation ────────────────────────────────────────────

(ert-deftest clime-test-negatable/no-double-negation ()
  "Flags starting with --no- don't get --no-no- variants."
  (let* ((opt (clime-make-option :name 'no-cache :flags '("--no-cache") :negatable t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    ;; --no-cache works
    (with-output-to-string
      (setq exit-code (clime-run app '("run" "--no-cache"))))
    (should (= exit-code 0))
    ;; --no-no-cache is not recognized
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--no-no-cache")))))
    (should (= exit-code 2))))

;;; ─── Help Rendering ────────────────────────────────────────────────

(ert-deftest clime-test-negatable/help-shows-no-variant ()
  "Help displays --color / --no-color for negatable options."
  (let* ((opt (clime-make-option :name 'color :flags '("--color") :negatable t
                                  :help "Colorize output"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "run"))))
    (should (string-match-p "--color / --no-color" help))))

(ert-deftest clime-test-negatable/help-with-short-flag ()
  "Help displays -c, --color / --no-color for negatable with short flag."
  (let* ((opt (clime-make-option :name 'color :flags '("-c" "--color") :negatable t
                                  :help "Colorize output"))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (help (clime-format-help cmd '("app" "run"))))
    (should (string-match-p "-c, --color / --no-color" help))))

;;; ─── Mutex Interaction ─────────────────────────────────────────────

(ert-deftest clime-test-negatable/exclusive-negated-counts-as-set ()
  "Negated flag counts as set for exclusive group check."
  (let* ((opt-color (clime-make-option :name 'color :flags '("--color") :negatable t))
         (opt-plain (clime-make-option :name 'plain :flags '("--plain") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-color opt-plain)
                                  :conform (clime-check-exclusive 'display '(color plain))))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--no-color" "--plain"))
     :type 'clime-usage-error)))

;;; ─── Multiple Long Flags ────────────────────────────────────────────

(ert-deftest clime-test-negatable/multiple-flags-all-negatable ()
  "All long flags get --no- variants when option has multiple flags."
  (let* ((opt (clime-make-option :name 'color :flags '("--color" "--colour") :negatable t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-param ctx 'color 'auto)))
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; Both positive forms work
    (should (equal (with-output-to-string (clime-run app '("run" "--color"))) "t\n"))
    (should (equal (with-output-to-string (clime-run app '("run" "--colour"))) "t\n"))
    ;; Both negated forms work
    (should (equal (with-output-to-string (clime-run app '("run" "--no-color"))) "nil\n"))
    (should (equal (with-output-to-string (clime-run app '("run" "--no-colour"))) "nil\n"))))

;;; ─── DSL Integration ───────────────────────────────────────────────

(ert-deftest clime-test-negatable/dsl-option ()
  ":negatable on option in DSL sets the slot."
  (eval
   '(clime-app clime-test--neg-dsl-app
      :version "1"
      (clime-option color ("--color") :negatable t :help "Colorize")
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--neg-dsl-app))
         (opt (clime-node-find-option app "--color")))
    (should (eq (clime-option-negatable opt) t))
    (should (clime-option-boolean-p opt))))

(provide 'clime-negatable-tests)
;;; clime-negatable-tests.el ends here
