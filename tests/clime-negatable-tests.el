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
(require 'clime-config)
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

;;; ─── Env: negatable bool can be disabled (clime-kqee) ──────────────

;; Sentinel default to distinguish "explicitly nil" from "absent".
(defconst clime-test-neg--absent (make-symbol "absent"))

(defun clime-test-neg--negatable-app (&optional default)
  "Build an app exposing negatable bool `color' via env prefix TEST_NEGENV.
DEFAULT, when given, becomes the option's :default."
  (let* ((opt (apply #'clime-make-option
                     :name 'color :flags '("--color")
                     :negatable t :env t
                     (when default (list :default default))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt))))
    (clime-make-app :name "t" :version "1" :env-prefix "TEST_NEGENV"
                    :children (list (cons "run" cmd)))))

(ert-deftest clime-test-negatable/env-disables-default-t ()
  "Negatable bool :default t + env=0 → param explicitly nil."
  (let* ((app (clime-test-neg--negatable-app t)))
    (let ((process-environment (append '("TEST_NEGENV_COLOR=0")
                                       process-environment)))
      (let ((result (clime-parse app '("run"))))
        (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                    nil))))))

(ert-deftest clime-test-negatable/env-false-word-disables ()
  "Negatable bool :default t + env=false/no → param nil."
  (dolist (falsy '("false" "no" "FALSE"))
    (let* ((app (clime-test-neg--negatable-app t)))
      (let ((process-environment (append (list (concat "TEST_NEGENV_COLOR=" falsy))
                                         process-environment)))
        (let ((result (clime-parse app '("run"))))
          (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                      nil)))))))

(ert-deftest clime-test-negatable/env-enables ()
  "Negatable bool :default t + env=1 → param t."
  (let* ((app (clime-test-neg--negatable-app t)))
    (let ((process-environment (append '("TEST_NEGENV_COLOR=1")
                                       process-environment)))
      (let ((result (clime-parse app '("run"))))
        (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                    t))))))

(ert-deftest clime-test-negatable/env-absent-uses-default ()
  "Negatable bool :default t + no env → param falls back to default t."
  (let* ((app (clime-test-neg--negatable-app t)))
    ;; Ensure the var is genuinely unset in this binding.
    (let ((process-environment (cons "TEST_NEGENV_COLOR" process-environment)))
      (let ((result (clime-parse app '("run"))))
        (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                    t))))))

(ert-deftest clime-test-negatable/env-nonneg-falsy-skipped ()
  "Non-negatable bool :default nil + env=0 → value skipped (param absent)."
  (let* ((opt (clime-make-option :name 'color :flags '("--color")
                                 :nargs 0 :env t)) ; plain bool, default nil
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_NEGENV"
                              :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_NEGENV_COLOR=0")
                                       process-environment)))
      (let ((result (clime-parse app '("run"))))
        ;; Unchanged behaviour: falsy env on a plain bool is dropped,
        ;; leaving the param unset (the nil default never materialises).
        (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                    clime-test-neg--absent))))))

(ert-deftest clime-test-negatable/env-disabled-source-is-env ()
  "Env-disabled negatable bool records provenance `env'."
  (let* ((app (clime-test-neg--negatable-app t)))
    (let ((process-environment (append '("TEST_NEGENV_COLOR=0")
                                       process-environment)))
      (let* ((result (clime-parse app '("run")))
             (values (clime-parse-result-values result)))
        (should (eq (clime-values-source values 'color) 'env))))))

(ert-deftest clime-test-negatable/cli-overrides-disabling-env ()
  "CLI --color overrides an env=0 on a negatable bool (CLI > env)."
  (let* ((app (clime-test-neg--negatable-app t)))
    (let ((process-environment (append '("TEST_NEGENV_COLOR=0")
                                       process-environment)))
      (let ((result (clime-parse app '("run" "--color"))))
        (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                    t))))))

;;; ─── Config: negatable bool can be disabled (clime-kqee) ───────────

(defmacro clime-test-neg--with-config-file (suffix content &rest body)
  "Write CONTENT to a temp file with SUFFIX, bind `cfg-file', run BODY."
  (declare (indent 2))
  `(let ((cfg-file (make-temp-file "clime-test-neg-cfg-" nil ,suffix)))
     (unwind-protect
         (progn (with-temp-file cfg-file (insert ,content))
                ,@body)
       (delete-file cfg-file))))

(defun clime-test-neg--config-app (provider-fn &optional default)
  "Build an app whose negatable bool `color' is fed by PROVIDER-FN.
PROVIDER-FN is a one-arg function of the config file path."
  (let* ((opt (apply #'clime-make-option
                     :name 'color :flags '("--color") :negatable t
                     (when default (list :default default))))
         (cmd (clime-make-command :name "run" :handler #'ignore
                                  :options (list opt))))
    (clime-make-app :name "t" :version "1"
                    :config (lambda (_app _result) (funcall provider-fn))
                    :children (list (cons "run" cmd)))))

(ert-deftest clime-test-negatable/config-json-false-disables ()
  "Negatable bool :default t + JSON \"color\": false → param nil."
  (clime-test-neg--with-config-file ".json" "{\"color\": false}"
    (let* ((file cfg-file)
           (app (clime-test-neg--config-app
                 (lambda () (clime-config-json file)) t))
           (result (clime-parse app '("run"))))
      (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                  nil)))))

(ert-deftest clime-test-negatable/config-sexp-nil-disables ()
  "Negatable bool :default t + sexp (:color nil) → param nil."
  (clime-test-neg--with-config-file ".eld" "(:color nil)"
    (let* ((file cfg-file)
           (app (clime-test-neg--config-app
                 (lambda () (clime-config-sexp file)) t))
           (result (clime-parse app '("run"))))
      (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                  nil)))))

(ert-deftest clime-test-negatable/config-json-true-enables ()
  "Negatable bool :default t + JSON \"color\": true → param t."
  (clime-test-neg--with-config-file ".json" "{\"color\": true}"
    (let* ((file cfg-file)
           (app (clime-test-neg--config-app
                 (lambda () (clime-config-json file)) t))
           (result (clime-parse app '("run"))))
      (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                  t)))))

(ert-deftest clime-test-negatable/config-absent-uses-default ()
  "Negatable bool :default t + config WITHOUT the key → default t.
Guards against false-positive shadowing of the default."
  (clime-test-neg--with-config-file ".json" "{\"other\": 1}"
    (let* ((file cfg-file)
           (app (clime-test-neg--config-app
                 (lambda () (clime-config-json file)) t))
           (result (clime-parse app '("run"))))
      (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                  t)))))

(ert-deftest clime-test-negatable/config-nonneg-false-dropped ()
  "Non-negatable bool + JSON false → value dropped (param absent)."
  (clime-test-neg--with-config-file ".json" "{\"color\": false}"
    (let* ((file cfg-file)
           (opt (clime-make-option :name 'color :flags '("--color")
                                   :nargs 0)) ; plain bool
           (cmd (clime-make-command :name "run" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_a _r) (clime-config-json file))
                                :children (list (cons "run" cmd))))
           (result (clime-parse app '("run"))))
      (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                  clime-test-neg--absent)))))

(ert-deftest clime-test-negatable/config-disabled-source-is-config ()
  "Config-disabled negatable bool records provenance `config'."
  (clime-test-neg--with-config-file ".json" "{\"color\": false}"
    (let* ((file cfg-file)
           (app (clime-test-neg--config-app
                 (lambda () (clime-config-json file)) t))
           (result (clime-parse app '("run")))
           (values (clime-parse-result-values result)))
      (should (eq (clime-values-source values 'color) 'config)))))

(ert-deftest clime-test-negatable/cli-overrides-disabling-config ()
  "CLI --color overrides JSON \"color\": false (CLI > config)."
  (clime-test-neg--with-config-file ".json" "{\"color\": false}"
    (let* ((file cfg-file)
           (app (clime-test-neg--config-app
                 (lambda () (clime-config-json file)) t))
           (result (clime-parse app '("run" "--color"))))
      (should (eq (clime-parse-result-param result 'color clime-test-neg--absent)
                  t)))))

(provide 'clime-negatable-tests)
;;; clime-negatable-tests.el ends here
