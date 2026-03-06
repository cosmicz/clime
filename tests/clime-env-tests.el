;;; clime-env-tests.el --- Tests for env var provider  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for env var fallback: explicit :env, auto-derived from
;; :env-prefix, precedence, type coercion, boolean parsing,
;; multiple option splitting, and edge cases.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)

;;; ─── Helper ────────────────────────────────────────────────────────────

(defmacro clime-test-with-env (bindings &rest body)
  "Execute BODY with environment variables set from BINDINGS.
BINDINGS is a list of (VAR VALUE) pairs.  Vars are unset after BODY."
  (declare (indent 1))
  (let ((saved (gensym "saved")))
    `(let ((,saved (mapcar (lambda (b) (cons (car b) (getenv (car b))))
                           ',bindings)))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (b)
                         `(setenv ,(car b) ,(cadr b)))
                       bindings)
             ,@body)
         (dolist (pair ,saved)
           (setenv (car pair) (cdr pair)))))))

;;; ─── Explicit :env ─────────────────────────────────────────────────────

(ert-deftest clime-test-env/explicit-env-var ()
  "Option with explicit :env reads from that env var."
  (clime-test-with-env (("MY_CUSTOM_VAR" "hello"))
    (let* ((opt (clime-make-option :name 'greeting :flags '("--greeting")
                                   :env "MY_CUSTOM_VAR"))
           (cmd (clime-make-command :name "greet" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "greet" cmd))))
           (result (clime-parse app '("greet"))))
      (should (equal (plist-get (clime-parse-result-params result) 'greeting)
                     "hello")))))

(ert-deftest clime-test-env/cli-overrides-env ()
  "CLI value takes precedence over env var."
  (clime-test-with-env (("MY_VAR" "from-env"))
    (let* ((opt (clime-make-option :name 'val :flags '("--val")
                                   :env "MY_VAR"))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "--val" "from-cli"))))
      (should (equal (plist-get (clime-parse-result-params result) 'val)
                     "from-cli")))))

(ert-deftest clime-test-env/env-overrides-default ()
  "Env var takes precedence over declared default."
  (clime-test-with-env (("MY_VAR" "from-env"))
    (let* ((opt (clime-make-option :name 'val :flags '("--val")
                                   :env "MY_VAR" :default "from-default"))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'val)
                     "from-env")))))

;;; ─── Auto-derived from :env-prefix ─────────────────────────────────────

(ert-deftest clime-test-env/auto-derive-from-prefix ()
  "Option without :env auto-derives var from app :env-prefix."
  (clime-test-with-env (("MYAPP_REGISTRY" "custom-reg"))
    (let* ((opt (clime-make-option :name 'registry :flags '("--registry")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'registry)
                     "custom-reg")))))

(ert-deftest clime-test-env/auto-derive-hyphen-to-underscore ()
  "Hyphenated option names convert to underscores in env var."
  (clime-test-with-env (("MYAPP_DRY_RUN" "true"))
    (let* ((opt (clime-make-option :name 'dry-run :flags '("--dry-run")
                                   :nargs 0))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (eq (plist-get (clime-parse-result-params result) 'dry-run) t)))))

(ert-deftest clime-test-env/explicit-env-overrides-prefix ()
  "Explicit :env takes priority over auto-derivation."
  (clime-test-with-env (("CUSTOM_VAR" "custom")
                        ("MYAPP_VAL" "derived"))
    (let* ((opt (clime-make-option :name 'val :flags '("--val")
                                   :env "CUSTOM_VAR"))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'val)
                     "custom")))))

;;; ─── No env lookup when neither :env nor :env-prefix ───────────────────

(ert-deftest clime-test-env/no-env-no-prefix-no-lookup ()
  "Without :env or :env-prefix, no env var lookup occurs."
  (clime-test-with-env (("T_VAL" "surprise"))
    (let* ((opt (clime-make-option :name 'val :flags '("--val")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should-not (plist-member (clime-parse-result-params result) 'val)))))

;;; ─── Type coercion ─────────────────────────────────────────────────────

(ert-deftest clime-test-env/integer-coercion ()
  "Env var value is coerced to integer when option has :type integer."
  (clime-test-with-env (("MYAPP_PORT" "8080"))
    (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

(ert-deftest clime-test-env/invalid-integer-signals-error ()
  "Non-numeric env var signals usage error for integer option."
  (clime-test-with-env (("MYAPP_PORT" "abc"))
    (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd)))))
      (should-error (clime-parse app '("cmd"))
                    :type 'clime-usage-error))))

;;; ─── Boolean env vars ──────────────────────────────────────────────────

(ert-deftest clime-test-env/boolean-truthy-values ()
  "Boolean option recognizes truthy env values."
  (dolist (val '("1" "true" "TRUE" "yes" "YES" "True" "Yes"))
    (clime-test-with-env (("MYAPP_FORCE" nil))
      (setenv "MYAPP_FORCE" val)
      (let* ((opt (clime-make-option :name 'force :flags '("--force") :nargs 0))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                  :children (list (cons "cmd" cmd))))
             (result (clime-parse app '("cmd"))))
        (should (eq (plist-get (clime-parse-result-params result) 'force) t))))))

(ert-deftest clime-test-env/boolean-falsy-values ()
  "Boolean option recognizes falsy env values (treated as unset)."
  (dolist (val '("0" "false" "FALSE" "no" "NO" "False" "No"))
    (clime-test-with-env (("MYAPP_FORCE" nil))
      (setenv "MYAPP_FORCE" val)
      (let* ((opt (clime-make-option :name 'force :flags '("--force") :nargs 0))
             (cmd (clime-make-command :name "cmd" :handler #'ignore
                                      :options (list opt)))
             (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                  :children (list (cons "cmd" cmd))))
             (result (clime-parse app '("cmd"))))
        (should-not (plist-member (clime-parse-result-params result) 'force))))))

(ert-deftest clime-test-env/boolean-invalid-signals-error ()
  "Invalid boolean env var value signals usage error."
  (clime-test-with-env (("MYAPP_FORCE" "maybe"))
    (let* ((opt (clime-make-option :name 'force :flags '("--force") :nargs 0))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd)))))
      (should-error (clime-parse app '("cmd"))
                    :type 'clime-usage-error))))

;;; ─── Multiple option from env ──────────────────────────────────────────

(ert-deftest clime-test-env/multiple-option-comma-split ()
  "Multiple option splits env var value on commas."
  (clime-test-with-env (("MYAPP_TAG" "dev,prod,test"))
    (let* ((opt (clime-make-option :name 'tag :flags '("--tag")
                                   :multiple t))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'tag)
                     '("dev" "prod" "test"))))))

;;; ─── Edge cases ────────────────────────────────────────────────────────

(ert-deftest clime-test-env/empty-env-var-ignored ()
  "Empty string env var is treated as unset."
  (clime-test-with-env (("MYAPP_VAL" ""))
    (let* ((opt (clime-make-option :name 'val :flags '("--val")
                                   :default "fallback"))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'val)
                     "fallback")))))

(ert-deftest clime-test-env/root-option-env ()
  "Root-level options also get env var resolution."
  (clime-test-with-env (("MYAPP_VERBOSE" "3"))
    (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")
                                        :type 'integer))
           (cmd (clime-make-command :name "cmd" :handler #'ignore))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :options (list root-opt)
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'verbose)
                     3)))))

(ert-deftest clime-test-env/count-option-env ()
  "Count option from env var sets the count value."
  (clime-test-with-env (("MYAPP_VERBOSE" "3"))
    (let* ((opt (clime-make-option :name 'verbose :flags '("-v" "--verbose")
                                   :count t))
           (cmd (clime-make-command :name "cmd" :handler #'ignore))
           (app (clime-make-app :name "t" :version "1" :env-prefix "MYAPP"
                                :options (list opt)
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'verbose)
                     3)))))

(provide 'clime-env-tests)
;;; clime-env-tests.el ends here
