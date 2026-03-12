;;; clime-zip-tests.el --- Tests for :zip paired option groups  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for :zip slot on options.  Covers struct slots, cardinality
;; validation, zipped alist in ctx, env vars, and DSL.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Struct Slots ───────────────────────────────────────────────────

(ert-deftest clime-test-zip/slot-nil-by-default ()
  "Options have :zip nil by default."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo"))))
    (should-not (clime-option-zip opt))))

(ert-deftest clime-test-zip/slot-accepts-symbol ()
  ":zip accepts a symbol."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :zip 'grp)))
    (should (eq (clime-option-zip opt) 'grp))))

(ert-deftest clime-test-zip/implies-multiple ()
  ":zip implies :multiple t."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :zip 'grp)))
    (should (clime-option-multiple opt))))

;;; ─── Basic Zipping ──────────────────────────────────────────────────

(ert-deftest clime-test-zip/two-options-zipped ()
  "Two zip members produce zipped alist in ctx."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'sr)))
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "SPEC" "--reason" "no spec"
                                    "--skip" "TEST" "--reason" "no test")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 2))
      (should (equal (alist-get 'skip (nth 0 result)) "SPEC"))
      (should (equal (alist-get 'reason (nth 0 result)) "no spec"))
      (should (equal (alist-get 'skip (nth 1 result)) "TEST"))
      (should (equal (alist-get 'reason (nth 1 result)) "no test")))))

(ert-deftest clime-test-zip/individual-lists-preserved ()
  "Individual option lists still available in ctx."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S %S"
                                                     (clime-ctx-get ctx 'skip)
                                                     (clime-ctx-get ctx 'reason)))
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "SPEC" "--reason" "no spec")))))
    (should (equal output "(\"SPEC\") (\"no spec\")"))))

;;; ─── All Absent OK ──────────────────────────────────────────────────

(ert-deftest clime-test-zip/all-absent-ok ()
  "Using neither zip option succeeds."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "ok"))))

;;; ─── Cardinality Mismatch ───────────────────────────────────────────

(ert-deftest clime-test-zip/cardinality-mismatch-error ()
  "Unequal counts signal usage error."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "SPEC" "--skip" "TEST" "--reason" "only one"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-zip/cardinality-error-message ()
  "Error message includes option names and counts."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (err (should-error
               (clime-parse app '("run" "--skip" "A" "--skip" "B" "--reason" "one"))
               :type 'clime-usage-error)))
    (should (string-match-p "--skip" (cadr err)))
    (should (string-match-p "--reason" (cadr err)))))

;;; ─── Partial Presence ───────────────────────────────────────────────

(ert-deftest clime-test-zip/partial-presence-error ()
  "Using one zip member without the other signals error."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "SPEC"))
     :type 'clime-usage-error)))

;;; ─── Three Options ──────────────────────────────────────────────────

(ert-deftest clime-test-zip/three-options ()
  "Zip works with 3+ options."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--aa") :zip 'trio))
         (opt-b (clime-make-option :name 'b :flags '("--bb") :zip 'trio))
         (opt-c (clime-make-option :name 'c :flags '("--cc") :zip 'trio))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'trio)))
                                  :options (list opt-a opt-b opt-c)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--aa" "1" "--bb" "x" "--cc" "p"
                                    "--aa" "2" "--bb" "y" "--cc" "q")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 2))
      (should (equal (alist-get 'a (nth 0 result)) "1"))
      (should (equal (alist-get 'b (nth 0 result)) "x"))
      (should (equal (alist-get 'c (nth 0 result)) "p")))))

;;; ─── Env Var ────────────────────────────────────────────────────────

(ert-deftest clime-test-zip/env-var-participates ()
  "Env vars contribute to zip groups."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'sr)))
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_ZIP"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_ZIP_REASON=from-env")
                                       process-environment)))
      (let ((output (with-output-to-string
                      (clime-run app '("run" "--skip" "SPEC")))))
        (let ((result (car (read-from-string output))))
          (should (= (length result) 1))
          (should (equal (alist-get 'skip (nth 0 result)) "SPEC"))
          (should (equal (alist-get 'reason (nth 0 result)) "from-env")))))))

;;; ─── Exit Code ──────────────────────────────────────────────────────

(ert-deftest clime-test-zip/clime-run-returns-exit-2 ()
  "Zip violation through clime-run returns exit code 2."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--skip" "A" "--skip" "B" "--reason" "one")))))
    (should (= exit-code 2))))

;;; ─── Ancestor Options ───────────────────────────────────────────────

(ert-deftest clime-test-zip/ancestor-option ()
  ":zip works across app-level and command-level options."
  (let* ((app-opt (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (cmd-opt (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'sr)))
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                               :options (list app-opt)
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "SPEC" "--reason" "ok")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 1))
      (should (equal (alist-get 'skip (nth 0 result)) "SPEC")))))

;;; ─── DSL Integration ────────────────────────────────────────────────

(ert-deftest clime-test-zip/dsl-option ()
  ":zip on option in DSL sets the slot."
  (eval
   '(clime-app clime-test--zip-dsl-app
      :version "1"
      (clime-option skip ("--skip") :zip 'skip-reason)
      (clime-option reason ("--reason") :zip 'skip-reason)
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--zip-dsl-app))
         (opt (clime-node-find-option app "--skip")))
    (should (eq (clime-option-zip opt) 'skip-reason))
    (should (clime-option-multiple opt))))

(provide 'clime-zip-tests)
;;; clime-zip-tests.el ends here
