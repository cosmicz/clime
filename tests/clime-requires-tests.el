;;; clime-requires-tests.el --- Tests for :requires option dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for :requires slot on options.  Covers struct slots, parser
;; validation (CLI + env), defaults, ancestor options, and DSL.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Struct Slots ───────────────────────────────────────────────────

(ert-deftest clime-test-requires/slot-nil-by-default ()
  "Options have :requires nil by default."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo"))))
    (should-not (clime-option-requires opt))))

(ert-deftest clime-test-requires/slot-accepts-list ()
  ":requires accepts a list of symbols."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :requires '(bar baz))))
    (should (equal (clime-option-requires opt) '(bar baz)))))

;;; ─── Both Present OK ────────────────────────────────────────────────

(ert-deftest clime-test-requires/both-present-ok ()
  "Using both coupled options together succeeds."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :requires '(skip)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "SPEC" "--reason" "no spec")))))
    (should (equal output "ok"))))

;;; ─── Neither Present OK ─────────────────────────────────────────────

(ert-deftest clime-test-requires/neither-present-ok ()
  "Using neither coupled option succeeds."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :requires '(skip)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "ok"))))

;;; ─── Missing Required Option ────────────────────────────────────────

(ert-deftest clime-test-requires/missing-required-signals-error ()
  "Using --skip without --reason signals usage error."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :requires '(skip)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "SPEC"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-requires/reverse-missing-signals-error ()
  "Using --reason without --skip signals usage error."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :requires '(skip)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--reason" "because"))
     :type 'clime-usage-error)))

;;; ─── Error Message ──────────────────────────────────────────────────

(ert-deftest clime-test-requires/error-message-names-options ()
  "Error message includes the requiring and missing option flags."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :requires '(skip)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (err (should-error
               (clime-parse app '("run" "--skip" "SPEC"))
               :type 'clime-usage-error)))
    (should (string-match-p "--skip" (cadr err)))
    (should (string-match-p "--reason" (cadr err)))))

;;; ─── One-Way Dependency ─────────────────────────────────────────────

(ert-deftest clime-test-requires/one-way-a-requires-b ()
  "A requires B, but B alone is fine."
  (let* ((opt-file (clime-make-option :name 'outfile :flags '("--output-file")
                                       :requires '(format)))
         (opt-fmt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-file opt-fmt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; B alone is fine
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--format" "json")))))
      (should (equal output "ok")))
    ;; A without B errors
    (should-error
     (clime-parse app '("run" "--output-file" "a.txt"))
     :type 'clime-usage-error)
    ;; Both together is fine
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--output-file" "a.txt" "--format" "json")))))
      (should (equal output "ok")))))

;;; ─── Multi-Dependency ───────────────────────────────────────────────

(ert-deftest clime-test-requires/multi-dep ()
  ":requires '(b c) — needs both b and c."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--aa") :nargs 0
                                    :requires '(b c)))
         (opt-b (clime-make-option :name 'b :flags '("--bb") :nargs 0))
         (opt-c (clime-make-option :name 'c :flags '("--cc") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-a opt-b opt-c)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; A with only B errors (missing C)
    (should-error
     (clime-parse app '("run" "--aa" "--bb"))
     :type 'clime-usage-error)
    ;; A with both B and C succeeds
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--aa" "--bb" "--cc")))))
      (should (equal output "ok")))))

;;; ─── Env Var Satisfies ──────────────────────────────────────────────

(ert-deftest clime-test-requires/env-var-satisfies ()
  "Env-var-set values satisfy the requirement."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :requires '(skip)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_REQ"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_REQ_REASON=because")
                                       process-environment)))
      (let ((output (with-output-to-string
                      (clime-run app '("run" "--skip" "SPEC")))))
        (should (equal output "ok"))))))

;;; ─── Defaults Don't Satisfy ─────────────────────────────────────────

(ert-deftest clime-test-requires/default-does-not-satisfy ()
  "Default values do NOT satisfy :requires."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :default "default-reason"))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "SPEC"))
     :type 'clime-usage-error)))

;;; ─── Ancestor Options ───────────────────────────────────────────────

(ert-deftest clime-test-requires/ancestor-option ()
  ":requires works across app-level and command-level options."
  (let* ((app-opt (clime-make-option :name 'auth :flags '("--auth")))
         (cmd-opt (clime-make-option :name 'admin :flags '("--admin") :nargs 0
                                      :requires '(auth)))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                               :options (list app-opt)
                               :children (list (cons "run" cmd)))))
    ;; --admin without --auth errors
    (should-error
     (clime-parse app '("run" "--admin"))
     :type 'clime-usage-error)
    ;; --admin with --auth succeeds
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--admin" "--auth" "token")))))
      (should (equal output "ok")))))

;;; ─── clime-run Exit Code ────────────────────────────────────────────

(ert-deftest clime-test-requires/clime-run-returns-exit-2 ()
  "Requires violation through clime-run returns exit code 2."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :requires '(reason)))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--skip" "SPEC")))))
    (should (= exit-code 2))))

;;; ─── Negatable Counts as Set ────────────────────────────────────────

(ert-deftest clime-test-requires/negatable-counts-as-set ()
  "Negated flags (--no-X) count as set for :requires."
  (let* ((opt-color (clime-make-option :name 'color :flags '("--color")
                                        :negatable t :requires '(format)))
         (opt-fmt (clime-make-option :name 'format :flags '("--format")))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-color opt-fmt)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; --no-color still requires --format
    (should-error
     (clime-parse app '("run" "--no-color"))
     :type 'clime-usage-error)
    ;; --no-color with --format is fine
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--no-color" "--format" "csv")))))
      (should (equal output "ok")))))

;;; ─── DSL Integration ────────────────────────────────────────────────

(ert-deftest clime-test-requires/dsl-option ()
  ":requires on option in DSL sets the slot."
  (eval
   '(clime-app clime-test--requires-dsl-app
      :version "1"
      (clime-option skip ("--skip") :requires '(reason))
      (clime-option reason ("--reason") :requires '(skip))
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--requires-dsl-app))
         (opt (clime-node-find-option app "--skip")))
    (should (equal (clime-option-requires opt) '(reason)))))

(provide 'clime-requires-tests)
;;; clime-requires-tests.el ends here
