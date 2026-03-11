;;; clime-mutex-tests.el --- Tests for mutual exclusion option groups  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for :mutex slot on options.  Covers struct slots, parser
;; validation (CLI + env), defaults, ancestor options, and DSL.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Struct Slots ───────────────────────────────────────────────────

(ert-deftest clime-test-mutex/slot-nil-by-default ()
  "Options have :mutex nil by default."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo"))))
    (should-not (clime-option-mutex opt))))

(ert-deftest clime-test-mutex/slot-accepts-symbol ()
  ":mutex accepts a symbol."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :mutex 'output-format)))
    (should (eq (clime-option-mutex opt) 'output-format))))

;;; ─── Single Option OK ──────────────────────────────────────────────

(ert-deftest clime-test-mutex/single-option-no-error ()
  "Using one option from a mutex group succeeds."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--json")))))
    (should (equal output "ok"))))

;;; ─── Conflict Detection (CLI) ──────────────────────────────────────

(ert-deftest clime-test-mutex/two-options-error ()
  "Using two options from the same mutex group signals usage error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-mutex/three-options-error ()
  "Using three options from the same mutex group signals usage error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (opt-text (clime-make-option :name 'text :flags '("--text") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv opt-text)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv" "--text"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-mutex/error-message-lists-flags ()
  "Error message includes the conflicting flag names."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (err (should-error
               (clime-parse app '("run" "--json" "--csv"))
               :type 'clime-usage-error)))
    (should (string-match-p "--json" (cadr err)))
    (should (string-match-p "--csv" (cadr err)))
    (should (string-match-p "mutually exclusive" (cadr err)))))

(ert-deftest clime-test-mutex/clime-run-returns-exit-2 ()
  "Mutex violation through clime-run returns exit code 2."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run" "--json" "--csv")))))
    (should (= exit-code 2))))

;;; ─── Different Groups Independent ──────────────────────────────────

(ert-deftest clime-test-mutex/different-groups-independent ()
  "Options in different mutex groups don't conflict."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (opt-verbose (clime-make-option :name 'verbose :flags '("--verbose") :nargs 0
                                          :mutex 'verbosity))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv opt-verbose)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--json" "--verbose")))))
    (should (equal output "ok"))))

;;; ─── Value Options (nargs > 0) ──────────────────────────────────────

(ert-deftest clime-test-mutex/value-options-conflict ()
  "Mutex works with value-taking options."
  (let* ((opt-file (clime-make-option :name 'outfile :flags '("--output-file")
                                       :mutex 'output))
         (opt-dir (clime-make-option :name 'outdir :flags '("--output-dir")
                                      :mutex 'output))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-file opt-dir)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--output-file" "a.txt" "--output-dir" "/tmp"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-mutex/value-default-suppressed ()
  "Default on value mutex option is suppressed when sibling is set."
  (let* ((opt-file (clime-make-option :name 'outfile :flags '("--output-file")
                                       :mutex 'output))
         (opt-dir (clime-make-option :name 'outdir :flags '("--output-dir")
                                      :mutex 'output :default "/tmp"))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (plist-get (clime-context-params ctx) 'outdir)))
                                  :options (list opt-file opt-dir)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--output-file" "a.txt")))))
    (should (equal output "nil"))))

;;; ─── No Mutex Unaffected ───────────────────────────────────────────

(ert-deftest clime-test-mutex/no-mutex-unaffected ()
  "Options without :mutex can be used together freely."
  (let* ((opt-a (clime-make-option :name 'a :flags '("--aa") :nargs 0))
         (opt-b (clime-make-option :name 'b :flags '("--bb") :nargs 0))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-a opt-b)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--aa" "--bb")))))
    (should (equal output "ok"))))

;;; ─── Defaults Don't Trigger ────────────────────────────────────────

(ert-deftest clime-test-mutex/default-suppressed-when-sibling-set ()
  "Setting one mutex option suppresses defaults on siblings."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt :default t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (let ((p (clime-context-params ctx)))
                                               (format "json=%s csv=%s"
                                                       (plist-get p 'json)
                                                       (plist-get p 'csv))))
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--json")))))
    (should (equal output "json=t csv=nil"))))

(ert-deftest clime-test-mutex/default-applies-when-no-sibling-set ()
  "Default applies normally when no mutex sibling is set."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt :default t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (let ((p (clime-context-params ctx)))
                                               (format "json=%s csv=%s"
                                                       (plist-get p 'json)
                                                       (plist-get p 'csv))))
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "json=nil csv=t"))))

;;; ─── Env Var Conflict ──────────────────────────────────────────────

(ert-deftest clime-test-mutex/env-var-conflict ()
  "Mutex violation via env vars signals usage error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_MX"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_MX_JSON=1" "TEST_MX_CSV=1")
                                       process-environment)))
      (should-error
       (clime-parse app '("run"))
       :type 'clime-usage-error))))

(ert-deftest clime-test-mutex/cli-plus-env-conflict ()
  "Mutex violation from CLI + env var combination signals usage error."
  (let* ((opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :mutex 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_MX"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_MX_CSV=1")
                                       process-environment)))
      (should-error
       (clime-parse app '("run" "--json"))
       :type 'clime-usage-error))))

;;; ─── Ancestor Options ──────────────────────────────────────────────

(ert-deftest clime-test-mutex/ancestor-option-conflict ()
  "Mutex works across app-level and command-level options."
  (let* ((app-opt (clime-make-option :name 'json :flags '("--json") :nargs 0
                                      :mutex 'fmt))
         (cmd-opt (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :mutex 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                               :options (list app-opt)
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)))

;;; ─── DSL Integration ───────────────────────────────────────────────

(ert-deftest clime-test-mutex/dsl-option ()
  ":mutex on option in DSL sets the slot."
  (eval
   '(clime-app clime-test--mutex-dsl-app
      :version "1"
      (clime-option fmt-json ("--json") :flag t :mutex 'output-format)
      (clime-option fmt-csv ("--csv") :flag t :mutex 'output-format)
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--mutex-dsl-app))
         (opt (clime-node-find-option app "--json")))
    (should (eq (clime-option-mutex opt) 'output-format))))

(provide 'clime-mutex-tests)
;;; clime-mutex-tests.el ends here
