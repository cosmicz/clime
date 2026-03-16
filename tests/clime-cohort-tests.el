;;; clime-cohort-tests.el --- Tests for cohorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for clime-cohort: the generalized construct that unifies
;; :mutex (exclusive) and :zip (paired) semantics.  Covers struct
;; slots, check functions, DSL integration, derived values, required
;; cohorts, and deprecation of :mutex/:zip slots.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── Base Struct (clime-param) ────────────────────────────────────

(ert-deftest clime-test-cohort/param-not-constructible ()
  "clime-param has no public constructor (abstract base)."
  (should-not (fboundp 'clime-make-param))
  (should-not (fboundp 'clime-param--create)))

(ert-deftest clime-test-cohort/param-accessors-on-option ()
  "clime-param-name and clime-param-required work on clime-option."
  (let ((opt (clime-make-option :name 'verbose :flags '("--verbose")
                                 :required t :help "Be loud")))
    (should (eq (clime-param-name opt) 'verbose))
    (should (eq (clime-param-required opt) t))
    (should (equal (clime-param-help opt) "Be loud"))
    ;; Original accessors still work (cl-defstruct inheritance)
    (should (eq (clime-option-name opt) 'verbose))))

(ert-deftest clime-test-cohort/param-accessors-on-arg ()
  "clime-param-name and clime-param-required work on clime-arg."
  (let ((arg (clime-make-arg :name 'file :required t :help "Input file")))
    (should (eq (clime-param-name arg) 'file))
    (should (eq (clime-param-required arg) t))
    (should (equal (clime-param-help arg) "Input file"))
    (should (eq (clime-arg-name arg) 'file))))

(ert-deftest clime-test-cohort/param-accessors-on-cohort ()
  "clime-param-name and clime-param-required work on clime-cohort."
  (let ((og (clime-make-cohort :name 'fmt :required t :help "Format")))
    (should (eq (clime-param-name og) 'fmt))
    (should (eq (clime-param-required og) t))
    (should (equal (clime-param-help og) "Format"))))

(ert-deftest clime-test-cohort/param-predicate ()
  "clime-param-p recognizes options, args, and cohorts."
  (should (clime-param-p (clime-make-option :name 'v :flags '("-v"))))
  (should (clime-param-p (clime-make-arg :name 'f)))
  (should (clime-param-p (clime-make-cohort :name 'g
                                              :resolve #'clime-cohort-check-exclusive))))

(ert-deftest clime-test-cohort/param-default-slot ()
  "clime-param-default works across option, arg, and cohort."
  (let ((opt (clime-make-option :name 'v :flags '("-v") :default 42))
        (arg (clime-make-arg :name 'f :default "stdin"))
        (og (clime-make-cohort :name 'g :resolve #'ignore :default 'text)))
    (should (= (clime-param-default opt) 42))
    (should (equal (clime-param-default arg) "stdin"))
    (should (eq (clime-param-default og) 'text))))

;;; ─── Cohort Struct ───────────────────────────────────────────────────

(ert-deftest clime-test-cohort/struct-slots ()
  "clime-cohort has name, resolve, required slots."
  (let ((og (clime-make-cohort :name 'fmt
                                      :resolve #'clime-cohort-check-exclusive
                                      :required t)))
    (should (eq (clime-cohort-name og) 'fmt))
    (should (eq (clime-cohort-resolve og) #'clime-cohort-check-exclusive))
    (should (eq (clime-cohort-required og) t))))

(ert-deftest clime-test-cohort/struct-requires-name ()
  "clime-make-cohort errors without :name."
  (should-error (clime-make-cohort :resolve #'clime-cohort-check-exclusive)))

(ert-deftest clime-test-cohort/cohort-slot ()
  "Options have :cohort slot, nil by default."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo"))))
    (should-not (clime-option-cohort opt))))

(ert-deftest clime-test-cohort/cohort-accepts-symbol ()
  ":cohort accepts a symbol."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :cohort 'fmt)))
    (should (eq (clime-option-cohort opt) 'fmt))))

;;; ─── Exclusive Group (at-most-one) ─────────────────────────────────

(ert-deftest clime-test-cohort/exclusive-single-ok ()
  "Exclusive group: one member set succeeds."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--json")))))
    (should (equal output "ok"))))

(ert-deftest clime-test-cohort/exclusive-two-error ()
  "Exclusive group: two members set signals error."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/exclusive-none-ok ()
  "Exclusive group without :required: zero members set is fine."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "ok"))))

(ert-deftest clime-test-cohort/exclusive-error-message ()
  "Exclusive group error lists conflicting flags."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (err (should-error
               (clime-parse app '("run" "--json" "--csv"))
               :type 'clime-usage-error)))
    (should (string-match-p "--json" (cadr err)))
    (should (string-match-p "--csv" (cadr err)))))

;;; ─── Exclusive Derived Value ───────────────────────────────────────

(ert-deftest clime-test-cohort/exclusive-derived-value ()
  "Exclusive group injects winner option name into params."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv")))))
    (should (equal output "csv"))))

(ert-deftest clime-test-cohort/exclusive-derived-nil-when-none ()
  "Exclusive group: no member set → group value is nil."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "nil"))))

;;; ─── Required Group ────────────────────────────────────────────────

(ert-deftest clime-test-cohort/required-none-error ()
  "Required group: error when no member set."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/required-one-ok ()
  "Required group: one member set succeeds."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv")))))
    (should (equal output "ok"))))

(ert-deftest clime-test-cohort/required-default-does-not-satisfy ()
  "Defaults don't satisfy required group."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt :default t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/required-error-lists-members ()
  "Required group error lists all member flags."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (err (should-error
               (clime-parse app '("run"))
               :type 'clime-usage-error)))
    (should (string-match-p "--json" (cadr err)))
    (should (string-match-p "--csv" (cadr err)))))

(ert-deftest clime-test-cohort/required-env-satisfies ()
  "Env var satisfies required group."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1" :env-prefix "TEST_OG"
                               :children (list (cons "run" cmd)))))
    (let ((process-environment (append '("TEST_OG_JSON=1")
                                       process-environment)))
      (let ((output (with-output-to-string
                      (clime-run app '("run")))))
        (should (equal output "ok"))))))

(ert-deftest clime-test-cohort/required-exit-2 ()
  "Required group violation returns exit code 2."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                       :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (exit-code nil))
    (with-output-to-string
      (clime-test-with-messages
        (setq exit-code (clime-run app '("run")))))
    (should (= exit-code 2))))

;;; ─── Paired Group ──────────────────────────────────────────────────

(ert-deftest clime-test-cohort/paired-two-options-zipped ()
  "Paired group zips member values into alist under group name."
  (let* ((og (clime-make-cohort :name 'sr
                                       :resolve #'clime-cohort-check-paired))
         (opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :cohort 'sr :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :cohort 'sr :multiple t))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%S" (clime-ctx-get ctx 'sr)))
                                  :options (list opt-skip opt-reason)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "A" "--reason" "x"
                                    "--skip" "B" "--reason" "y")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 2))
      (should (equal (alist-get 'skip (nth 0 result)) "A"))
      (should (equal (alist-get 'reason (nth 0 result)) "x"))
      (should (equal (alist-get 'skip (nth 1 result)) "B"))
      (should (equal (alist-get 'reason (nth 1 result)) "y")))))

(ert-deftest clime-test-cohort/paired-cardinality-error ()
  "Paired group: unequal cardinality signals error."
  (let* ((og (clime-make-cohort :name 'sr
                                       :resolve #'clime-cohort-check-paired))
         (opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :cohort 'sr :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :cohort 'sr :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "A" "--skip" "B" "--reason" "x"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/paired-partial-error ()
  "Paired group: partial presence signals error."
  (let* ((og (clime-make-cohort :name 'sr
                                       :resolve #'clime-cohort-check-paired))
         (opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :cohort 'sr :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :cohort 'sr :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "A"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/paired-all-absent-ok ()
  "Paired group: all absent is fine."
  (let* ((og (clime-make-cohort :name 'sr
                                       :resolve #'clime-cohort-check-paired))
         (opt-skip (clime-make-option :name 'skip :flags '("--skip")
                                       :cohort 'sr :multiple t))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason")
                                         :cohort 'sr :multiple t))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list opt-skip opt-reason)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run")))))
    (should (equal output "ok"))))

;;; ─── DSL: clime-mutex ──────────────────────────────────────────────

(ert-deftest clime-test-cohort/dsl-mutex ()
  "clime-mutex DSL wraps options into exclusive group."
  (eval
   '(clime-app clime-test--cohort-mutex-app
      :version "1"
      (clime-mutex output-format :required
        (clime-option fmt-json ("--json") :bool)
        (clime-option fmt-csv ("--csv") :bool))
      (clime-command run
        :help "Run"
        (clime-handler (ctx)
          (format "%s" (clime-ctx-get ctx 'output-format)))))
   t)
  (let* ((app (symbol-value 'clime-test--cohort-mutex-app)))
    ;; Should error with no format
    (should-error
     (clime-parse app '("run"))
     :type 'clime-usage-error)
    ;; Should succeed and derive value
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--json")))))
      (should (equal output "fmt-json")))))

(ert-deftest clime-test-cohort/dsl-mutex-options-on-node ()
  "clime-mutex promotes wrapped options to node's :options."
  (eval
   '(clime-app clime-test--cohort-mutex-opts-app
      :version "1"
      (clime-mutex fmt
        (clime-option json ("--json") :bool)
        (clime-option csv ("--csv") :bool))
      (clime-command run
        :help "Run"
        (clime-handler (_ctx) nil)))
   t)
  (let* ((app (symbol-value 'clime-test--cohort-mutex-opts-app)))
    (should (clime-node-find-option app "--json"))
    (should (clime-node-find-option app "--csv"))
    (should (eq (clime-option-cohort (clime-node-find-option app "--json")) 'fmt))))

;;; ─── DSL: clime-zip ───────────────────────────────────────────────

(ert-deftest clime-test-cohort/dsl-zip ()
  "clime-zip DSL wraps options into paired group."
  (eval
   '(clime-app clime-test--cohort-zip-app
      :version "1"
      (clime-command run
        :help "Run"
        (clime-zip skip-reason
          (clime-option skip ("--skip"))
          (clime-option reason ("--reason")))
        (clime-handler (ctx)
          (format "%S" (clime-ctx-get ctx 'skip-reason)))))
   t)
  (let* ((app (symbol-value 'clime-test--cohort-zip-app))
         (output (with-output-to-string
                   (clime-run app '("run" "--skip" "A" "--reason" "x")))))
    (let ((result (car (read-from-string output))))
      (should (= (length result) 1))
      (should (equal (alist-get 'skip (nth 0 result)) "A"))
      (should (equal (alist-get 'reason (nth 0 result)) "x")))))

;;; ─── Ancestor Groups ───────────────────────────────────────────────

(ert-deftest clime-test-cohort/ancestor-group ()
  "Option group on app applies to command-level parsing."
  (let* ((og (clime-make-cohort :name 'fmt
                                       :resolve #'clime-cohort-check-exclusive
                                       :required t))
         (app-opt (clime-make-option :name 'json :flags '("--json") :nargs 0
                                      :cohort 'fmt))
         (cmd-opt (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) "ok")
                                  :options (list cmd-opt)))
         (app (clime-make-app :name "t" :version "1"
                               :options (list app-opt)
                               :cohorts (list og)
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv")))))
    (should (equal output "ok"))))

;;; ─── Edge Cases ──────────────────────────────────────────────────

(ert-deftest clime-test-cohort/multiple-cohorts-same-node ()
  "Two distinct cohorts on a single command work independently."
  (let* ((og-fmt (clime-make-cohort :name 'fmt
                                     :resolve #'clime-cohort-check-exclusive))
         (og-src (clime-make-cohort :name 'src
                                     :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                      :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                     :cohort 'fmt))
         (opt-local (clime-make-option :name 'local :flags '("--local") :nargs 0
                                       :cohort 'src))
         (opt-remote (clime-make-option :name 'remote :flags '("--remote") :nargs 0
                                        :cohort 'src))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s/%s"
                                                     (clime-ctx-get ctx 'fmt)
                                                     (clime-ctx-get ctx 'src)))
                                  :options (list opt-json opt-csv opt-local opt-remote)
                                  :cohorts (list og-fmt og-src)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd))))
         (output (with-output-to-string
                   (clime-run app '("run" "--csv" "--remote")))))
    (should (equal output "csv/remote"))))

(ert-deftest clime-test-cohort/multiple-cohorts-independent-errors ()
  "Violation in one cohort doesn't mask violations in another."
  (let* ((og-fmt (clime-make-cohort :name 'fmt
                                     :resolve #'clime-cohort-check-exclusive))
         (og-src (clime-make-cohort :name 'src
                                     :resolve #'clime-cohort-check-exclusive))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                      :cohort 'fmt))
         (opt-csv (clime-make-option :name 'csv :flags '("--csv") :nargs 0
                                     :cohort 'fmt))
         (opt-local (clime-make-option :name 'local :flags '("--local") :nargs 0
                                       :cohort 'src))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-json opt-csv opt-local)
                                  :cohorts (list og-fmt og-src)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; fmt group violation
    (should-error
     (clime-parse app '("run" "--json" "--csv"))
     :type 'clime-usage-error)
    ;; src group: single member is fine (handler returns nil → no output)
    (let ((exit-code (clime-run app '("run" "--local"))))
      (should (= exit-code 0)))))

(ert-deftest clime-test-cohort/exclusive-value-taking-options ()
  "Exclusive group works with value-taking (non-boolean) options."
  (let* ((og (clime-make-cohort :name 'out
                                 :resolve #'clime-cohort-check-exclusive))
         (opt-file (clime-make-option :name 'file :flags '("--file")
                                      :cohort 'out))
         (opt-url (clime-make-option :name 'url :flags '("--url")
                                     :cohort 'out))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s:%s"
                                                     (clime-ctx-get ctx 'out)
                                                     (or (clime-ctx-get ctx 'file)
                                                         (clime-ctx-get ctx 'url))))
                                  :options (list opt-file opt-url)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; One value option set
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--file" "/tmp/x")))))
      (should (equal output "file:/tmp/x")))
    ;; Both set → error
    (should-error
     (clime-parse app '("run" "--file" "/tmp/x" "--url" "http://y"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/cohort-default-value ()
  "Cohort :default is injected when no member set and not :required."
  (let* ((og (clime-make-cohort :name 'fmt
                                 :resolve #'clime-cohort-check-exclusive
                                 :default 'text))
         (opt-json (clime-make-option :name 'json :flags '("--json") :nargs 0
                                      :cohort 'fmt))
         (cmd (clime-make-command :name "run"
                                  :handler (lambda (ctx)
                                             (format "%s" (clime-ctx-get ctx 'fmt)))
                                  :options (list opt-json)
                                  :cohorts (list og)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    ;; No member set → cohort default injected
    (let ((output (with-output-to-string
                    (clime-run app '("run")))))
      (should (equal output "text")))
    ;; Member set → derived value overrides default
    (let ((output (with-output-to-string
                    (clime-run app '("run" "--json")))))
      (should (equal output "json")))))

;;; ─── Deprecation: :mutex slot ──────────────────────────────────────

(ert-deftest clime-test-cohort/deprecated-mutex-slot ()
  ":mutex on option still works via deprecation path."
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

(ert-deftest clime-test-cohort/deprecated-mutex-maps-to-group ()
  ":mutex on option sets :group slot."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :mutex 'fmt)))
    (should (eq (clime-option-cohort opt) 'fmt))))

;;; ─── Deprecation: :zip slot ────────────────────────────────────────

(ert-deftest clime-test-cohort/deprecated-zip-slot ()
  ":zip on option still works via deprecation path."
  (let* ((opt-skip (clime-make-option :name 'skip :flags '("--skip") :zip 'sr))
         (opt-reason (clime-make-option :name 'reason :flags '("--reason") :zip 'sr))
         (cmd (clime-make-command :name "run" :handler (lambda (_ctx) nil)
                                  :options (list opt-skip opt-reason)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "run" cmd)))))
    (should-error
     (clime-parse app '("run" "--skip" "A"))
     :type 'clime-usage-error)))

(ert-deftest clime-test-cohort/deprecated-zip-maps-to-group ()
  ":zip on option sets :group slot."
  (let ((opt (clime-make-option :name 'foo :flags '("--foo") :zip 'sr)))
    (should (eq (clime-option-cohort opt) 'sr))))

(provide 'clime-cohort-tests)
;;; clime-cohort-tests.el ends here
