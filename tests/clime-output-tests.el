;;; clime-output-tests.el --- Tests for clime-output  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the output protocol: JSON encoding, output helpers,
;; pre-parse detection, and clime-run integration.
;;
;; Note: json-encode internally uses princ via with-output-to-string,
;; so we must NOT rebind princ with cl-letf (that breaks json-encode).
;; Instead we use with-output-to-string to redirect standard-output.

;;; Code:

(require 'ert)
(require 'json)
(require 'clime-core)
(require 'clime-output)
(require 'clime-run)
(require 'clime-test-helpers)

;;; ─── JSON Encoding ──────────────────────────────────────────────────

(ert-deftest clime-test-output/json-encode-string ()
  "Strings encode as JSON strings."
  (should (equal (clime-json-encode "hello") "\"hello\"")))

(ert-deftest clime-test-output/json-encode-nil-is-null ()
  "nil encodes as JSON null."
  (should (equal (clime-json-encode nil) "null")))

(ert-deftest clime-test-output/json-encode-vector-is-array ()
  "Vectors encode as JSON arrays."
  (let ((parsed (json-read-from-string (clime-json-encode [1 2 3]))))
    (should (equal parsed [1 2 3]))))

(ert-deftest clime-test-output/json-encode-empty-vector ()
  "Empty vector encodes as empty JSON array."
  (should (equal (clime-json-encode []) "[]")))

(ert-deftest clime-test-output/json-encode-null-keyword ()
  ":null encodes as JSON string \"null\" (not JSON null — use nil for that)."
  (should (equal (clime-json-encode :null) "\"null\"")))

(ert-deftest clime-test-output/json-encode-json-false ()
  ":json-false encodes as JSON false."
  (should (equal (clime-json-encode :json-false) "false")))

(ert-deftest clime-test-output/json-encode-plist ()
  "Plists encode as JSON objects."
  (let ((result (json-read-from-string (clime-json-encode '(:key "val")))))
    (should (equal (cdr (assq 'key result)) "val"))))

;;; ─── Pre-parse Detection ────────────────────────────────────────────

(ert-deftest clime-test-output/pre-parse-detects-json ()
  "--json in argv is detected."
  (should (clime--pre-parse-json-p '("--json" "list"))))

(ert-deftest clime-test-output/pre-parse-no-json ()
  "Absent --json returns nil."
  (should-not (clime--pre-parse-json-p '("list" "--verbose"))))

(ert-deftest clime-test-output/pre-parse-json-anywhere ()
  "--json anywhere in argv is detected."
  (should (clime--pre-parse-json-p '("show" "--json" "id"))))

;;; ─── Auto-inject --json Option ──────────────────────────────────────

(ert-deftest clime-test-output/ensure-json-option-adds ()
  "Auto-inject adds --json option when :json-mode t."
  (let ((app (clime-make-app :name "t" :version "1" :json-mode t)))
    (clime--ensure-json-option app)
    (should (clime-node-find-option app "--json"))))

(ert-deftest clime-test-output/ensure-json-option-idempotent ()
  "Auto-inject is idempotent — does not duplicate."
  (let ((app (clime-make-app :name "t" :version "1" :json-mode t)))
    (clime--ensure-json-option app)
    (clime--ensure-json-option app)
    (let ((count (cl-count-if
                  (lambda (opt)
                    (member "--json" (clime-option-flags opt)))
                  (clime-group-options app))))
      (should (= count 1)))))

(ert-deftest clime-test-output/ensure-json-option-skips-no-json-mode ()
  "Auto-inject does nothing when :json-mode is nil."
  (let ((app (clime-make-app :name "t" :version "1")))
    (clime--ensure-json-option app)
    (should-not (clime-node-find-option app "--json"))))

;;; ─── Output Helpers (Text Mode) ─────────────────────────────────────

(ert-deftest clime-test-output/output-text-mode ()
  "clime-output in text mode princ's the data."
  (let ((output (with-output-to-string
                  (let ((clime--json-mode-p nil))
                    (clime-output "hello")))))
    (should (equal output "hello"))))

(ert-deftest clime-test-output/output-success-text-mode ()
  "clime-output-success in text mode princ's data as-is."
  (let ((output (with-output-to-string
                  (let ((clime--json-mode-p nil))
                    (clime-output-success "done")))))
    (should (equal output "done"))))

(ert-deftest clime-test-output/output-error-text-mode ()
  "clime-output-error in text mode uses message (stderr)."
  (let ((msgs (clime-test-with-messages
                (let ((clime--json-mode-p nil))
                  (clime-output-error "bad input")))))
    (should (cl-some (lambda (m) (string-match-p "Error: bad input" m)) msgs))))

(ert-deftest clime-test-output/output-list-text-mode ()
  "clime-output-list in text mode prints one item per line."
  (let ((output (with-output-to-string
                  (let ((clime--json-mode-p nil))
                    (clime-output-list '("a" "b" "c"))))))
    (should (equal output "a\nb\nc\n"))))

;;; ─── Output Helpers (JSON Mode) ─────────────────────────────────────

(ert-deftest clime-test-output/output-json-mode ()
  "clime-output in JSON mode emits JSON + newline."
  (let ((output (with-output-to-string
                  (let ((clime--json-mode-p t))
                    (clime-output "hello")))))
    (should (equal output "\"hello\"\n"))))

(ert-deftest clime-test-output/output-success-json-mode ()
  "clime-output-success in JSON mode emits success envelope."
  (let* ((output (with-output-to-string
                   (let ((clime--json-mode-p t))
                     (clime-output-success "done"))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) "done"))))

(ert-deftest clime-test-output/output-error-json-mode ()
  "clime-output-error in JSON mode emits error envelope to stdout."
  (let* ((output (with-output-to-string
                   (let ((clime--json-mode-p t))
                     (clime-output-error "bad input"))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'error parsed)) "bad input"))))

(ert-deftest clime-test-output/output-list-json-mode ()
  "clime-output-list in JSON mode emits array in success envelope."
  (let* ((output (with-output-to-string
                   (let ((clime--json-mode-p t))
                     (clime-output-list '("a" "b" "c")))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) ["a" "b" "c"]))))

(ert-deftest clime-test-output/output-list-json-vector ()
  "clime-output-list accepts vectors in JSON mode."
  (let* ((output (with-output-to-string
                   (let ((clime--json-mode-p t))
                     (clime-output-list ["x" "y"]))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'data parsed)) ["x" "y"]))))

;;; ─── NDJSON ─────────────────────────────────────────────────────────

(ert-deftest clime-test-output/ndjson-multiple-calls ()
  "Multiple output calls in JSON mode produce NDJSON."
  (let* ((output (with-output-to-string
                   (let ((clime--json-mode-p t))
                     (clime-output "line1")
                     (clime-output "line2"))))
         (lines (split-string (string-trim output) "\n" t)))
    (should (= (length lines) 2))
    (should (equal (json-read-from-string (nth 0 lines)) "line1"))
    (should (equal (json-read-from-string (nth 1 lines)) "line2"))))

;;; ─── clime-run Integration ──────────────────────────────────────────

(ert-deftest clime-test-output/run-json-mode-success ()
  "clime-run with --json wraps handler result in success envelope."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hello")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "greet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("greet" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) "hello"))))

(ert-deftest clime-test-output/run-json-mode-nil-return ()
  "clime-run with --json and nil handler result produces no output."
  (let* ((cmd (clime-make-command :name "quiet"
                                   :handler (lambda (_ctx) nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "quiet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("quiet" "--json"))))
                     (should (= code 0))))))
    (should (equal output ""))))

(ert-deftest clime-test-output/run-json-mode-usage-error ()
  "clime-run with --json emits JSON error for usage errors."
  (let* ((cmd (clime-make-command :name "show"
                                   :handler #'ignore
                                   :args (list (clime-make-arg :name 'id))))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "show" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("show" "--json"))))
                     (should (= code 2)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (stringp (cdr (assq 'error parsed))))))

(ert-deftest clime-test-output/run-json-mode-runtime-error ()
  "clime-run with --json emits JSON error for runtime errors."
  (let* ((cmd (clime-make-command :name "boom"
                                   :handler (lambda (_ctx) (error "kaboom"))))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "boom" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("boom" "--json"))))
                     (should (= code 1)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (string-match-p "kaboom" (cdr (assq 'error parsed))))))

(ert-deftest clime-test-output/run-text-mode-unchanged ()
  "clime-run without --json behaves as before (text output)."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hello")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "greet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("greet"))))
                     (should (= code 0))))))
    (should (equal output "hello"))))

(ert-deftest clime-test-output/run-no-json-mode-ignores-flag ()
  "clime-run without :json-mode t ignores --json flag."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hello")))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "greet" cmd)))))
    ;; --json should be treated as unknown option (no :json-mode)
    (let ((code (clime-run app '("greet" "--json"))))
      (should (= code 2)))))

(ert-deftest clime-test-output/json-option-visible-in-help ()
  "Auto-injected --json option appears in help output."
  (let* ((cmd (clime-make-command :name "list" :handler #'ignore
                                   :help "List items"))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "list" cmd))))
         (output (with-output-to-string
                   (clime-run app '("--help")))))
    (should (string-match-p "--json" output))
    (should (string-match-p "Output as JSON" output))))

;;; ─── Edge Cases ──────────────────────────────────────────────────────

(ert-deftest clime-test-output/output-returns-data ()
  "clime-output returns its data argument for chaining."
  (let ((result (with-output-to-string
                  (let ((clime--json-mode-p nil))
                    (should (equal (clime-output "val") "val"))))))
    (should (stringp result))))

(ert-deftest clime-test-output/output-success-returns-data ()
  "clime-output-success returns its data argument."
  (with-output-to-string
    (let ((clime--json-mode-p t))
      (should (equal (clime-output-success 42) 42)))))

(ert-deftest clime-test-output/output-list-empty-text ()
  "clime-output-list with empty list produces no output in text mode."
  (let ((output (with-output-to-string
                  (let ((clime--json-mode-p nil))
                    (clime-output-list '())))))
    (should (equal output ""))))

(ert-deftest clime-test-output/output-list-empty-json ()
  "clime-output-list with empty list produces empty array in JSON mode."
  (let* ((output (with-output-to-string
                   (let ((clime--json-mode-p t))
                     (clime-output-list '()))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) []))))

(ert-deftest clime-test-output/run-json-mode-numeric-return ()
  "clime-run with --json wraps numeric handler result."
  (let* ((cmd (clime-make-command :name "count"
                                   :handler (lambda (_ctx) 42)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "count" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("count" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'data parsed)) 42))))

(ert-deftest clime-test-output/run-json-mode-handler-uses-output-helpers ()
  "Handler can call clime-output-* directly in JSON mode."
  (let* ((cmd (clime-make-command
               :name "items"
               :handler (lambda (_ctx)
                          (clime-output-list '("x" "y"))
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "items" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("items" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'data parsed)) ["x" "y"]))))

(provide 'clime-output-tests)
;;; clime-output-tests.el ends here
