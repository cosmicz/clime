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

;;; ─── Auto-inject --json Option ──────────────────────────────────────

(ert-deftest clime-test-output/json-option-injected-at-construction ()
  "Constructor auto-injects --json option when :json-mode t."
  (let ((app (clime-make-app :name "t" :version "1" :json-mode t)))
    (should (clime-node-find-option app "--json"))))

(ert-deftest clime-test-output/json-option-not-duplicated ()
  "Constructor does not duplicate --json when user already defined it."
  (let* ((user-opt (clime-make-option :name 'json :flags '("--json") :nargs 0))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                              :options (list user-opt))))
    (let ((count (cl-count-if
                  (lambda (opt)
                    (member "--json" (clime-option-flags opt)))
                  (clime-node-options app))))
      (should (= count 1)))))

(ert-deftest clime-test-output/json-option-skipped-no-json-mode ()
  "Constructor does not inject --json when :json-mode is nil."
  (let ((app (clime-make-app :name "t" :version "1")))
    (should-not (clime-node-find-option app "--json"))))

;;; ─── Output Helpers (Text Mode) ─────────────────────────────────────

(ert-deftest clime-test-output/output-text-mode ()
  "clime-output in text mode princ's the data."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'text))
                    (clime-output "hello")))))
    (should (equal output "hello"))))

(ert-deftest clime-test-output/output-success-text-mode ()
  "clime-output-success in text mode princ's data as-is."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'text))
                    (clime-output-success "done")))))
    (should (equal output "done"))))

(ert-deftest clime-test-output/output-error-text-mode ()
  "clime-output-error in text mode uses message (stderr)."
  (let ((msgs (clime-test-with-messages
                (let ((clime-output-mode 'text))
                  (clime-output-error "bad input")))))
    (should (cl-some (lambda (m) (string-match-p "Error: bad input" m)) msgs))))

(ert-deftest clime-test-output/output-list-text-mode ()
  "clime-output-list in text mode prints one item per line."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'text))
                    (clime-output-list '("a" "b" "c"))))))
    (should (equal output "a\nb\nc\n"))))

;;; ─── Output Helpers (JSON Mode) ─────────────────────────────────────

(ert-deftest clime-test-output/output-json-mode ()
  "clime-output in JSON mode emits JSON + newline."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'json))
                    (clime-output "hello")))))
    (should (equal output "\"hello\"\n"))))

(ert-deftest clime-test-output/output-success-json-mode ()
  "clime-output-success in JSON mode emits success envelope."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json))
                     (clime-output-success "done"))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) "done"))))

(ert-deftest clime-test-output/output-error-json-mode ()
  "clime-output-error in JSON mode emits error envelope to stdout."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json))
                     (clime-output-error "bad input"))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'error parsed)) "bad input"))))

(ert-deftest clime-test-output/output-list-json-mode ()
  "clime-output-list in JSON mode emits array in success envelope."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json))
                     (clime-output-list '("a" "b" "c")))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) ["a" "b" "c"]))))

(ert-deftest clime-test-output/output-list-json-vector ()
  "clime-output-list accepts vectors in JSON mode."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json))
                     (clime-output-list ["x" "y"]))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'data parsed)) ["x" "y"]))))

;;; ─── NDJSON (unbuffered) ─────────────────────────────────────────────

(ert-deftest clime-test-output/ndjson-multiple-calls ()
  "Multiple output calls without buffer produce NDJSON."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json))
                     (clime-output "line1")
                     (clime-output "line2"))))
         (lines (split-string (string-trim output) "\n" t)))
    (should (= (length lines) 2))
    (should (equal (json-read-from-string (nth 0 lines)) "line1"))
    (should (equal (json-read-from-string (nth 1 lines)) "line2"))))

;;; ─── Output Accumulation ────────────────────────────────────────────

(ert-deftest clime-test-output/accumulate-multi-items ()
  "Multiple clime-output calls with buffer produce JSON array on flush."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json)
                         (clime--output-buffer '(:buffer)))
                     (clime-output '((name . "a")))
                     (clime-output '((name . "b")))
                     (clime--output-flush))))
         (parsed (json-read-from-string (string-trim output))))
    (should (vectorp parsed))
    (should (= (length parsed) 2))
    (should (equal (cdr (assq 'name (aref parsed 0))) "a"))
    (should (equal (cdr (assq 'name (aref parsed 1))) "b"))))

(ert-deftest clime-test-output/accumulate-single-item ()
  "Single clime-output call with buffer produces bare object on flush."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json)
                         (clime--output-buffer '(:buffer)))
                     (clime-output '((key . "val")))
                     (clime--output-flush))))
         (parsed (json-read-from-string (string-trim output))))
    (should (not (vectorp parsed)))
    (should (equal (cdr (assq 'key parsed)) "val"))))

(ert-deftest clime-test-output/accumulate-empty ()
  "No clime-output calls with buffer produces no output on flush."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'json)
                        (clime--output-buffer '(:buffer)))
                    (clime--output-flush)))))
    (should (equal output ""))))

(ert-deftest clime-test-output/accumulate-text-mode-unchanged ()
  "clime-output in text mode ignores buffer, prints immediately."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'text)
                        (clime--output-buffer '(:buffer)))
                    (clime-output "hello")))))
    (should (equal output "hello"))))

(ert-deftest clime-test-output/accumulate-success-buffered ()
  "clime-output-success pushes to buffer in JSON mode."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json)
                         (clime--output-buffer '(:buffer)))
                     (clime-output-success "done")
                     (clime--output-flush))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) "done"))))

(ert-deftest clime-test-output/accumulate-list-buffered ()
  "clime-output-list pushes to buffer in JSON mode."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json)
                         (clime--output-buffer '(:buffer)))
                     (clime-output-list '("x" "y"))
                     (clime--output-flush))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) ["x" "y"]))))

(ert-deftest clime-test-output/error-never-buffered ()
  "clime-output-error always emits immediately, even with buffer."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json)
                         (clime--output-buffer '(:buffer)))
                     (clime-output-error "bad"))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'error parsed)) "bad"))))

(ert-deftest clime-test-output/stream-bypasses-buffer ()
  "clime-output-stream emits NDJSON immediately, bypassing accumulator."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json)
                         (clime--output-buffer '(:buffer)))
                     (clime-output-stream '((a . 1)))
                     (clime-output-stream '((b . 2))))))
         (lines (split-string (string-trim output) "\n" t)))
    (should (= (length lines) 2))
    (should (equal (cdr (assq 'a (json-read-from-string (nth 0 lines)))) 1))
    (should (equal (cdr (assq 'b (json-read-from-string (nth 1 lines)))) 2))))

(ert-deftest clime-test-output/stream-returns-data ()
  "clime-output-stream returns its data argument."
  (with-output-to-string
    (let ((clime-output-mode 'json))
      (should (equal (clime-output-stream "val") "val")))))

;;; ─── clime-run Accumulation Integration ─────────────────────────────

(ert-deftest clime-test-output/run-json-accumulates-output-calls ()
  "clime-run buffers multiple clime-output calls into JSON array."
  (let* ((cmd (clime-make-command
               :name "multi"
               :handler (lambda (_ctx)
                          (clime-output '((x . 1)))
                          (clime-output '((x . 2)))
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "multi" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("multi" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (vectorp parsed))
    (should (= (length parsed) 2))))

(ert-deftest clime-test-output/run-json-single-output-bare ()
  "clime-run with single clime-output call emits bare object."
  (let* ((cmd (clime-make-command
               :name "one"
               :handler (lambda (_ctx)
                          (clime-output '((key . "val")))
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "one" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("one" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (not (vectorp parsed)))
    (should (equal (cdr (assq 'key parsed)) "val"))))

(ert-deftest clime-test-output/run-json-return-value-when-no-output ()
  "clime-run still wraps return value in success envelope when no output calls."
  (let* ((cmd (clime-make-command
               :name "ret"
               :handler (lambda (_ctx) "hello")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "ret" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("ret" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) "hello"))))

(ert-deftest clime-test-output/run-json-output-takes-precedence-over-return ()
  "When handler calls clime-output AND returns a value, buffer wins."
  (let* ((cmd (clime-make-command
               :name "both"
               :handler (lambda (_ctx)
                          (clime-output '((buffered . t)))
                          "ignored")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "both" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("both" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'buffered parsed)) t))))

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
  (let* ((debug-on-error nil)
         (cmd (clime-make-command :name "boom"
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

;;; ─── Output Format ──────────────────────────────────────────────────

(ert-deftest clime-test-output/format-struct-inherits-option ()
  "clime-output-format inherits clime-option slots."
  (let ((fmt (clime-make-output-format :name 'json :flags '("--json")
                                        :help "JSON out")))
    (should (clime-output-format-p fmt))
    (should (clime-option-p fmt))
    (should (eq (clime-option-name fmt) 'json))
    (should (equal (clime-option-flags fmt) '("--json")))
    (should (eql (clime-option-nargs fmt) 0))
    (should (equal (clime-option-category fmt) "Output"))))

(ert-deftest clime-test-output/format-in-app ()
  "App with clime-output-format generates option and stores format."
  (let* ((fmt (clime-make-output-format :name 'json :flags '("--json")))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt))))
    (should (= (length (clime-app-output-formats app)) 1))
    (should (clime-node-find-option app "--json"))))

(ert-deftest clime-test-output/format-replaces-json-mode ()
  "App with output-format json works like :json-mode t."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hi")))
         (fmt (clime-make-output-format :name 'json :flags '("--json")))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt)
                               :children (list (cons "greet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("greet" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'success parsed)) t))
    (should (equal (cdr (assq 'data parsed)) "hi"))))

(ert-deftest clime-test-output/format-custom-finalize ()
  "Custom finalize controls envelope shape."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hi")))
         (fmt (clime-make-output-format
               :name 'json :flags '("--json")
               :finalize (lambda (_items retval)
                           (when retval `((result . ,retval) (status . "ok"))))))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt)
                               :children (list (cons "greet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("greet" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'result parsed)) "hi"))
    (should (equal (cdr (assq 'status parsed)) "ok"))))

(ert-deftest clime-test-output/format-custom-finalize-with-items ()
  "Custom finalize receives accumulated items."
  (let* ((cmd (clime-make-command
               :name "multi"
               :handler (lambda (_ctx)
                          (clime-output '((x . 1)))
                          (clime-output '((x . 2)))
                          nil)))
         (fmt (clime-make-output-format
               :name 'json :flags '("--json")
               :finalize (lambda (items _retval)
                           `((items . ,(vconcat items))
                             (count . ,(length items))))))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt)
                               :children (list (cons "multi" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("multi" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (= (cdr (assq 'count parsed)) 2))
    (should (= (length (cdr (assq 'items parsed))) 2))))

(ert-deftest clime-test-output/format-streaming ()
  "Streaming output-format emits NDJSON per clime-output call."
  (let* ((cmd (clime-make-command
               :name "stream"
               :handler (lambda (_ctx)
                          (clime-output '((line . 1)))
                          (clime-output '((line . 2)))
                          nil)))
         (fmt (clime-make-output-format :name 'json :flags '("--json")
                                         :streaming t))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt)
                               :children (list (cons "stream" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("stream" "--json"))))
                     (should (= code 0)))))
         (lines (split-string (string-trim output) "\n" t)))
    (should (= (length lines) 2))
    (should (= (cdr (assq 'line (json-read-from-string (nth 0 lines)))) 1))
    (should (= (cdr (assq 'line (json-read-from-string (nth 1 lines)))) 2))))

(ert-deftest clime-test-output/format-json-mode-sugar ()
  ":json-mode t synthesizes output-format."
  (let ((app (clime-make-app :name "t" :version "1" :json-mode t)))
    (should (= (length (clime-app-output-formats app)) 1))
    (should (eq (clime-output-format-name (car (clime-app-output-formats app))) 'json))
    (should (clime-node-find-option app "--json"))))

(ert-deftest clime-test-output/format-json-mode-conflict ()
  ":json-mode t + explicit json output-format signals error."
  (let ((fmt (clime-make-output-format :name 'json :flags '("--json"))))
    (should-error
     (clime-make-app :name "t" :version "1" :json-mode t
                      :output-formats (list fmt))
     :type 'error)))

(ert-deftest clime-test-output/format-auto-mutex ()
  "Output formats get auto-assigned mutex group."
  (let* ((fmt (clime-make-output-format :name 'json :flags '("--json")))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt))))
    (should (eq (clime-option-mutex (car (clime-app-output-formats app)))
                'clime--output-format))))

(ert-deftest clime-test-output/format-mutex-enforced ()
  "Multiple output formats are mutually exclusive."
  (let* ((cmd (clime-make-command :name "x" :handler #'ignore))
         (json (clime-make-output-format :name 'json :flags '("--json")))
         (yaml (clime-make-output-format :name 'yaml :flags '("--yaml")))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list json yaml)
                               :children (list (cons "x" cmd))))
         (code (clime-run app '("x" "--json" "--yaml"))))
    (should (= code 2))))

;;; ─── Output Mode Variable ────────────────────────────────────────────

(ert-deftest clime-test-output/output-mode-defaults-to-text ()
  "clime-output-mode defaults to text."
  (should (eq clime-output-mode 'text)))

(ert-deftest clime-test-output/output-mode-json-p-true ()
  "clime-output-mode-json-p returns non-nil in json mode."
  (let ((clime-output-mode 'json))
    (should (clime-output-mode-json-p))))

(ert-deftest clime-test-output/output-mode-json-p-false ()
  "clime-output-mode-json-p returns nil in text mode."
  (let ((clime-output-mode 'text))
    (should-not (clime-output-mode-json-p))))

;;; ─── Edge Cases ──────────────────────────────────────────────────────

(ert-deftest clime-test-output/output-returns-data ()
  "clime-output returns its data argument for chaining."
  (let ((result (with-output-to-string
                  (let ((clime-output-mode 'text))
                    (should (equal (clime-output "val") "val"))))))
    (should (stringp result))))

(ert-deftest clime-test-output/output-success-returns-data ()
  "clime-output-success returns its data argument."
  (with-output-to-string
    (let ((clime-output-mode 'json))
      (should (equal (clime-output-success 42) 42)))))

(ert-deftest clime-test-output/output-list-empty-text ()
  "clime-output-list with empty list produces no output in text mode."
  (let ((output (with-output-to-string
                  (let ((clime-output-mode 'text))
                    (clime-output-list '())))))
    (should (equal output ""))))

(ert-deftest clime-test-output/output-list-empty-json ()
  "clime-output-list with empty list produces empty array in JSON mode."
  (let* ((output (with-output-to-string
                   (let ((clime-output-mode 'json))
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
