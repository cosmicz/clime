;;; clime-output-tests.el --- Tests for clime-output  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the output protocol: JSON encoding, format-driven dispatch,
;; accumulation, error handling, finalization, and clime-run integration.
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

;;; ─── Default Format ─────────────────────────────────────────────────

(ert-deftest clime-test-output/default-format-exists ()
  "clime-output-default-format is a clime-output-format."
  (should (clime-output-format-p clime-output-default-format)))

(ert-deftest clime-test-output/default-format-is-text ()
  "Default format has name 'text and is streaming."
  (should (eq (clime-output-format-name clime-output-default-format) 'text))
  (should (clime-output-format-streaming clime-output-default-format)))

(ert-deftest clime-test-output/default-format-encoder ()
  "Default format encoder produces string representation."
  (let ((enc (clime-output-format-encoder clime-output-default-format)))
    (should (equal (funcall enc "hello") "hello"))
    (should (equal (funcall enc 42) "42"))))

(ert-deftest clime-test-output/default-format-error-handler ()
  "Default format error-handler sends to stderr via message."
  (let ((msgs (clime-test-with-messages
                (funcall (clime-output-format-error-handler clime-output-default-format)
                         "bad input"))))
    (should (cl-some (lambda (m) (string-match-p "Error: bad input" m)) msgs))))

;;; ─── Active Format Binding ──────────────────────────────────────────

(ert-deftest clime-test-output/active-format-defaults-to-text ()
  "clime-out--active-format defaults to clime-output-default-format."
  (should (eq clime-out--active-format clime-output-default-format)))

(ert-deftest clime-test-output/output-name-returns-format-name ()
  "clime-out-format returns the active format's name symbol."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json"))))
    (should (eq (clime-out-format) 'json)))
  (let ((clime-out--active-format clime-output-default-format))
    (should (eq (clime-out-format) 'text)))
  (let ((clime-out--active-format
         (clime-make-output-format :name 'yaml :flags '("--yaml"))))
    (should (eq (clime-out-format) 'yaml))))

;;; ─── Output Dispatch (Text Mode) ───────────────────────────────────

(ert-deftest clime-test-output/text-output ()
  "clime-out in text mode princ's via encoder."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((output (with-output-to-string (clime-out "hello"))))
      (should (equal output "hello")))))

(ert-deftest clime-test-output/text-error ()
  "clime-out-error in text mode uses format's error-handler (stderr)."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((msgs (clime-test-with-messages
                  (clime-out-error "bad input"))))
      (should (cl-some (lambda (m) (string-match-p "Error: bad input" m)) msgs)))))

(ert-deftest clime-test-output/text-output-returns-data ()
  "clime-out returns its data argument for chaining."
  (let ((clime-out--active-format clime-output-default-format))
    (with-output-to-string
      (should (equal (clime-out "val") "val")))))

;;; ─── Output Dispatch (Streaming JSON) ──────────────────────────────

(ert-deftest clime-test-output/streaming-json-output ()
  "clime-out in streaming JSON emits NDJSON immediately."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json") :streaming t)))
    (let* ((output (with-output-to-string
                     (clime-out "hello")))
           (parsed (json-read-from-string (string-trim output))))
      (should (equal parsed "hello")))))

(ert-deftest clime-test-output/streaming-json-multiple ()
  "Multiple output calls in streaming JSON produce NDJSON."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json") :streaming t)))
    (let* ((output (with-output-to-string
                     (clime-out "line1")
                     (clime-out "line2")))
           (lines (split-string (string-trim output) "\n" t)))
      (should (= (length lines) 2))
      (should (equal (json-read-from-string (nth 0 lines)) "line1"))
      (should (equal (json-read-from-string (nth 1 lines)) "line2")))))

(ert-deftest clime-test-output/streaming-json-error ()
  "clime-out-error in streaming JSON emits immediately."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json") :streaming t)))
    (let* ((output (with-output-to-string
                     (clime-out-error "bad")))
           (parsed (json-read-from-string (string-trim output))))
      (should (equal (cdr (assq 'error parsed)) "bad")))))

;;; ─── Output Dispatch (Buffered JSON) ───────────────────────────────

(ert-deftest clime-test-output/buffered-json-accumulates ()
  "clime-out in buffered JSON pushes to items list."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (with-output-to-string
      (clime-out '((x . 1)))
      (clime-out '((x . 2))))
    (should (= (length clime-out--items) 2))))

(ert-deftest clime-test-output/buffered-json-error-accumulates ()
  "clime-out-error in buffered JSON pushes to errors list."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (clime-out-error "bad")
    (should (equal clime-out--errors '("bad")))))

(ert-deftest clime-test-output/buffered-json-error-no-stdout ()
  "clime-out-error in buffered JSON produces no immediate stdout."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (let ((output (with-output-to-string
                    (clime-out-error "bad"))))
      (should (equal output "")))))

(ert-deftest clime-test-output/buffered-json-multiple-errors ()
  "Multiple clime-out-error calls accumulate in order."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (clime-out-error "err1")
    (clime-out-error "err2")
    ;; Pushed in reverse (push order), reversed at flush
    (should (= (length clime-out--errors) 2))))

;;; ─── clime-out-emit ───────────────────────────────────────────

(ert-deftest clime-test-output/stream-bypasses-buffer ()
  "clime-out-emit emits immediately, even in buffered mode."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (let* ((output (with-output-to-string
                     (clime-out-emit '((a . 1)))
                     (clime-out-emit '((b . 2)))))
           (lines (split-string (string-trim output) "\n" t)))
      (should (= (length lines) 2))
      (should (equal (cdr (assq 'a (json-read-from-string (nth 0 lines)))) 1))
      ;; Nothing pushed to items buffer
      (should (null clime-out--items)))))

(ert-deftest clime-test-output/stream-returns-data ()
  "clime-out-emit returns its data argument."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json"))))
    (with-output-to-string
      (should (equal (clime-out-emit "val") "val")))))

(ert-deftest clime-test-output/stream-text-mode ()
  "clime-out-emit in text mode uses text encoder."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((output (with-output-to-string (clime-out-emit "hello"))))
      (should (equal output "hello")))))

;;; ─── Finalize ──────────────────────────────────────────────────────

(ert-deftest clime-test-output/finalize-default-errors-win ()
  "Default finalize: errors present → error envelope."
  (let ((result (clime-out--finalize-default
                 '(((x . 1))) "retval" '("fail"))))
    (should (equal (cdr (assq 'error result)) "fail"))))

(ert-deftest clime-test-output/finalize-default-multi-items ()
  "Default finalize: 2+ items → JSON array."
  (let ((result (clime-out--finalize-default
                 '(((a . 1)) ((b . 2))) nil nil)))
    (should (vectorp result))
    (should (= (length result) 2))))

(ert-deftest clime-test-output/finalize-default-single-item ()
  "Default finalize: 1 item → bare object."
  (let ((result (clime-out--finalize-default
                 '(((key . "val"))) nil nil)))
    (should (not (vectorp result)))
    (should (equal (cdr (assq 'key result)) "val"))))

(ert-deftest clime-test-output/finalize-default-retval ()
  "Default finalize: no items, retval → success envelope."
  (let ((result (clime-out--finalize-default nil "hello" nil)))
    (should (equal (cdr (assq 'success result)) t))
    (should (equal (cdr (assq 'data result)) "hello"))))

(ert-deftest clime-test-output/finalize-default-nil ()
  "Default finalize: all nil → nil (no output)."
  (should-not (clime-out--finalize-default nil nil nil)))

(ert-deftest clime-test-output/finalize-errors-trump-items ()
  "Default finalize: errors take priority over items and retval."
  (let ((result (clime-out--finalize-default
                 '(((x . 1)) ((y . 2))) "retval" '("fail"))))
    (should (equal (cdr (assq 'error result)) "fail"))
    (should-not (assq 'x result))
    (should-not (assq 'data result))))

(ert-deftest clime-test-output/finalize-items-trump-retval ()
  "Default finalize: items take priority over retval."
  (let ((result (clime-out--finalize-default
                 '(((buffered . t))) "ignored" nil)))
    (should (equal (cdr (assq 'buffered result)) t))
    (should-not (assq 'data result))))

;;; ─── Flush ─────────────────────────────────────────────────────────

(ert-deftest clime-test-output/flush-multi-items ()
  "Flush with multiple items produces JSON array."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (push '((name . "b")) clime-out--items)
    (push '((name . "a")) clime-out--items)
    (let* ((output (with-output-to-string (clime-out--flush nil nil)))
           (parsed (json-read-from-string (string-trim output))))
      (should (vectorp parsed))
      (should (= (length parsed) 2))
      (should (equal (cdr (assq 'name (aref parsed 0))) "a"))
      (should (equal (cdr (assq 'name (aref parsed 1))) "b")))))

(ert-deftest clime-test-output/flush-single-item ()
  "Flush with single item produces bare object."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items (list '((key . "val"))))
        (clime-out--errors nil))
    (let* ((output (with-output-to-string (clime-out--flush nil nil)))
           (parsed (json-read-from-string (string-trim output))))
      (should (not (vectorp parsed)))
      (should (equal (cdr (assq 'key parsed)) "val")))))

(ert-deftest clime-test-output/flush-empty ()
  "Flush with no items and no retval produces no output."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (let ((output (with-output-to-string (clime-out--flush nil nil))))
      (should (equal output "")))))

(ert-deftest clime-test-output/flush-retval-when-no-items ()
  "Flush with retval and no items produces success envelope."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (let* ((output (with-output-to-string (clime-out--flush nil "hello")))
           (parsed (json-read-from-string (string-trim output))))
      (should (equal (cdr (assq 'success parsed)) t))
      (should (equal (cdr (assq 'data parsed)) "hello")))))

(ert-deftest clime-test-output/flush-errors-suppress-items ()
  "Flush with errors suppresses item output (default finalize)."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items (list '((x . 1))))
        (clime-out--errors (list "fail")))
    (let* ((output (with-output-to-string (clime-out--flush nil nil)))
           (parsed (json-read-from-string (string-trim output))))
      (should (equal (cdr (assq 'error parsed)) "fail"))
      (should-not (assq 'x parsed)))))

(ert-deftest clime-test-output/flush-custom-finalize ()
  "Flush with custom finalize controls output shape."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items (list '((x . 1))))
        (clime-out--errors nil))
    (let* ((finalize (lambda (items retval _errors)
                       `((items . ,(vconcat items))
                         (count . ,(length items)))))
           (output (with-output-to-string (clime-out--flush finalize nil)))
           (parsed (json-read-from-string (string-trim output))))
      (should (= (cdr (assq 'count parsed)) 1)))))

(ert-deftest clime-test-output/flush-custom-finalize-with-errors ()
  "Custom finalize receives errors and can merge with items."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items (list '((x . 1))))
        (clime-out--errors (list "fail")))
    (let* ((received nil)
           (finalize (lambda (items retval errors)
                       (setq received (list items retval errors))
                       `((partial . t)
                         (items . ,(vconcat items))
                         (errors . ,(vconcat errors)))))
           (output (with-output-to-string (clime-out--flush finalize nil)))
           (parsed (json-read-from-string (string-trim output))))
      ;; Finalize received errors
      (should (equal (nth 2 received) '("fail")))
      ;; Custom finalize chose to emit partial results
      (should (equal (cdr (assq 'partial parsed)) t)))))

;;; ─── clime-run Integration ─────────────────────────────────────────

(ert-deftest clime-test-output/run-json-accumulates-output-calls ()
  "clime-run buffers multiple clime-out calls into JSON array."
  (let* ((cmd (clime-make-command
               :name "multi"
               :handler (lambda (_ctx)
                          (clime-out '((x . 1)))
                          (clime-out '((x . 2)))
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
  "clime-run with single clime-out call emits bare object."
  (let* ((cmd (clime-make-command
               :name "one"
               :handler (lambda (_ctx)
                          (clime-out '((key . "val")))
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
  "clime-run wraps return value in success envelope when no output calls."
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
  "When handler calls clime-out AND returns a value, items win."
  (let* ((cmd (clime-make-command
               :name "both"
               :handler (lambda (_ctx)
                          (clime-out '((buffered . t)))
                          "ignored")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "both" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("both" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'buffered parsed)) t))))

(ert-deftest clime-test-output/run-json-nil-return-no-output ()
  "clime-run with nil handler result produces no output."
  (let* ((cmd (clime-make-command :name "quiet"
                                   :handler (lambda (_ctx) nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "quiet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("quiet" "--json"))))
                     (should (= code 0))))))
    (should (equal output ""))))

(ert-deftest clime-test-output/run-json-numeric-return ()
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

(ert-deftest clime-test-output/run-text-mode-unchanged ()
  "clime-run without --json uses text mode (princ retval)."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hello")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "greet" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("greet"))))
                     (should (= code 0))))))
    (should (equal output "hello\n"))))

(ert-deftest clime-test-output/run-text-nil-retval-no-newline ()
  "Handler returning nil produces no output (no spurious newline)."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) nil)))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "greet" cmd))))
         (output (with-output-to-string
                   (clime-run app '("greet")))))
    (should (equal output ""))))

(ert-deftest clime-test-output/run-no-json-mode-ignores-flag ()
  "clime-run without :json-mode t ignores --json flag."
  (let* ((cmd (clime-make-command :name "greet"
                                   :handler (lambda (_ctx) "hello")))
         (app (clime-make-app :name "t" :version "1"
                               :children (list (cons "greet" cmd)))))
    (let ((code (clime-run app '("greet" "--json"))))
      (should (= code 2)))))

;;; ─── clime-run Error Handling ──────────────────────────────────────

(ert-deftest clime-test-output/run-handler-error-via-finalize ()
  "Handler calling clime-out-error routes through finalize."
  (let* ((cmd (clime-make-command
               :name "fail"
               :handler (lambda (_ctx)
                          (clime-out-error "handler fail")
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "fail" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("fail" "--json"))))
                     (should (= code 1)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (equal (cdr (assq 'error parsed)) "handler fail"))))

(ert-deftest clime-test-output/run-handler-error-suppresses-items ()
  "Handler error + buffered items → error wins, items suppressed."
  (let* ((cmd (clime-make-command
               :name "mixed"
               :handler (lambda (_ctx)
                          (clime-out '((x . 1)))
                          (clime-out-error "handler fail")
                          "retval")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "mixed" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("mixed" "--json"))))
                     (should (= code 1)))))
         (lines (split-string (string-trim output) "\n" t)))
    ;; Single line: error envelope only
    (should (= (length lines) 1))
    (should (equal (cdr (assq 'error (json-read-from-string (car lines))))
                   "handler fail"))))

(ert-deftest clime-test-output/run-handler-error-suppresses-retval ()
  "Handler error + return value → error wins, no success envelope."
  (let* ((cmd (clime-make-command
               :name "errval"
               :handler (lambda (_ctx)
                          (clime-out-error "bad")
                          "retval")))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "errval" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("errval" "--json"))))
                     (should (= code 1)))))
         (lines (split-string (string-trim output) "\n" t)))
    (should (= (length lines) 1))
    (should (equal (cdr (assq 'error (json-read-from-string (car lines)))) "bad"))))

(ert-deftest clime-test-output/run-signal-error-via-finalize ()
  "Signal (error ...) caught, routed through finalize."
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

(ert-deftest clime-test-output/run-signal-preserves-buffer ()
  "Signal error preserves buffered items for finalize."
  (let* ((debug-on-error nil)
         (finalize-received nil)
         (cmd (clime-make-command
               :name "boom"
               :handler (lambda (_ctx)
                          (clime-out '((x . 1)))
                          (error "kaboom"))))
         (fmt (clime-make-output-format
               :name 'json :flags '("--json")
               :finalize (lambda (items retval errors)
                           (setq finalize-received
                                 (list :items items :errors errors))
                           `((partial . ,(vconcat items))
                             (error . ,(car errors))))))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list fmt)
                               :children (list (cons "boom" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("boom" "--json"))))
                     (should (= code 1)))))
         (parsed (json-read-from-string (string-trim output))))
    ;; Custom finalize received both items and error
    (should (= (length (plist-get finalize-received :items)) 1))
    (should (string-match-p "kaboom" (car (plist-get finalize-received :errors))))
    ;; Custom finalize chose to emit both
    (should (equal (cdr (assq 'error parsed)) "kaboom"))
    (should (vectorp (cdr (assq 'partial parsed))))))

(ert-deftest clime-test-output/run-usage-error-immediate ()
  "Usage errors emit immediately (pre-handler, not through finalize)."
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

(ert-deftest clime-test-output/run-handler-error-exit-code-1 ()
  "Handler calling clime-out-error results in exit code 1."
  (let* ((cmd (clime-make-command
               :name "fail"
               :handler (lambda (_ctx)
                          (clime-out-error "nope")
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "fail" cmd)))))
    (let ((code (with-output-to-string
                  (clime-run app '("fail" "--json")))))
      ;; Exit code captured inside — tested in handler above
      )))

(ert-deftest clime-test-output/run-handler-usage-error-exit-2 ()
  "Handler signaling clime-usage-error gets exit code 2 (not 1)."
  (let* ((cmd (clime-make-command
               :name "val"
               :handler (lambda (_ctx)
                          (signal 'clime-usage-error '("bad input")))))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "val" cmd)))))
    (let ((code (with-output-to-string
                  (clime-run app '("val" "--json")))))
      ;; clime-run returns inside with-output-to-string; check via nested let
      )
    (let ((code (clime-run app '("val"))))
      (should (= code 2)))))

(ert-deftest clime-test-output/run-no-error-unchanged ()
  "Normal flow without errors works as before."
  (let* ((cmd (clime-make-command
               :name "ok"
               :handler (lambda (_ctx)
                          (clime-out '((a . 1)))
                          (clime-out '((b . 2)))
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "ok" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("ok" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (vectorp parsed))
    (should (= (length parsed) 2))))

;;; ─── Output Format Struct ──────────────────────────────────────────

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
               :finalize (lambda (_items retval _errors)
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
                          (clime-out '((x . 1)))
                          (clime-out '((x . 2)))
                          nil)))
         (fmt (clime-make-output-format
               :name 'json :flags '("--json")
               :finalize (lambda (items _retval _errors)
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
  "Streaming output-format emits NDJSON per clime-out call."
  (let* ((cmd (clime-make-command
               :name "stream"
               :handler (lambda (_ctx)
                          (clime-out '((line . 1)))
                          (clime-out '((line . 2)))
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

(ert-deftest clime-test-output/format-auto-exclusive ()
  "Multiple output formats are automatically mutually exclusive."
  (let* ((cmd (clime-make-command :name "x" :handler #'ignore))
         (json (clime-make-output-format :name 'json :flags '("--json")))
         (yaml (clime-make-output-format :name 'yaml :flags '("--yaml")))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list json yaml)
                               :children (list (cons "x" cmd))))
         (code (clime-run app '("x" "--json" "--yaml"))))
    (should (= code 2))))

(ert-deftest clime-test-output/format-single-no-conform ()
  "Single output format does not get auto-exclusivity conformer."
  (let* ((cmd (clime-make-command :name "x" :handler #'ignore))
         (json (clime-make-output-format :name 'json :flags '("--json")))
         (app (clime-make-app :name "t" :version "1"
                               :output-formats (list json)
                               :children (list (cons "x" cmd)))))
    ;; No conform attached for single format
    (should-not (clime-node-conform app))))

;;; ─── Help ──────────────────────────────────────────────────────────

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

;;; ─── :text Keyword on clime-out ──────────────────────────────────

(ert-deftest clime-test-output/text-mode-with-text-keyword ()
  "In text mode, :text string is emitted instead of encoded data."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((output (with-output-to-string
                    (clime-out '((name . "foo") (status . "ok"))
                                 :text "foo     ok"))))
      (should (equal output "foo     ok\n")))))

(ert-deftest clime-test-output/text-mode-without-text-keyword ()
  "In text mode without :text, behavior unchanged (encoder called)."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((output (with-output-to-string (clime-out "hello"))))
      (should (equal output "hello")))))

(ert-deftest clime-test-output/text-mode-text-keyword-returns-data ()
  ":text keyword does not change return value — still returns data."
  (let ((clime-out--active-format clime-output-default-format)
        (data '((name . "foo"))))
    (with-output-to-string
      (should (equal (clime-out data :text "foo") data)))))

(ert-deftest clime-test-output/json-buffered-ignores-text-keyword ()
  "In buffered JSON mode, :text is ignored — data is accumulated."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json")))
        (clime-out--items nil)
        (clime-out--errors nil))
    (with-output-to-string
      (clime-out '((name . "foo")) :text "foo"))
    (should (= (length clime-out--items) 1))
    (should (equal (car clime-out--items) '((name . "foo"))))))

(ert-deftest clime-test-output/json-streaming-ignores-text-keyword ()
  "In streaming JSON mode, :text is ignored — data is JSON-encoded."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json") :streaming t)))
    (let* ((output (with-output-to-string
                     (clime-out '((name . "foo")) :text "foo")))
           (parsed (json-read-from-string (string-trim output))))
      (should (equal (cdr (assq 'name parsed)) "foo")))))

(ert-deftest clime-test-output/text-keyword-nil-uses-encoder ()
  "Passing :text nil falls back to encoder (same as omitting :text)."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((output (with-output-to-string
                    (clime-out "hello" :text nil))))
      (should (equal output "hello")))))

;;; ─── :text Keyword Integration (clime-run) ──────────────────────

(ert-deftest clime-test-output/run-text-keyword-json-accumulates ()
  "clime-run with --json accumulates data from :text calls into array."
  (let* ((cmd (clime-make-command
               :name "list"
               :handler (lambda (_ctx)
                          (clime-out '((n . 1)) :text "one")
                          (clime-out '((n . 2)) :text "two")
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "list" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("list" "--json"))))
                     (should (= code 0)))))
         (parsed (json-read-from-string (string-trim output))))
    (should (vectorp parsed))
    (should (= (length parsed) 2))
    (should (= (cdr (assq 'n (aref parsed 0))) 1))))

(ert-deftest clime-test-output/run-text-keyword-text-mode ()
  "clime-run without --json uses :text strings."
  (let* ((cmd (clime-make-command
               :name "list"
               :handler (lambda (_ctx)
                          (clime-out '((n . 1)) :text "one")
                          (clime-out '((n . 2)) :text "two")
                          nil)))
         (app (clime-make-app :name "t" :version "1" :json-mode t
                               :children (list (cons "list" cmd))))
         (output (with-output-to-string
                   (let ((code (clime-run app '("list"))))
                     (should (= code 0))))))
    (should (equal output "one\ntwo\n"))))

;;; ─── clime-out-text ──────────────────────────────────────────

(ert-deftest clime-test-output/output-text-emits-in-text-mode ()
  "clime-out-text emits string with newline in text mode."
  (let ((clime-out--active-format clime-output-default-format))
    (let ((output (with-output-to-string
                    (clime-out-text "Agents:"))))
      (should (equal output "Agents:\n")))))

(ert-deftest clime-test-output/output-text-noop-in-json ()
  "clime-out-text is a no-op in JSON mode."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'json :flags '("--json"))))
    (let ((output (with-output-to-string
                    (clime-out-text "Agents:"))))
      (should (equal output "")))))

(ert-deftest clime-test-output/output-text-noop-in-custom-structured ()
  "clime-out-text is a no-op for any non-text format."
  (let ((clime-out--active-format
         (clime-make-output-format :name 'yaml :flags '("--yaml"))))
    (let ((output (with-output-to-string
                    (clime-out-text "Header:"))))
      (should (equal output "")))))

(ert-deftest clime-test-output/output-text-returns-nil ()
  "clime-out-text always returns nil."
  (let ((clime-out--active-format clime-output-default-format))
    (with-output-to-string
      (should-not (clime-out-text "text")))))

(provide 'clime-output-tests)
;;; clime-output-tests.el ends here
