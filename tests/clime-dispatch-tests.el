;;; clime-dispatch-tests.el --- Tests for transport-neutral dispatch port  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the transport-neutral dispatch request/response contract.
;; These are intentionally red until the dispatch port is implemented.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-dsl)

(condition-case nil
    (require 'clime-dispatch)
  (error nil))

(defun clime-test-dispatch--require-port ()
  "Assert that the dispatch port API is present."
  (should (fboundp 'clime-make-dispatch-input))
  (should (fboundp 'clime-dispatch-input-p))
  (should (fboundp 'clime-make-dispatch-request))
  (should (fboundp 'clime-dispatch-request-p))
  (should (fboundp 'clime-make-dispatch-response))
  (should (fboundp 'clime-dispatch-response-p)))

(ert-deftest clime-test-dispatch/request-preserves-unmerged-input-provenance ()
  "A dispatch request preserves duplicate names from distinct input sources."
  (clime-test-dispatch--require-port)
  (let* ((query-input (clime-make-dispatch-input
                       :name 'id :value "from-query" :source 'query))
         (body-input (clime-make-dispatch-input
                      :name 'id :value "from-body" :source 'json-body))
         (request (clime-make-dispatch-request
                   :surface 'serve
                   :adapter 'http
                   :path '("items" "show")
                   :inputs (list query-input body-input)
                   :metadata '(:method POST :content-type "application/json")
                   :correlation-id "req-1")))
    (should (clime-dispatch-request-p request))
    (should (equal (mapcar #'clime-dispatch-input-source
                           (clime-dispatch-request-inputs request))
                   '(query json-body)))
    (should (equal (mapcar #'clime-dispatch-input-value
                           (clime-dispatch-request-inputs request))
                   '("from-query" "from-body")))))

(ert-deftest clime-test-dispatch/request-rejects-raw-adapter-objects ()
  "Raw adapter objects and raw payloads cannot cross the dispatch port."
  (clime-test-dispatch--require-port)
  (should-error
   (clime-make-dispatch-request
    :surface 'serve
    :adapter 'http
    :path '("items")
    :metadata '(:method GET :raw-request opaque-web-server-request)))
  (should-error
   (clime-make-dispatch-request
    :surface 'serve
    :adapter 'http
    :path '("items")
    :metadata '(:method POST :raw-body "{\"secret\":true}"))))

(ert-deftest clime-test-dispatch/request-rejects-arbitrary-callables ()
  "The request model cannot carry arbitrary Elisp evaluation instructions."
  (clime-test-dispatch--require-port)
  (should-error
   (clime-make-dispatch-request
    :surface 'serve
    :adapter 'pipe
    :path '("items")
    :metadata (list :eval (lambda () "do not cross"))))
  (should-error
   (clime-make-dispatch-input
    :name 'operation
    :value (lambda () "do not cross")
    :source 'envelope)))

(ert-deftest clime-test-dispatch/request-validates-surface-and-adapter ()
  "Surface and adapter are separate required symbols."
  (clime-test-dispatch--require-port)
  (let ((request (clime-make-dispatch-request
                  :surface 'serve
                  :adapter 'http
                  :path '("health"))))
    (should (eq (clime-dispatch-request-surface request) 'serve))
    (should (eq (clime-dispatch-request-adapter request) 'http)))
  (should-error
   (clime-make-dispatch-request :surface 'http :adapter 'serve :path '("x")))
  (should-error
   (clime-make-dispatch-request :surface 'serve :path '("x")))
  (should-error
   (clime-make-dispatch-request :adapter 'http :path '("x"))))

(ert-deftest clime-test-dispatch/request-carries-format-and-auth-context ()
  "The request record carries requested format and sanitized auth context."
  (clime-test-dispatch--require-port)
  (let ((request (clime-make-dispatch-request
                  :surface 'serve
                  :adapter 'http
                  :path '("reports" "summary")
                  :format 'json
                  :auth-context '(:principal "alice" :scheme bearer))))
    (should (eq (clime-dispatch-request-format request) 'json))
    (should (equal (clime-dispatch-request-auth-context request)
                   '(:principal "alice" :scheme bearer))))
  (should-error
   (clime-make-dispatch-request
    :surface 'serve
    :adapter 'http
    :path '("reports")
    :auth-context (list :principal "alice"
                        :callback (lambda () "do not cross")))))

(ert-deftest clime-test-dispatch/response-carries-domain-outcome-and-adapter-data ()
  "A dispatch response separates domain outcome from adapter wire metadata."
  (clime-test-dispatch--require-port)
  (let ((response (clime-make-dispatch-response
                   :outcome 'rejected
                   :error-type 'method-not-allowed
                   :error-message "Method not allowed"
                   :format 'json
                   :content-type "application/json"
                   :adapter-data '(:http-status 405
                                   :headers (("Allow" . "GET, POST"))))))
    (should (clime-dispatch-response-p response))
    (should (eq (clime-dispatch-response-outcome response) 'rejected))
    (should (equal (plist-get (clime-dispatch-response-adapter-data response)
                              :headers)
                   '(("Allow" . "GET, POST"))))))

(ert-deftest clime-test-dispatch/response-validates-outcome ()
  "Responses accept known dispatch outcomes and reject unknown ones."
  (clime-test-dispatch--require-port)
  (dolist (outcome '(completed not-found usage-error runtime-error rejected))
    (should (clime-dispatch-response-p
             (clime-make-dispatch-response :outcome outcome))))
  (should-error
   (clime-make-dispatch-response :outcome 'http-405)))

(ert-deftest clime-test-dispatch/surface-denied-path-never-falls-through-to-arg ()
  "A known denied path returns not-found before positional route matching."
  (clime-test-dispatch--require-port)
  (let* ((parent-called nil)
         (private-called nil)
         (private (clime-make-command
                   :name "private" :surfaces '(mcp)
                   :handler (lambda (_ctx) (setq private-called t))))
         (app (clime-make-app
               :name "test" :version "1"
               :args (list (clime-make-arg :name 'value :required nil))
               :handler (lambda (_ctx) (setq parent-called t))
               :children `(("private" . ,private))))
         (request (clime-make-dispatch-request
                   :surface 'serve :adapter 'http :path '("private")))
         (response (clime-dispatch-run-request app request nil)))
    (should (eq (clime-dispatch-response-outcome response) 'not-found))
    (should-not parent-called)
    (should-not private-called)))

(eval '(clime-app clime-test--dispatch-core-app
         (clime-command ok
           (clime-arg id :optional :default "default-id" :help "ID")
           (clime-option name ("--name") :default "default" :help "Name")
           (clime-option tag ("--tag") :default "default" :help "Tag")
           (clime-handler (ctx)
             (list :id (clime-ctx-get ctx 'id)
                   :name (clime-ctx-get ctx 'name)
                   :tag (clime-ctx-get ctx 'tag))))
         (clime-command usage
           (clime-option required ("--required") :required t :help "Required")
           (clime-handler (_ctx) "unreachable"))
         (clime-command boom
           (clime-handler (_ctx) (error "dispatch boom"))))
      t)

(defun clime-test-dispatch--run-with-events (path &optional inputs)
  "Dispatch HTTP PATH with INPUTS and return (RESPONSE . EVENTS)."
  (let ((events nil))
    (setf (clime-app-on-invocation clime-test--dispatch-core-app)
          (list (lambda (event) (push event events))))
    (let ((response
           (clime-dispatch-run-request
            clime-test--dispatch-core-app
            (clime-make-dispatch-request
             :surface 'serve :adapter 'http :path path :inputs inputs)
            nil)))
      (cons response events))))

(ert-deftest clime-test-dispatch/lifecycle-correlates-starts-with-all-terminal-paths ()
  "Serve starts pair with completed, rejected, and route-miss terminals."
  (dolist (case '(("ok" nil completed)
                  ("missing" nil not-found)
                  ("ok" rejected rejected)))
    (pcase-let ((`(,path ,reject-p ,terminal-phase) case))
      (let ((events nil)
            (request (clime-make-dispatch-request
                      :surface 'serve :adapter 'http :path (list path))))
        (setf (clime-app-on-lifecycle clime-test--dispatch-core-app)
              (list (lambda (event) (push event events))))
        (clime-dispatch-run-request
         clime-test--dispatch-core-app request nil
         :policy-rejector
         (and reject-p
              (lambda (_node _request _format _start-time)
                (clime-make-dispatch-response
                 :outcome 'rejected :body "blocked"
                 :adapter-data '(:http-status 403)))))
        (setq events (nreverse events))
        (should (= 2 (length events)))
        (should (eq 'started (clime-invocation-event-phase (car events))))
        (should (eq terminal-phase
                    (clime-invocation-event-phase (cadr events))))
        (should (equal (clime-invocation-event-invocation-id (car events))
                       (clime-invocation-event-invocation-id (cadr events)))))))
  (setf (clime-app-on-lifecycle clime-test--dispatch-core-app) nil))

(ert-deftest clime-test-dispatch/lifecycle-aborts-before-worker-visible-errors ()
  "An error that escapes dispatch still closes the lifecycle pair first."
  (let ((events nil)
        (request (clime-make-dispatch-request
                  :surface 'serve :adapter 'http :path '("ok"))))
    (unwind-protect
        (progn
          (setf (clime-app-on-lifecycle clime-test--dispatch-core-app)
                (list (lambda (event) (push event events))))
          (should-error
           (clime-dispatch-run-request
            clime-test--dispatch-core-app request nil
            :policy-rejector
            (lambda (&rest _ignored) (error "policy exploded"))))
          (setq events (nreverse events))
          (should (equal '(started aborted)
                         (mapcar #'clime-invocation-event-phase events)))
          (should (equal (clime-invocation-event-invocation-id (car events))
                         (clime-invocation-event-invocation-id (cadr events)))))
      (setf (clime-app-on-lifecycle clime-test--dispatch-core-app) nil))))

(ert-deftest clime-test-dispatch/completed-response-fires-final-event-once ()
  "A completed dispatch fires once with result, adapter, and final status."
  (unwind-protect
      (let* ((query-input (clime-make-dispatch-input
                           :name 'tag :value "query" :source 'query))
             (body-input (clime-make-dispatch-input
                          :name 'name :value "Ada" :source 'json-body))
             (result (clime-test-dispatch--run-with-events
                      '("ok" "42") (list query-input body-input)))
             (response (car result))
             (events (cdr result)))
        (should (eq 'completed (clime-dispatch-response-outcome response)))
        (should (= 1 (length events)))
        (let ((event (car events)))
          (should (eq 'completed (clime-invocation-event-phase event)))
          (should (eq 'http (clime-invocation-event-adapter event)))
          (should (= 200 (clime-invocation-event-response-status event)))
          (let ((provided (clime-invocation-event-provided-params event)))
            (should (= 6 (length provided)))
            (should (equal "42" (plist-get provided 'id)))
            (should (equal "Ada" (plist-get provided 'name)))
            (should (equal "query" (plist-get provided 'tag))))
          (should (clime-invocation-event-returned-p event))
          (should (equal '(:id "42" :name "Ada" :tag "query")
                         (clime-invocation-event-return-value event)))))
    (setf (clime-app-on-invocation clime-test--dispatch-core-app) nil)))

(ert-deftest clime-test-dispatch/error-responses-carry-final-http-status ()
  "Usage, runtime, and not-found events carry independent final statuses."
  (unwind-protect
      (dolist (case '((("usage") usage-error 400 nil)
                      (("boom") runtime-error 500 t)
                      (("missing") not-found 404 nil)))
        (pcase-let* ((`(,path ,outcome ,status ,invoked-p) case)
                     (result (clime-test-dispatch--run-with-events path))
                     (response (car result))
                     (events (cdr result))
                     (event (car events)))
          (should (eq outcome (clime-dispatch-response-outcome response)))
          (should (= 1 (length events)))
          (should (eq 'http (clime-invocation-event-adapter event)))
          (should (= status (clime-invocation-event-response-status event)))
          (should (eq invoked-p
                      (not (null
                            (clime-invocation-event-handler-invoked-p event)))))))
    (setf (clime-app-on-invocation clime-test--dispatch-core-app) nil)))

(ert-deftest clime-test-dispatch/core-policy-rejection-fires-once ()
  "The shared dispatch core owns rejected event emission exactly once."
  (clime-test-dispatch--require-port)
  (should (fboundp 'clime-dispatch-run-request))
  (let ((events nil)
        (hook-calls 0))
    (unwind-protect
        (progn
          (setf (clime-app-on-invocation clime-test--dispatch-core-app)
                (list (lambda (ev) (push ev events))))
          (let* ((request
                  (clime-make-dispatch-request
                   :surface 'serve
                   :adapter 'http
                   :path '("ok")
                   :metadata '(:method GET)))
                 (rejector
                  (lambda (_node _request _fmt _start-time)
                    (cl-incf hook-calls)
                    (clime-make-dispatch-response
                     :outcome 'rejected
                     :body "blocked\n"
                     :content-type "text/plain; charset=utf-8"
                     :error-type 'test-policy-rejected
                     :error-message "blocked"
                     :adapter-data '(:http-status 405))))
                 (response
                  (clime-dispatch-run-request
                   clime-test--dispatch-core-app request nil
                   :policy-rejector rejector)))
            (should (= 1 hook-calls))
            (should (clime-dispatch-response-p response))
            (should (eq (clime-dispatch-response-outcome response) 'rejected))
            (should (equal (plist-get (clime-dispatch-response-adapter-data response)
                                      :http-status)
                           405))
            (should (= 1 (length events)))
            (let ((event (car events)))
              (should (eq (clime-invocation-event-phase event) 'rejected))
              (should (eq (clime-invocation-event-adapter event) 'http))
              (should (= (clime-invocation-event-response-status event) 405))
              (should (eq (clime-invocation-event-error-type event)
                          'test-policy-rejected))
              (should (equal (clime-invocation-event-error-message event)
                             "blocked")))))
      (setf (clime-app-on-invocation clime-test--dispatch-core-app) nil))))

(provide 'clime-dispatch-tests)
;;; clime-dispatch-tests.el ends here
