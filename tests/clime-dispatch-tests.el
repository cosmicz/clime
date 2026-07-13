;;; clime-dispatch-tests.el --- Tests for transport-neutral dispatch port  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the transport-neutral dispatch request/response contract.
;; These are intentionally red until the dispatch port is implemented.

;;; Code:

(require 'ert)
(require 'clime-core)

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

(provide 'clime-dispatch-tests)
;;; clime-dispatch-tests.el ends here
