;;; clime-mcp-tests.el --- Tests for the MCP tools engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure protocol tests for the transport-neutral MCP 2025-11-25 tools
;; session engine.  No transport, Clime command tree, or live Emacs
;; introspection is involved.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'clime-mcp)

(defun clime-test-mcp--object (&rest members)
  "Return an equal-tested JSON object populated from MEMBERS."
  (let ((object (make-hash-table :test #'equal)))
    (while members
      (puthash (pop members) (pop members) object))
    object))

(defun clime-test-mcp--default-list (_cursor)
  "Return an empty tool-list page."
  (clime-test-mcp--object "tools" []))

(defun clime-test-mcp--default-call (_name _arguments)
  "Return a successful text tool result."
  (clime-test-mcp--object
   "content" (vector (clime-test-mcp--object
                       "type" "text" "text" "ok"))))

(cl-defun clime-test-mcp--session
    (&key (versions '("2025-11-25" "2025-06-18"))
          (list-tools #'clime-test-mcp--default-list)
          (call-tool #'clime-test-mcp--default-call)
          (instructions "Use only the tools explicitly listed by this server."))
  "Return a test MCP session with optional port overrides."
  (clime-mcp-make-session
   :supported-versions versions
   :server-info (clime-test-mcp--object
                 "name" "clime-test" "version" "1.0.0")
   :instructions instructions
   :list-tools list-tools
   :call-tool call-tool))

(defun clime-test-mcp--request (id method &optional params params-present-p)
  "Return a decoded request for ID and METHOD.
Include PARAMS when PARAMS-PRESENT-P is non-nil."
  (let ((request (clime-test-mcp--object
                  "jsonrpc" "2.0" "id" id "method" method)))
    (when params-present-p
      (puthash "params" params request))
    request))

(defun clime-test-mcp--notification (method &optional params params-present-p)
  "Return a decoded notification for METHOD and optional PARAMS."
  (let ((notification (clime-test-mcp--object
                       "jsonrpc" "2.0" "method" method)))
    (when params-present-p
      (puthash "params" params notification))
    notification))

(defun clime-test-mcp--initialize-params (&optional version)
  "Return valid initialize params for VERSION."
  (clime-test-mcp--object
   "protocolVersion" (or version "2025-11-25")
   "capabilities" (clime-test-mcp--object)
   "clientInfo" (clime-test-mcp--object
                 "name" "test-client" "version" "1.0.0")))

(defun clime-test-mcp--initialize (session id &optional version)
  "Initialize SESSION using ID and optional VERSION."
  (clime-mcp-handle
   session
   (clime-test-mcp--request
    id "initialize" (clime-test-mcp--initialize-params version) t)))

(defun clime-test-mcp--operating-session ()
  "Return a session in the operating state."
  (let ((session (clime-test-mcp--session)))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    session))

(defun clime-test-mcp--error-code (response)
  "Return the JSON-RPC error code from RESPONSE."
  (gethash "code" (gethash "error" response)))

(ert-deftest clime-test-mcp/initialize-negotiates-tools-only-session ()
  "Initialization returns configured identity and only the tools capability."
  (let* ((session (clime-test-mcp--session))
         (response (clime-test-mcp--initialize session "init"))
         (result (gethash "result" response))
         (capabilities (gethash "capabilities" result)))
    (should (equal (gethash "id" response) "init"))
    (should (equal (gethash "protocolVersion" result) "2025-11-25"))
    (should (equal (gethash "name" (gethash "serverInfo" result))
                   "clime-test"))
    (should (<= (length (gethash "instructions" result)) 512))
    (should (= (hash-table-count capabilities) 1))
    (should (hash-table-p (gethash "tools" capabilities)))
    (should (eq (clime-mcp-session-state session) 'awaiting-initialized))))

(ert-deftest clime-test-mcp/initialize-selects-preferred-supported-version ()
  "An unsupported requested version receives the preferred server version."
  (let* ((session (clime-test-mcp--session))
         (response (clime-test-mcp--initialize session 1 "2099-01-01")))
    (should (equal (gethash "protocolVersion" (gethash "result" response))
                   "2025-11-25"))))

(ert-deftest clime-test-mcp/initialize-validates-required-params ()
  "Malformed initialization is invalid params and does not advance state."
  (let* ((session (clime-test-mcp--session))
         (response
          (clime-mcp-handle
           session
           (clime-test-mcp--request
            1 "initialize"
            (clime-test-mcp--object "protocolVersion" "2025-11-25") t))))
    (should (= (clime-test-mcp--error-code response) -32602))
    (should (eq (clime-mcp-session-state session) 'new))))

(ert-deftest clime-test-mcp/lifecycle-gates-tools-until-initialized-notification ()
  "Tool operations require the initialized notification."
  (let ((session (clime-test-mcp--session)))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session (clime-test-mcp--request 1 "tools/list")))
               -32600))
    (clime-test-mcp--initialize session 2)
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session (clime-test-mcp--request 3 "tools/list")))
               -32600))
    (should-not
     (clime-mcp-handle
      session (clime-test-mcp--notification "notifications/initialized")))
    (should (eq (clime-mcp-session-state session) 'operating))
    (should (vectorp
             (gethash "tools"
                      (gethash "result"
                               (clime-mcp-handle
                                session
                                (clime-test-mcp--request 4 "tools/list"))))))))

(ert-deftest clime-test-mcp/ping-works-in-every-open-state ()
  "Ping returns an empty object before and after initialization."
  (let ((session (clime-test-mcp--session)))
    (dolist (setup (list nil 'initialize 'operate))
      (pcase setup
        ('initialize (clime-test-mcp--initialize session 2))
        ('operate
         (clime-mcp-handle
          session (clime-test-mcp--notification "notifications/initialized"))))
      (let ((result (gethash "result"
                             (clime-mcp-handle
                              session
                              (clime-test-mcp--request
                               (pcase setup
                                 ('initialize 3)
                                 ('operate 4)
                                 (_ 1))
                               "ping")))))
        (should (hash-table-p result))
        (should (= (hash-table-count result) 0))))))

(ert-deftest clime-test-mcp/request-ids-are-non-null-unique-and-preserved ()
  "String, integer, and zero IDs work; null and duplicate IDs do not."
  (let ((session (clime-test-mcp--session)))
    (should (= (gethash "id"
                        (clime-mcp-handle
                         session (clime-test-mcp--request 0 "ping")))
               0))
    (should (equal (gethash "id"
                            (clime-mcp-handle
                             session
                             (clime-test-mcp--request "p" "ping")))
                   "p"))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session (clime-test-mcp--request 0 "ping")))
               -32600))
    (let ((response
           (clime-mcp-handle
            session
            (clime-test-mcp--request clime-mcp-json-null "ping"))))
      (should (= (clime-test-mcp--error-code response) -32600))
      (should (eq (gethash "id" response) clime-mcp-json-null)))))

(ert-deftest clime-test-mcp/notifications-never-respond-or-run-request-methods ()
  "Unknown, malformed, and request-method notifications are side-effect free."
  (let ((calls 0)
        (session
         (clime-test-mcp--session
          :call-tool (lambda (_name _arguments)
                       (cl-incf calls)
                       (clime-test-mcp--default-call nil nil)))))
    (should-not (clime-mcp-handle
                 session (clime-test-mcp--notification "unknown/note")))
    (should-not (clime-mcp-handle session (clime-test-mcp--object
                                           "jsonrpc" "2.0")))
    (should-not (clime-mcp-handle
                 session (clime-test-mcp--notification "tools/call")))
    (should (= calls 0))))

(ert-deftest clime-test-mcp/invalid-envelopes-and-methods-use-standard-errors ()
  "Decoded invalid requests use stable JSON-RPC error categories."
  (let ((session (clime-test-mcp--session)))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle session []))
               -32600))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session (clime-test-mcp--object
                          "jsonrpc" "1.0" "id" 1 "method" "ping")))
               -32600))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session (clime-test-mcp--request 2 "not/a-method")))
               -32601))))

(ert-deftest clime-test-mcp/unexpected-client-response-is-ignored ()
  "A client response never causes a response loop."
  (let ((session (clime-test-mcp--session)))
    (should-not
     (clime-mcp-handle
      session (clime-test-mcp--object
               "jsonrpc" "2.0" "id" 1
               "result" (clime-test-mcp--object))))))

(ert-deftest clime-test-mcp/tools-list-validates-cursor-and-port-result ()
  "Tool listing passes opaque cursors and rejects malformed data."
  (let* ((seen-cursor nil)
         (session
          (clime-test-mcp--session
           :list-tools
           (lambda (cursor)
             (setq seen-cursor cursor)
             (clime-test-mcp--object "tools" [] "nextCursor" "next")))))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    (let* ((response
            (clime-mcp-handle
             session
             (clime-test-mcp--request
              2 "tools/list" (clime-test-mcp--object "cursor" "opaque") t)))
           (result (gethash "result" response)))
      (should (equal seen-cursor "opaque"))
      (should (equal (gethash "nextCursor" result) "next")))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session
                 (clime-test-mcp--request
                  3 "tools/list" (clime-test-mcp--object "cursor" 7) t)))
               -32602)))
  (let ((session
         (clime-test-mcp--session :list-tools (lambda (_cursor) "bad"))))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session (clime-test-mcp--request 2 "tools/list")))
               -32603))))

(ert-deftest clime-test-mcp/tools-call-validates-arguments-and-ignores-task-metadata ()
  "Absent arguments become an object and task metadata is ignored in V0."
  (let* ((seen nil)
         (session
          (clime-test-mcp--session
           :call-tool
           (lambda (name arguments)
             (setq seen (list name arguments))
             (clime-test-mcp--default-call name arguments)))))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    (should (gethash
             "result"
             (clime-mcp-handle
              session
              (clime-test-mcp--request
               2 "tools/call"
               (clime-test-mcp--object
                "name" "inspect" "task" "ignored-even-if-malformed") t))))
    (should (equal (car seen) "inspect"))
    (should (hash-table-p (cadr seen)))
    (should (= (hash-table-count (cadr seen)) 0))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session
                 (clime-test-mcp--request
                  3 "tools/call"
                  (clime-test-mcp--object
                   "name" "inspect" "arguments" '("not" "object")) t)))
               -32602))))

(ert-deftest clime-test-mcp/tools-call-separates-protocol-and-tool-errors ()
  "Unknown tools are protocol errors; execution errors remain results."
  (let ((session
         (clime-test-mcp--session
          :call-tool
          (lambda (name _arguments)
            (if (equal name "missing")
                (signal 'clime-mcp-unknown-tool nil)
              (clime-test-mcp--object
               "content" (vector (clime-test-mcp--object
                                   "type" "text" "text" "retry"))
               "isError" t))))))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session
                 (clime-test-mcp--request
                  2 "tools/call"
                  (clime-test-mcp--object "name" "missing") t)))
               -32602))
    (let ((result
           (gethash
            "result"
            (clime-mcp-handle
             session
             (clime-test-mcp--request
              3 "tools/call" (clime-test-mcp--object "name" "fails") t)))))
      (should (eq (gethash "isError" result) t)))))

(ert-deftest clime-test-mcp/port-failures-are-sanitized-internal-errors ()
  "Unexpected port conditions do not expose internal messages."
  (let ((session
         (clime-test-mcp--session
          :call-tool (lambda (_name _arguments)
                       (error "secret /tmp/internal-path")))))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    (let* ((response
            (clime-mcp-handle
             session
             (clime-test-mcp--request
              2 "tools/call" (clime-test-mcp--object "name" "boom") t)))
           (error-object (gethash "error" response)))
      (should (= (gethash "code" error-object) -32603))
      (should-not (string-match-p "secret\\|/tmp"
                                  (gethash "message" error-object))))))

(ert-deftest clime-test-mcp/malformed-call-result-is-an-internal-error ()
  "A call port cannot emit an empty or structurally invalid result."
  (let ((session
         (clime-test-mcp--session
          :call-tool
          (lambda (_name _arguments)
            (clime-test-mcp--object "content" [])))))
    (clime-test-mcp--initialize session 1)
    (clime-mcp-handle
     session (clime-test-mcp--notification "notifications/initialized"))
    (should (= (clime-test-mcp--error-code
                (clime-mcp-handle
                 session
                 (clime-test-mcp--request
                  2 "tools/call" (clime-test-mcp--object "name" "bad") t)))
               -32603))))

(ert-deftest clime-test-mcp/cancellation-is-response-free-and-cannot-cancel-initialize ()
  "Cancellation notifications are ignored in V0."
  (let ((session (clime-test-mcp--session)))
    (should-not
     (clime-mcp-handle
      session
      (clime-test-mcp--notification
       "notifications/cancelled"
       (clime-test-mcp--object "requestId" 1) t)))
    (should (eq (clime-mcp-session-state session) 'new))
    (should (gethash "result" (clime-test-mcp--initialize session 1)))))

(ert-deftest clime-test-mcp/close-is-terminal-and-releases-request-ids ()
  "Adapter-owned close makes the session terminal and clears ID state."
  (let ((session (clime-test-mcp--session)))
    (clime-mcp-handle session (clime-test-mcp--request 1 "ping"))
    (should (= (hash-table-count (clime-mcp-session-used-ids session)) 1))
    (clime-mcp-close session)
    (should (eq (clime-mcp-session-state session) 'closed))
    (should (= (hash-table-count (clime-mcp-session-used-ids session)) 0))
    (should-error
     (clime-mcp-handle session (clime-test-mcp--request 2 "ping"))
     :type 'clime-mcp-session-closed)))

(provide 'clime-mcp-tests)
;;; clime-mcp-tests.el ends here
