;;; clime-serve.el --- HTTP surface for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; HTTP serve surface for clime apps.  Same shape as clime-invoke:
;; prepare tree once at startup (pass 1 + setup/config), deep copy per
;; request, walk URL path to resolve command, populate values from
;; request params, finalize, execute.
;;
;; Depends on `web-server' (GNU ELPA) for the network layer.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'subr-x)
(require 'clime-core)
(require 'clime-contract)
(require 'clime-parse)
(require 'clime-dispatch)
(require 'clime-run)
(require 'clime-output)

;; Vendored web-server lives under lib/web-server/ alongside this file.
;; Add it to the load-path so users don't have to wire it up themselves.
(declare-function ws-start "web-server" (handlers port &optional log-buffer &rest extra))
(declare-function ws-response-header "web-server" (process code &rest headers))
(declare-function ws-stop "web-server" (server))

(defun clime-serve--ensure-web-server ()
  "Ensure `web-server' is available, adding vendored path if needed."
  (unless (featurep 'web-server)
    (let* ((here (file-name-directory (or load-file-name
                                         buffer-file-name
                                         (locate-library "clime-serve"))))
           (ws-dir (and here (expand-file-name "lib/web-server" here))))
      (when (and ws-dir (file-directory-p ws-dir))
        (add-to-list 'load-path ws-dir)))
    (require 'web-server)))

;;; ─── Customization ──────────────────────────────────────────────────────

(defcustom clime-serve-default-port 8080
  "Default port for `clime-serve'."
  :type 'integer
  :group 'clime)

;;; ─── State ──────────────────────────────────────────────────────────────

(defvar clime-serve--servers nil
  "Alist of (PORT . SERVER-PROCESS) for active servers.")

;;; ─── Query String / JSON Body Parsing ───────────────────────────────────

(defun clime-serve--parse-query-string (qs)
  "Parse query string QS into an alist of (KEY . VALUE) pairs.
VALUE is nil for bare keys (no `=').  Returns nil for empty/nil QS."
  (when (and qs (not (string-empty-p qs)))
    (let ((pairs (split-string qs "&" t)))
      (mapcar (lambda (pair)
                (if (string-match-p "=" pair)
                    (let ((parts (split-string pair "=" t)))
                      (cons (url-unhex-string (car parts))
                            (url-unhex-string (cadr parts))))
                  (cons (url-unhex-string pair) t)))
              pairs))))

(defun clime-serve--parse-json-body (body)
  "Parse JSON BODY string into an alist of (KEY . VALUE) pairs.
Returns nil for empty/nil BODY."
  (when (and body (not (string-empty-p body)))
    (let* ((json-object-type 'alist)
           (json-key-type 'string)
           (parsed (json-read-from-string body)))
      (when (listp parsed)
        parsed))))

;;; ─── Path Walking ───────────────────────────────────────────────────────

(defun clime-serve--walk-path (tree segments)
  "Compatibility wrapper for `clime-dispatch-walk-path'."
  (condition-case err
      (clime-dispatch-walk-path tree segments)
    (clime-dispatch-not-found
     (signal 'clime-serve-not-found (cdr err)))))

(define-error 'clime-serve-not-found "Route not found" 'clime-dispatch-not-found)

;;; ─── Values Seeding ─────────────────────────────────────────────────────

(defun clime-serve--seed-values (values params)
  "Seed VALUES with request PARAMS alist.
PARAMS is ((KEY . VALUE) ...) where KEY is a string.
Values are set with source `user'.  Returns updated VALUES."
  (dolist (pair params)
    (let* ((key (car pair))
           (val (cdr pair))
           (sym (intern key)))
      ;; Bare query params (no =) arrive as nil or "" from
      ;; web-server; normalize to t so they act as boolean flags
      (when (or (null val) (equal val ""))
        (setq val t))
      (setq values (clime-values-set values sym val 'user))))
  values)

(defun clime-serve--seed-dispatch-inputs (values inputs)
  "Compatibility wrapper for `clime-dispatch-seed-inputs'."
  (clime-dispatch-seed-inputs values inputs))

(defun clime-serve--alist-inputs (params source)
  "Convert PARAMS alist to dispatch inputs with SOURCE provenance."
  (mapcar (lambda (pair)
            (clime-make-dispatch-input
             :name (intern (car pair))
             :value (cdr pair)
             :source source))
          params))

(defun clime-serve--normalize-media-type (content-type)
  "Return lower-case media type from CONTENT-TYPE, ignoring parameters."
  (when (and content-type (stringp content-type)
             (not (string-empty-p content-type)))
    (downcase (string-trim (car (split-string content-type ";" t))))))

(defun clime-serve--method-string (method)
  "Return upper-case string form of METHOD."
  (upcase (if (symbolp method) (symbol-name method) (format "%s" method))))

(defun clime-serve--method-symbol (method)
  "Return upper-case symbol form of METHOD."
  (intern (clime-serve--method-string method)))

(defun clime-serve--policy-key-present-p (policy key)
  "Return non-nil when POLICY explicitly contains KEY."
  (and (listp policy) (plist-member policy key)))

(defconst clime-serve--http-policy-keys
  '(:methods :param-sources :content-types :max-body-bytes :same-origin)
  "Known HTTP adapter policy keys.")

(defun clime-serve--policy-node-name (node)
  "Return NODE name for validation errors."
  (or (clime-node-name node) "<unnamed>"))

(defun clime-serve--validate-http-policy-keys (node policy)
  "Validate raw local HTTP POLICY keys for NODE."
  (unless (listp policy)
    (error "clime-serve: malformed HTTP adapter policy on `%s'"
           (clime-serve--policy-node-name node)))
  (let ((rest policy))
    (while rest
      (unless (and (consp rest) (consp (cdr rest)))
        (error "clime-serve: malformed HTTP adapter policy on `%s'"
               (clime-serve--policy-node-name node)))
      (let ((key (car rest)))
        (unless (memq key clime-serve--http-policy-keys)
          (error "clime-serve: unknown HTTP adapter policy key %S on `%s'"
                 key (clime-serve--policy-node-name node))))
      (setq rest (cddr rest)))))

(defun clime-serve--validate-adapter-policies (node)
  "Validate raw local adapter policies for NODE."
  (dolist (entry (clime-node-adapter-policies node))
    (unless (consp entry)
      (error "clime-serve: malformed adapter policy entry on `%s'"
             (clime-serve--policy-node-name node)))
    (let ((adapter (car entry))
          (policy (cdr entry)))
      (unless (memq adapter clime-dispatch-adapters)
        (error "clime-serve: unknown adapter policy `%s' on `%s'"
               adapter (clime-serve--policy-node-name node)))
      (when (eq adapter 'http)
        (clime-serve--validate-http-policy-keys node policy)))))

(defun clime-serve--intersect-symbol-list (left right)
  "Return intersection of LEFT and RIGHT preserving LEFT order."
  (cl-remove-if-not (lambda (item) (memq item right)) left))

(defun clime-serve--intersect-string-list (left right)
  "Return case-insensitive media-type intersection preserving LEFT order."
  (cl-remove-if-not
   (lambda (item)
     (member (clime-serve--normalize-media-type item)
             (mapcar #'clime-serve--normalize-media-type right)))
   left))

(defun clime-serve--merge-http-policy (effective local)
  "Restrictively merge LOCAL HTTP policy into EFFECTIVE."
  (let ((result (copy-sequence effective)))
    (when (clime-serve--policy-key-present-p local :methods)
      (let ((methods (mapcar #'clime-serve--method-symbol
                             (plist-get local :methods))))
        (setq result
              (plist-put result :methods
                         (if (clime-serve--policy-key-present-p result :methods)
                             (clime-serve--intersect-symbol-list
                              (plist-get result :methods) methods)
                           methods)))))
    (when (clime-serve--policy-key-present-p local :param-sources)
      (let ((sources (plist-get local :param-sources)))
        (setq result
              (plist-put result :param-sources
                         (if (clime-serve--policy-key-present-p result :param-sources)
                             (clime-serve--intersect-symbol-list
                              (plist-get result :param-sources) sources)
                           sources)))))
    (when (clime-serve--policy-key-present-p local :content-types)
      (let ((types (plist-get local :content-types)))
        (setq result
              (plist-put result :content-types
                         (if (clime-serve--policy-key-present-p result :content-types)
                             (clime-serve--intersect-string-list
                              (plist-get result :content-types) types)
                           types)))))
    (when (clime-serve--policy-key-present-p local :max-body-bytes)
      (let ((limit (plist-get local :max-body-bytes)))
        (setq result
              (plist-put result :max-body-bytes
                         (if (clime-serve--policy-key-present-p result :max-body-bytes)
                             (min (plist-get result :max-body-bytes) limit)
                           limit)))))
    (when (clime-serve--policy-key-present-p local :same-origin)
      (setq result
            (plist-put result :same-origin
                       (or (plist-get result :same-origin)
                           (plist-get local :same-origin)))))
    result))

(defun clime-serve--effective-http-policy (node)
  "Return effective HTTP adapter policy for NODE."
  (let ((effective nil))
    (dolist (scope (append (clime-node-ancestors node) (list node)))
      (when-let ((policy (cdr (assq 'http (clime-node-adapter-policies scope)))))
        (setq effective (clime-serve--merge-http-policy effective policy))))
    effective))

(defun clime-serve--validate-http-policy (node)
  "Validate NODE's effective HTTP policy, if any."
  (clime-serve--validate-adapter-policies node)
  (let ((policy (clime-serve--effective-http-policy node)))
    (when (and (clime-serve--policy-key-present-p policy :methods)
               (null (plist-get policy :methods)))
      (error "clime-serve: empty effective HTTP :methods on `%s'"
             (clime-node-name node)))
    (when (and (clime-serve--policy-key-present-p policy :param-sources)
               (null (plist-get policy :param-sources)))
      (error "clime-serve: empty effective HTTP :param-sources on `%s'"
             (clime-node-name node)))
    (when (and (clime-serve--policy-key-present-p policy :content-types)
               (null (plist-get policy :content-types)))
      (error "clime-serve: empty effective HTTP :content-types on `%s'"
             (clime-node-name node)))))

(defun clime-serve--validate-http-policies (node)
  "Validate effective HTTP policies across NODE's tree."
  (clime-serve--validate-http-policy node)
  (when (clime-group-p node)
    (dolist (entry (clime-group-children node))
      (clime-serve--validate-http-policies (cdr entry)))))

(defun clime-serve--request-input-sources (request)
  "Return unique input sources present in REQUEST."
  (delete-dups (mapcar #'clime-dispatch-input-source
                       (clime-dispatch-request-inputs request))))

(defun clime-serve--format-error-body (message fmt)
  "Render rejection MESSAGE for output format FMT."
  (if (and fmt (eq (clime-output-format-name fmt) 'json))
      (clime-json-encode `((error . ,message)))
    (concat message "\n")))

(defun clime-serve--policy-response
    (status message fmt &optional headers error-type)
  "Return an HTTP adapter policy rejection dispatch response."
  (clime-make-dispatch-response
   :outcome 'rejected
   :body (clime-serve--format-error-body message fmt)
   :format fmt
   :content-type (clime-serve--content-type-for-format fmt)
   :error-type (or error-type 'clime-serve-policy-rejected)
   :error-message message
   :adapter-data (list :http-status status :headers headers)))

(defun clime-serve--origin-allowed-p (request)
  "Return non-nil when REQUEST satisfies same-origin browser checks."
  (let* ((metadata (clime-dispatch-request-metadata request))
         (origin (plist-get metadata :origin))
         (fetch-site (plist-get metadata :sec-fetch-site))
         (server-origin (plist-get metadata :server-origin)))
    (and (not (equal origin "null"))
         (not (member fetch-site '("cross-site" "same-site")))
         (or (null origin)
             (and server-origin (string= origin server-origin)))
         (or (null fetch-site)
             (member fetch-site '("same-origin" "none"))))))

(defun clime-serve--policy-rejection (policy request fmt)
  "Return rejection response if POLICY rejects REQUEST, else nil."
  (let* ((metadata (clime-dispatch-request-metadata request))
         (method (clime-serve--method-symbol (plist-get metadata :method)))
         (body-bytes (or (plist-get metadata :body-bytes) 0))
         (content-type (clime-serve--normalize-media-type
                        (plist-get metadata :content-type)))
         (sources (clime-serve--request-input-sources request)))
    (cond
     ((and (clime-serve--policy-key-present-p policy :param-sources)
           (memq 'json-body (plist-get policy :param-sources))
           (plist-get metadata :json-error))
      (clime-serve--policy-response
       400 (plist-get metadata :json-error) fmt nil
       'clime-serve-malformed-json))
     ((and (clime-serve--policy-key-present-p policy :methods)
           (not (memq method (plist-get policy :methods))))
      (let ((allow (mapconcat #'clime-serve--method-string
                              (plist-get policy :methods) ", ")))
        (clime-serve--policy-response
         405 "Method Not Allowed" fmt (list (cons "Allow" allow))
         'clime-serve-method-not-allowed)))
     ((and (clime-serve--policy-key-present-p policy :max-body-bytes)
           (> body-bytes (plist-get policy :max-body-bytes)))
      (clime-serve--policy-response
       413 "Request Entity Too Large" fmt nil
       'clime-serve-request-too-large))
     ((and (clime-serve--policy-key-present-p policy :content-types)
           sources
           (not (member content-type
                        (mapcar #'clime-serve--normalize-media-type
                                (plist-get policy :content-types)))))
      (clime-serve--policy-response
       415 "Unsupported Media Type" fmt nil
       'clime-serve-unsupported-media-type))
     ((and (clime-serve--policy-key-present-p policy :param-sources)
           (or (cl-set-difference sources (plist-get policy :param-sources))
               (> (length sources) 1)))
      (clime-serve--policy-response
       400 "Forbidden request parameter source" fmt nil
       'clime-serve-forbidden-param-source))
     ((and (plist-get policy :same-origin)
           (not (clime-serve--origin-allowed-p request)))
      (clime-serve--policy-response
       403 "Forbidden origin" fmt nil
       'clime-serve-forbidden-origin))
     (t nil))))

;;; ─── Format Helpers ─────────────────────────────────────────────────────

(defvaralias 'clime-serve--default-json-format
  'clime-dispatch--default-json-format
  "Fallback JSON `clime-output-format' for `_api' introspection endpoints.")

(defun clime-serve--content-type-for-format (fmt)
  "Compatibility wrapper for `clime-dispatch-content-type-for-format'."
  (clime-dispatch-content-type-for-format fmt))

(defun clime-serve--extract-suffix (segments app)
  "Compatibility wrapper for `clime-dispatch-extract-suffix'."
  (clime-dispatch-extract-suffix segments app))

;;; ─── Dispatch ───────────────────────────────────────────────────────────

(defun clime-serve--legacy-request (segments params)
  "Build a legacy HTTP dispatch request from SEGMENTS and PARAMS."
  (clime-make-dispatch-request
   :surface 'serve
   :adapter 'http
   :path segments
   :inputs (clime-serve--alist-inputs params 'query)
   :metadata (list :method 'GET :body-bytes 0)))

(defun clime-serve--dispatch (app segments params &optional default-format)
  "Dispatch a legacy request to APP.
SEGMENTS is a list of URL path segments (strings).  PARAMS is an alist
of (KEY . VALUE) from query string or body.  New HTTP adapter code should
prefer `clime-serve--dispatch-request'."
  (clime-serve--dispatch-request
   app (clime-serve--legacy-request segments params) default-format))

(defun clime-serve--http-policy-rejection (node request fmt _start-time)
  "Return HTTP adapter rejection for NODE and REQUEST, or nil."
  (clime-serve--policy-rejection
   (clime-serve--effective-http-policy node) request fmt))

(defun clime-serve--dispatch-response-plist (response)
  "Convert dispatch RESPONSE to the legacy serve response plist."
  (let ((adapter-data (clime-dispatch-response-adapter-data response)))
    (list :status (clime-dispatch-response-status response)
          :body (or (clime-dispatch-response-body response) "")
          :content-type (or (clime-dispatch-response-content-type response)
                            "text/plain; charset=utf-8")
          :headers (plist-get adapter-data :headers))))

(defun clime-serve--dispatch-request (app request &optional default-format)
  "Dispatch normalized REQUEST to APP.
DEFAULT-FORMAT, when non-nil, is a `clime-output-format' to use when
no path suffix overrides the format.
Returns plist (:status N :body STRING :content-type STRING :headers ALIST)."
  (clime-serve--dispatch-response-plist
   (clime-dispatch-run-request
    app request default-format
    :policy-rejector #'clime-serve--http-policy-rejection
    :default-json-format clime-serve--default-json-format)))

;;; ─── JSON Serialization ─────────────────────────────────────────────────

(defconst clime-serve--contract-policy
  '(:surface serve :tree-mode prepared :visibility visible :value-mode declared)
  "Explicit declaration policy for HTTP introspection.")

(defconst clime-serve--detail-contract-policy
  '(:surface serve :tree-mode prepared :visibility all :value-mode declared)
  "Explicit lookup policy for HTTP command detail.

The command listing omits hidden nodes, while the established detail endpoint
allows a hidden command that remains serve-eligible to be inspected directly.")

(defun clime-serve--contract-type-name (type policy)
  "Return HTTP's legacy type name for declaration TYPE under POLICY."
  (or (apply #'clime-contract-describe-type type policy) "function"))

(defun clime-serve--option-to-alist (option policy)
  "Serialize declaration OPTION to an alist for JSON encoding."
  (let ((result `(("name" . ,(symbol-name (clime-option-name option)))
                  ("flags" . ,(vconcat (clime-option-flags option)))
                  ("type" . ,(clime-serve--contract-type-name
                               (clime-option-type option) policy))
                  ("help" . ,(or (clime-option-help option) :json-false))
                  ("required" . ,(if (clime-option-required option)
                                      t :json-false)))))
    (when-let ((default (apply #'clime-contract-safe-default option policy)))
      (push `("default" . ,default) result))
    (let ((choices (apply #'clime-contract-safe-choices option policy)))
      (when (and choices (listp choices))
        (push `("choices" . ,(vconcat choices)) result)))
    (when (clime-option-negatable option)
      (push '("negatable" . t) result))
    (when (clime-option-deprecated option)
      (push `("deprecated" . ,(let ((d (clime-option-deprecated option)))
                                 (if (stringp d) d t)))
            result))
    result))

(defun clime-serve--arg-to-alist (arg policy)
  "Serialize declaration ARG to an alist for JSON encoding."
  (let ((result `(("name" . ,(symbol-name (clime-arg-name arg)))
                  ("type" . ,(clime-serve--contract-type-name
                               (clime-arg-type arg) policy))
                  ("help" . ,(or (clime-arg-help arg) :json-false))
                  ("required" . ,(if (clime-arg-required arg)
                                      t :json-false)))))
    (when-let ((default (apply #'clime-contract-safe-default arg policy)))
      (push `("default" . ,default) result))
    (let ((choices (apply #'clime-contract-safe-choices arg policy)))
      (when (and choices (listp choices))
        (push `("choices" . ,(vconcat choices)) result)))
    result))

(defun clime-serve--node-to-alist (entry policy nodes)
  "Render contract ENTRY from NODES as HTTP command metadata."
  (let* ((node (car entry))
         (is-group (clime-branch-p node))
         (result `(("name" . ,(clime-node-name node))
                   ("type" . ,(if is-group "group" "command"))
                   ("help" . ,(or (clime-node-help node) :json-false)))))
    ;; Aliases
    (when (clime-node-aliases node)
      (push `("aliases" . ,(vconcat (clime-node-aliases node))) result))
    ;; Projection policy has already removed hidden options.
    (let ((opts (apply #'clime-contract-options node policy)))
      (when opts
        (push `("options" . ,(vconcat
                               (mapcar (lambda (option)
                                         (clime-serve--option-to-alist option policy))
                                       opts)))
              result)))
    ;; Args
    (when (clime-node-args node)
      (push `("args" . ,(vconcat
                          (mapcar (lambda (arg)
                                    (clime-serve--arg-to-alist arg policy))
                                  (clime-node-args node))))
            result))
    ;; Surface and node visibility are also contract policy decisions.  The
    ;; hidden guard additionally preserves the legacy detail rendering rule:
    ;; a hidden node may be fetched directly, but its hidden descendants stay
    ;; omitted from that node's rendered children.
    (let ((children (and is-group
                         (delq nil (mapcar (lambda (child)
                                             (and (not (clime-node-hidden (cdr child)))
                                                  (assq (cdr child) nodes)))
                                           (clime-group-children node))))))
      (when children
        (push `("children" . ,(vconcat
                                (mapcar (lambda (child)
                                          (clime-serve--node-to-alist
                                           child policy nodes))
                                        children)))
              result)))
    ;; Deprecated
    (when (clime-node-deprecated node)
      (push `("deprecated" . ,(let ((d (clime-node-deprecated node)))
                                 (if (stringp d) d t)))
            result))
    ;; Examples
    (when (clime-node-examples node)
      (push `("examples" . ,(vconcat
                              (mapcar (lambda (ex)
                                        (if (consp ex)
                                            `(("invocation" . ,(car ex))
                                              ("description" . ,(cdr ex)))
                                          `(("invocation" . ,ex))))
                                      (clime-node-examples node))))
            result))
    result))

(defun clime-serve--command-contract (app)
  "Return APP's prepared, visible HTTP node traversal."
  (apply #'clime-contract-nodes app clime-serve--contract-policy))

;;; ─── API Handlers ──────────────────────────────────────────────────────

(defun clime-serve--routes-handler (ctx)
  "Handle CTX for /_routes as a plain text route listing."
  (let* ((app (clime-context-app ctx))
         (items (clime-serve--command-contract app)))
    (princ (string-join
            (mapcar
             (lambda (entry)
               (let ((command (car entry)))
                 (format "/%s  %s"
                         (string-join (cdr entry) "/")
                         (or (clime-node-help command) ""))))
             (cl-remove-if-not (lambda (entry)
                                 (and (clime-command-p (car entry))
                                      (clime-node-handler (car entry))))
                               items))
            "\n"))
    nil))

(defun clime-serve--api-meta-handler (ctx)
  "Handle CTX for /_api/meta as app metadata."
  (let ((app (clime-context-app ctx)))
    (clime-out `(("name" . ,(clime-node-name app))
                 ("version" . ,(or (clime-app-version app) :json-false))
                 ("help" . ,(or (clime-node-help app) :json-false))))))

(defun clime-serve--api-commands-handler (ctx)
  "Handle CTX for /_api/commands as full tree or single command detail."
  (let* ((app (clime-context-app ctx))
         (path-segs (clime-ctx-get ctx 'path)))
    (if path-segs
        (let ((detail-nodes (apply #'clime-contract-nodes app
                                   clime-serve--detail-contract-policy)))
          (if-let* ((found (apply #'clime-contract-find app path-segs
                                  clime-serve--detail-contract-policy))
                    (entry (cl-find (cdr found) detail-nodes
                                    :key #'cdr :test #'equal)))
              (clime-out (clime-serve--node-to-alist entry
                                                      clime-serve--contract-policy
                                                      detail-nodes))
            ;; throw bypasses clime-run--execute's condition-case;
            ;; dispatch catches it and returns 404
            (throw 'clime-dispatch--not-found
                   (format "Unknown command path: %s"
                           (string-join path-segs "/")))))
      (let* ((nodes (clime-serve--command-contract app))
             (children (delq nil (mapcar (lambda (entry)
                                           (assq (cdr entry) nodes))
                                         (clime-group-children (caar nodes))))))
        (clime-out `(("commands" . ,(vconcat
                                      (mapcar (lambda (entry)
                                                (clime-serve--node-to-alist
                                                 entry clime-serve--contract-policy
                                                 nodes))
                                              children)))))))))

;;; ─── API Command Injection ─────────────────────────────────────────────

(defun clime-serve--inject-api-commands (app)
  "Inject hidden API commands into APP's children for introspection routing.
Idempotent — does nothing if `_api' is already a child."
  (unless (assoc "_api" (clime-group-children app))
    (let* ((routes-cmd
          (clime-make-command
           :name "_routes"
           :help "Plain text route listing"
           :hidden t
           :surfaces '(serve)
           :handler #'clime-serve--routes-handler))
         (meta-cmd
          (clime-make-command
           :name "meta"
           :help "App metadata"
           :hidden t
           :surfaces '(serve)
           :handler #'clime-serve--api-meta-handler))
         (commands-cmd
          (clime-make-command
           :name "commands"
           :help "Command tree"
           :hidden t
           :surfaces '(serve)
           :args (list (clime-arg--create :name 'path :nargs :rest))
           :handler #'clime-serve--api-commands-handler))
         (api-group
          (clime-group--create
           :name "_api"
           :help "API endpoints"
           :hidden t
           :surfaces '(serve)
           ;; Auto-declare json so introspection endpoints default to JSON
           ;; even when the app registers no output formats.
           :output-formats (list clime-serve--default-json-format)
           :children (list (cons "meta" meta-cmd)
                           (cons "commands" commands-cmd)))))
    ;; Set parent refs
    (setf (clime-node-parent routes-cmd) app)
    (setf (clime-node-parent api-group) app)
    (setf (clime-node-parent meta-cmd) api-group)
    (setf (clime-node-parent commands-cmd) api-group)
    ;; Append to app children (after user commands)
    (setf (clime-group-children app)
          (append (clime-group-children app)
                  (list (cons "_routes" routes-cmd)
                        (cons "_api" api-group)))))))

;;; ─── Server Lifecycle ───────────────────────────────────────────────────

(defun clime-serve--resolve-format (app format)
  "Resolve FORMAT to a `clime-output-format' registered on APP, or nil.
FORMAT is nil (no default — bare routes use the app's text output), a
`clime-output-format' struct (returned as-is), or a format-name symbol
\(resolved against APP's registered output formats).  A symbol naming no
registered format signals an error."
  (cond
   ((null format) nil)
   ((clime-output-format-p format) format)
   ((symbolp format)
    (or (cl-find format (clime-app-output-formats app)
                 :key #'clime-output-format-name)
        (error "clime-serve: no output format `%s' registered on app `%s'"
               format (clime-node-name app))))
   (t (error "clime-serve: invalid :default-format %S" format))))

(defun clime-serve--unauthorized (&optional fmt)
  "Return the 401 Unauthorized response plist, rendered in output format FMT.
FMT is a `clime-output-format' struct or nil.  When FMT is the `json'
format the body is a JSON object equivalent to the error envelope
\((error . \"unauthorized\")) with Content-Type application/json; otherwise
the body is plain text.  This lets a caller obtain a structured 401 purely
through the public `:default-format' argument of `clime-serve'."
  (if (and fmt (eq (clime-output-format-name fmt) 'json))
      (list :status 401
            :body (clime-json-encode '((error . "unauthorized")))
            :content-type (clime-serve--content-type-for-format fmt))
    (list :status 401
          :body "Unauthorized\n"
          :content-type "text/plain; charset=utf-8")))

(defun clime-serve--auth-gate (auth request-info &optional fmt)
  "Return a 401 response plist when AUTH rejects REQUEST-INFO, else nil.
AUTH is nil (no auth required) or a predicate of one argument, the
REQUEST-INFO plist (:method :path :headers :body).  When AUTH is non-nil
and the predicate returns nil the request is rejected with a 401 rendered
in output format FMT (see `clime-serve--unauthorized'); callers MUST skip
dispatch whenever this returns non-nil."
  (when (and auth (not (funcall auth request-info)))
    (clime-serve--unauthorized fmt)))

(defun clime-serve--send-response (process result)
  "Send RESULT to PROCESS as an HTTP response.
RESULT is a plist (:status N :body STRING :content-type STRING :headers ALIST)."
  (let ((status (plist-get result :status))
        (ct (or (plist-get result :content-type)
                "text/plain; charset=utf-8"))
        (response-body (or (plist-get result :body) ""))
        (headers (plist-get result :headers)))
    (apply #'ws-response-header process status
           (append headers
                   (list (cons "Content-Type" ct)
                         (cons "Content-Length"
                               (number-to-string
                                (string-bytes response-body))))))
    (process-send-string process response-body)))

(cl-defun clime-serve (app &key (port clime-serve-default-port)
                                (host "127.0.0.1")
                                default-format
                                auth)
  "Start an HTTP server for APP on PORT (default 8080) bound to HOST.
HOST defaults to 127.0.0.1 (localhost only).

DEFAULT-FORMAT, when non-nil, selects the output format for bare routes
that carry no `.<ext>' suffix.  It is a registered output-format name (a
symbol, e.g. `json') or a `clime-output-format' struct, resolved against
APP's formats via `clime-serve--resolve-format'.  Without it, bare routes
use the app's default (text) output; a `.<ext>' suffix still overrides.

AUTH, when non-nil, is a predicate called once per request BEFORE
dispatch with a request-info plist (:method :path :headers :body).
Returning nil rejects the request with 401 Unauthorized — rendered in
DEFAULT-FORMAT, so a json default yields an application/json error
envelope — and the handler is never run; a non-nil return lets the
request proceed.

Returns the server process object."
  (clime-serve--ensure-web-server)
  ;; Inject API commands (idempotent — skips if already present)
  (clime-serve--inject-api-commands app)
  ;; Resolve the default output format up front (errors early on a bad name).
  (let ((fmt (clime-serve--resolve-format app default-format)))
    ;; Run setup/config once at startup (mirror clime-run two-pass)
    (let* ((setup (clime-app-setup app))
           (config-factory (clime-app-config app)))
      (when (or setup config-factory)
        (let ((result (clime-parse app '() t)))
          (when setup (funcall setup app result))
          (when config-factory
            (let ((provider (funcall config-factory app result)))
              (when provider
                (setf (clime-parse-result-config-provider result) provider))))
          (clime-parse-finalize result))))
    ;; Build root handler and start server
    (clime-serve--validate-http-policies app)
    (let* ((handler (clime-serve--make-handler app fmt auth host port))
           (server (ws-start handler port nil :host host)))
      (push (cons port server) clime-serve--servers)
      (message "clime-serve: %s listening on %s:%d"
               (clime-node-name app) host port)
      server)))

(defun clime-serve--header-value (headers name)
  "Return case-insensitive HTTP header NAME from HEADERS."
  (or (cdr (assoc-string name headers t))
      (cdr (assoc (intern (concat ":" name)) headers))))

(defun clime-serve--method-url (headers)
  "Return (METHOD . URL) extracted from web-server HEADERS."
  (let ((known '(:GET :POST :PUT :PATCH :DELETE :HEAD :OPTIONS)))
    (or (cl-some (lambda (method)
                   (when-let ((url (cdr (assoc method headers))))
                     (cons (intern (substring (symbol-name method) 1)) url)))
                 known)
        (cons nil "/"))))

(defun clime-serve--json-body-inputs (body metadata)
  "Return (INPUTS . METADATA) parsed from BODY with controlled errors."
  (if (not (and body (not (string-empty-p body))))
      (cons nil metadata)
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-key-type 'string)
               (parsed (json-read-from-string body)))
          (if (listp parsed)
              (cons (clime-serve--alist-inputs parsed 'json-body) metadata)
            (cons nil (plist-put metadata :json-error
                                  "JSON body must be an object"))))
      (error
       (cons nil (plist-put metadata :json-error
                             (format "Malformed JSON body: %s"
                                     (error-message-string err))))))))

(defun clime-serve--request-from-web-server
    (method url headers body host port query-params)
  "Build a normalized dispatch request from web-server request data."
  (let* ((segments (mapcar #'url-unhex-string
                           (cl-remove-if #'string-empty-p
                                         (split-string url "/" t))))
         (content-type (clime-serve--header-value headers "Content-Type"))
         (origin (clime-serve--header-value headers "Origin"))
         (fetch-site (clime-serve--header-value headers "Sec-Fetch-Site"))
         (metadata (list :method method
                         :content-type content-type
                         :body-bytes (if body (string-bytes body) 0)
                         :server-origin (format "http://%s:%d" host port)
                         :origin origin
                         :sec-fetch-site fetch-site))
         (json-result (clime-serve--json-body-inputs body metadata))
         (json-inputs (car json-result))
         (metadata (cdr json-result)))
    (clime-make-dispatch-request
     :surface 'serve
     :adapter 'http
     :path segments
     :inputs (append (clime-serve--alist-inputs query-params 'query)
                     json-inputs)
     :metadata metadata)))

(defun clime-serve--make-handler (app &optional default-format auth host port)
  "Build the root ws-start handler function for APP.
DEFAULT-FORMAT, when non-nil, is a `clime-output-format' used for bare
routes (no `.<ext>' suffix) and for rendering an AUTH rejection.  AUTH,
when non-nil, is a request-info predicate enforced before dispatch (see
`clime-serve'); a nil return yields a 401 and the handler is never run."
  (lambda (request)
    (let* ((process (with-no-warnings (slot-value request 'process)))
           (headers (with-no-warnings (slot-value request 'headers)))
           (body    (with-no-warnings (slot-value request 'body)))
           (method-url (clime-serve--method-url headers))
           (method (car method-url))
           (url (cdr method-url))
           ;; Web-server-agnostic view handed to the auth predicate.
           (request-info (list :method method :path url
                               :headers headers :body body))
           (reject (clime-serve--auth-gate auth request-info default-format)))
      (if reject
          ;; Auth failed: respond (format-aware) 401 and never dispatch.
          (clime-serve--send-response process reject)
        (let* (;; web-server parses query params into headers as
               ;; string-keyed entries; keyword keys are HTTP headers
               (query-params (cl-remove-if-not
                              (lambda (e) (stringp (car e)))
                              headers))
               (dispatch-request
                (clime-serve--request-from-web-server
                 method url headers body (or host "127.0.0.1")
                 (or port clime-serve-default-port) query-params))
               (result (clime-serve--dispatch-request
                        app dispatch-request default-format)))
          (clime-serve--send-response process result))))))

(defun clime-serve-stop (&optional server)
  "Stop SERVER, or all clime servers if SERVER is nil."
  (if server
      (progn
        (ws-stop server)
        (setq clime-serve--servers
              (cl-remove-if (lambda (entry) (eq (cdr entry) server))
                            clime-serve--servers)))
    ;; Stop all
    (dolist (entry clime-serve--servers)
      (ws-stop (cdr entry)))
    (setq clime-serve--servers nil)))

(provide 'clime-serve)
;;; clime-serve.el ends here
