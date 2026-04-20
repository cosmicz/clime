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
(require 'clime-core)
(require 'clime-parse)
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

(defun clime-serve--next-unfilled-arg (node values)
  "Return the next positional arg on NODE not yet in VALUES, or nil.
A :rest arg is always returned (it accepts unlimited segments)."
  (or (cl-find-if (lambda (arg)
                    (eq (clime-arg-nargs arg) :rest))
                  (clime-node-args node))
      (cl-find-if (lambda (arg)
                    (not (clime-values-get values (clime-param-name arg))))
                  (clime-node-args node))))

(defun clime-serve--walk-path (tree segments)
  "Walk TREE by SEGMENTS, returning plist (:node N :path P :values V).
Segments matching child names descend into that child.
Non-matching segments are consumed as positional arg values.
Signals error if a segment matches neither child nor available arg."
  (let ((node tree)
        (path (list (clime-node-name tree)))
        (values nil))
    (dolist (seg segments)
      (let ((child (and (clime-branch-p node)
                        (clime-group-find-child node seg))))
        (if child
            (progn
              (setq node child)
              (push (clime-node-name child) path))
          ;; No child match — try consuming as positional arg
          (let ((arg (clime-serve--next-unfilled-arg node values)))
            (if arg
                (let ((name (clime-param-name arg)))
                  (if (eq (clime-arg-nargs arg) :rest)
                      ;; Rest arg: accumulate into a list
                      (let ((existing (clime-values-value values name)))
                        (setq values (clime-values-set
                                      values name
                                      (append (and (listp existing) existing)
                                              (list seg))
                                      'user)))
                    (setq values (clime-values-set values name seg 'user))))
              (signal 'clime-serve-not-found
                      (list (format "No match for path segment: %s" seg))))))))
    (list :node node :path (nreverse path) :values values)))

(define-error 'clime-serve-not-found "Route not found")

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

;;; ─── Type Coercion ──────────────────────────────────────────────────────

;;; ─── Format Helpers ─────────────────────────────────────────────────────

(defun clime-serve--content-type-for-format (fmt)
  "Return MIME content-type string for output format FMT.
FMT is a `clime-output-format' struct or nil (defaults to text/plain)."
  (if (null fmt)
      "text/plain; charset=utf-8"
    (pcase (clime-output-format-name fmt)
      ('json "application/json")
      ('html "text/html")
      ('yaml "text/yaml")
      (_ "text/plain; charset=utf-8"))))

(defun clime-serve--extract-suffix (segments app)
  "Try to strip a format suffix from the last element of SEGMENTS.
Returns (STRIPPED-SEGMENTS . FORMAT) if a registered output format
matches, or (SEGMENTS . nil) if no match or no formats on APP."
  (let* ((formats (clime-app-output-formats app))
         (last-seg (car (last segments))))
    (if (or (null formats) (null last-seg)
            (not (string-match "\\`\\(.+\\)\\.\\([^.]+\\)\\'" last-seg)))
        (cons segments nil)
      (let* ((base (match-string 1 last-seg))
             (ext  (match-string 2 last-seg))
             (fmt  (cl-find (intern ext) formats
                            :key #'clime-output-format-name)))
        (if fmt
            (cons (append (butlast segments) (list base)) fmt)
          (cons segments nil))))))

;;; ─── Dispatch ───────────────────────────────────────────────────────────

(defun clime-serve--dispatch (app segments params &optional default-format)
  "Dispatch a request to APP.
SEGMENTS is a list of URL path segments (strings).
PARAMS is an alist of (KEY . VALUE) from query string or body.
DEFAULT-FORMAT, when non-nil, is a `clime-output-format' to use when
no path suffix overrides the format.
Returns plist (:status N :body STRING :content-type STRING)."
  (let* ((suffix-result (clime-serve--extract-suffix segments app))
         (segments (car suffix-result))
         (suffix-fmt (cdr suffix-result))
         (api-fmt (when (and (null default-format)
                             (equal (car segments) "_api"))
                    (cl-find 'json (clime-app-output-formats app)
                             :key #'clime-output-format-name)))
         (active-fmt (or suffix-fmt default-format api-fmt)))
    (condition-case err
        (let* ((tree (clime--prepare-tree app))
               (walk (clime-serve--walk-path tree segments))
               (node (plist-get walk :node))
               (path (plist-get walk :path))
               (values (plist-get walk :values))
               (handler (clime-node-handler node)))
          (unless handler
            (signal 'clime-serve-not-found
                    (list (format "No handler at path: /%s"
                                  (string-join segments "/")))))
          ;; Seed values from request params
          (setq values (clime-serve--seed-values values params))
          ;; Finalize → execute via shared pipeline
          (let* ((result
                  (catch 'clime-serve--not-found
                    (let ((clime-out--active-format
                           (or active-fmt clime-out--active-format)))
                      (clime-run-from-values app node path values)))))
            ;; catch returns the thrown string (not-found) or the (exit-code . output) cons
            (if (stringp result)
                (list :status 404 :body result
                      :content-type "text/plain; charset=utf-8")
              (let ((exit-code (car result))
                    (output (cdr result)))
                (list :status (pcase (or exit-code 0)
                                (0 200)
                                (2 400)
                                (_ 500))
                      :body output
                      :content-type (clime-serve--content-type-for-format active-fmt))))))
      (clime-serve-not-found
       (list :status 404 :body (cadr err)
             :content-type "text/plain; charset=utf-8")))))

;;; ─── JSON Serialization ─────────────────────────────────────────────────

(defun clime-serve--option-to-alist (opt)
  "Serialize OPT (`clime-option') to an alist for JSON encoding."
  (let ((result `(("name" . ,(symbol-name (clime-option-name opt)))
                  ("flags" . ,(vconcat (clime-option-flags opt)))
                  ("type" . ,(let ((ty (clime-option-type opt)))
                               (if (listp ty)
                                   (symbol-name (car ty))
                                 (symbol-name ty))))
                  ("help" . ,(or (clime-option-help opt) :json-false))
                  ("required" . ,(if (clime-option-required opt) t :json-false)))))
    (when (clime-option-default opt)
      (push `("default" . ,(clime-option-default opt)) result))
    (when (clime-option-choices opt)
      (let ((choices (clime-option-choices opt)))
        (when (listp choices)
          (push `("choices" . ,(vconcat choices)) result))))
    (when (clime-option-negatable opt)
      (push '("negatable" . t) result))
    (when (clime-option-deprecated opt)
      (push `("deprecated" . ,(let ((d (clime-option-deprecated opt)))
                                 (if (stringp d) d t)))
            result))
    result))

(defun clime-serve--arg-to-alist (arg)
  "Serialize ARG (`clime-arg') to an alist for JSON encoding."
  (let ((result `(("name" . ,(symbol-name (clime-param-name arg)))
                  ("type" . ,(let ((ty (clime-arg-type arg)))
                               (if (listp ty)
                                   (symbol-name (car ty))
                                 (symbol-name ty))))
                  ("help" . ,(or (clime-param-help arg) :json-false))
                  ("required" . ,(if (clime-param-required arg) t :json-false)))))
    (when (clime-param-default arg)
      (push `("default" . ,(clime-param-default arg)) result))
    (when (clime-arg-choices arg)
      (let ((choices (clime-arg-choices arg)))
        (when (listp choices)
          (push `("choices" . ,(vconcat choices)) result))))
    result))

(defun clime-serve--node-to-alist (node)
  "Serialize NODE to an alist for JSON encoding.
Recurses into children for groups.  Omits hidden nodes and options."
  (let* ((is-group (and (clime-group-p node)
                        (not (clime-command-p node))))
         (result `(("name" . ,(clime-node-name node))
                   ("type" . ,(if is-group "group" "command"))
                   ("help" . ,(or (clime-node-help node) :json-false)))))
    ;; Aliases
    (when (clime-node-aliases node)
      (push `("aliases" . ,(vconcat (clime-node-aliases node))) result))
    ;; Options (non-hidden)
    (let ((opts (cl-remove-if #'clime-option-hidden
                              (clime-node-options node))))
      (when opts
        (push `("options" . ,(vconcat (mapcar #'clime-serve--option-to-alist opts)))
              result)))
    ;; Args
    (when (clime-node-args node)
      (push `("args" . ,(vconcat (mapcar #'clime-serve--arg-to-alist
                                          (clime-node-args node))))
            result))
    ;; Children (non-hidden, recurse)
    (when (clime-branch-p node)
      (let ((children (cl-remove-if
                       (lambda (entry)
                         (clime-node-hidden (cdr entry)))
                       (clime-group-children node))))
        (when children
          (push `("children" . ,(vconcat (mapcar (lambda (entry)
                                                    (clime-serve--node-to-alist (cdr entry)))
                                                  children)))
                result))))
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

;;; ─── API Handlers ──────────────────────────────────────────────────────

(defun clime-serve--routes-handler (ctx)
  "Handler for /_routes — plain text route listing."
  (let* ((app (clime-context-app ctx))
         (tree (clime--prepare-tree app))
         (items (clime-node-collect
                 tree
                 :recurse-p (lambda (n) (and (clime-group-p n)
                                             (not (clime-command-p n))))
                 :match-p (lambda (type item)
                            (and (memq type '(:command :group))
                                 (clime-command-p item)
                                 (clime-node-handler item)
                                 (not (clime-node-hidden item)))))))
    (princ (string-join
            (mapcar
             (lambda (entry)
               (let* ((cmd (cadr entry))
                      (ancestors (clime-node-ancestors cmd))
                      (path-parts
                       (mapcar #'clime-node-name
                               (seq-filter
                                (lambda (n)
                                  (and (not (clime-app-p n))
                                       (not (clime-node-inline n))))
                                ancestors))))
                 (format "/%s  %s"
                         (string-join
                          (append path-parts
                                  (list (clime-node-name cmd)))
                          "/")
                         (or (clime-node-help cmd) ""))))
             items)
            "\n"))
    nil))

(defun clime-serve--api-meta-handler (ctx)
  "Handler for /_api/meta — app metadata."
  (let ((app (clime-context-app ctx)))
    (clime-out `(("name" . ,(clime-node-name app))
                 ("version" . ,(or (clime-app-version app) :json-false))
                 ("help" . ,(or (clime-node-help app) :json-false))))))

(defun clime-serve--api-commands-handler (ctx)
  "Handler for /_api/commands — full tree or single command detail."
  (let* ((app (clime-context-app ctx))
         (path-segs (clime-ctx-get ctx 'path))
         (tree (clime--prepare-tree app)))
    (if path-segs
        ;; Single command detail — walk to the target node
        (condition-case _err
            (let* ((walk (clime-serve--walk-path tree path-segs))
                   (node (plist-get walk :node)))
              (clime-out (clime-serve--node-to-alist node)))
          (clime-serve-not-found
           ;; throw bypasses clime-run--execute's condition-case;
           ;; dispatch catches it and returns 404
           (throw 'clime-serve--not-found
                  (format "Unknown command path: %s"
                          (string-join path-segs "/")))))
      ;; Full tree — list visible top-level children
      (let ((children (cl-remove-if
                       (lambda (entry)
                         (clime-node-hidden (cdr entry)))
                       (clime-group-children tree))))
        (clime-out `(("commands" . ,(vconcat
                                     (mapcar (lambda (entry)
                                               (clime-serve--node-to-alist (cdr entry)))
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
           :handler #'clime-serve--routes-handler))
         (meta-cmd
          (clime-make-command
           :name "meta"
           :help "App metadata"
           :hidden t
           :handler #'clime-serve--api-meta-handler))
         (commands-cmd
          (clime-make-command
           :name "commands"
           :help "Command tree"
           :hidden t
           :args (list (clime-arg--create :name 'path :nargs :rest))
           :handler #'clime-serve--api-commands-handler))
         (api-group
          (clime-group--create
           :name "_api"
           :help "API endpoints"
           :hidden t
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

(cl-defun clime-serve (app &key (port clime-serve-default-port)
                                (host "127.0.0.1"))
  "Start an HTTP server for APP on PORT (default 8080) bound to HOST.
HOST defaults to 127.0.0.1 (localhost only).
Returns the server process object."
  (clime-serve--ensure-web-server)
  ;; Inject API commands (idempotent — skips if already present)
  (clime-serve--inject-api-commands app)
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
  (let* ((handler (clime-serve--make-handler app))
         (server (ws-start handler port nil :host host)))
    (push (cons port server) clime-serve--servers)
    (message "clime-serve: %s listening on %s:%d"
             (clime-node-name app) host port)
    server))

(defun clime-serve--make-handler (app)
  "Build the root ws-start handler function for APP."
  (lambda (request)
    (let* ((process (with-no-warnings (slot-value request 'process)))
           (headers (with-no-warnings (slot-value request 'headers)))
           (body    (with-no-warnings (slot-value request 'body)))
           (url (or (cdr (assoc :GET headers))
                    (cdr (assoc :POST headers))
                    "/"))
           (segments (mapcar #'url-unhex-string
                            (cl-remove-if #'string-empty-p
                                          (split-string url "/" t))))
           ;; web-server parses query params into headers as
           ;; string-keyed entries; keyword keys are HTTP headers
           (query-params (cl-remove-if-not
                          (lambda (e) (stringp (car e)))
                          headers))
           (params (or query-params
                       (and body (clime-serve--parse-json-body body))))
           (result (clime-serve--dispatch app segments params))
           (status (plist-get result :status))
           (ct (or (plist-get result :content-type)
                   "text/plain; charset=utf-8"))
           (response-body (or (plist-get result :body) "")))
      (ws-response-header process status
                          (cons "Content-Type" ct)
                          (cons "Content-Length"
                                (number-to-string (string-bytes response-body))))
      (process-send-string process response-body))))

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
