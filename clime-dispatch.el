;;; clime-dispatch.el --- Transport-neutral dispatch runner  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared dispatch core for adapter-normalized requests.  Adapters own
;; transport parsing and wire response mapping; this module owns route
;; resolution, value seeding, output format selection, execution, status
;; mapping, and dispatch lifecycle events.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'clime-core)
(require 'clime-run)
(require 'clime-output)

(define-error 'clime-dispatch-not-found "Route not found")

(defvar clime-dispatch--default-json-format
  (clime-make-output-format :name 'json :flags '("--json")
                            :help "Output as JSON")
  "Fallback JSON format for injected introspection dispatch.")

(defun clime-dispatch--next-unfilled-arg (node values)
  "Return the next positional arg on NODE not yet in VALUES, or nil."
  (or (cl-find-if (lambda (arg)
                    (eq (clime-arg-nargs arg) :rest))
                  (clime-node-args node))
      (cl-find-if (lambda (arg)
                    (not (clime-values-get values (clime-param-name arg))))
                  (clime-node-args node))))

(defun clime-dispatch-walk-path (tree segments &optional surface)
  "Walk TREE by SEGMENTS on SURFACE, returning (:node N :path P :values V).
SURFACE defaults to `serve' for existing adapters."
  (setq surface (or surface 'serve))
  (unless (clime-node-surface-eligible-p tree surface)
    (signal 'clime-dispatch-not-found
            (list (format "Root unavailable on %s" surface))))
  (let ((node tree)
        (path (list (clime-node-name tree)))
        (values nil))
    (dolist (seg segments)
      (let* ((match (and (clime-branch-p node)
                         (clime-group-find-child-path-on-surface
                          node seg surface)))
             (child (and (eq (plist-get match :status) 'eligible)
                         (car (last (plist-get match :path))))))
        (cond
         (child
            (progn
              (setq node child)
              (push (clime-node-name child) path)))
         (match
          (signal 'clime-dispatch-not-found
                  (list (format "Path segment unavailable on %s: %s"
                                surface seg))))
         (t
          (let ((arg (clime-dispatch--next-unfilled-arg node values)))
            (if arg
                (let ((name (clime-param-name arg)))
                  (if (eq (clime-arg-nargs arg) :rest)
                      (let ((existing (clime-values-value values name)))
                        (setq values (clime-values-set
                                      values name
                                      (append (and (listp existing) existing)
                                              (list seg))
                                      'user)))
                    (setq values (clime-values-set values name seg 'user))))
              (signal 'clime-dispatch-not-found
                      (list (format "No match for path segment: %s" seg)))))))))
    (list :node node :path (nreverse path) :values values)))

(defun clime-dispatch-seed-inputs (values inputs)
  "Seed VALUES from accepted dispatch INPUTS."
  (dolist (input inputs)
    (let ((val (clime-dispatch-input-value input)))
      (when (or (null val) (equal val ""))
        (setq val t))
      (setq values (clime-values-set values
                                     (clime-dispatch-input-name input)
                                     val 'user))))
  values)

(defun clime-dispatch-content-type-for-format (fmt)
  "Return MIME content-type string for output format FMT."
  (if (null fmt)
      "text/plain; charset=utf-8"
    (pcase (clime-output-format-name fmt)
      ('json "application/json")
      ('html "text/html")
      ('yaml "text/yaml")
      (_ "text/plain; charset=utf-8"))))

(defun clime-dispatch-extract-suffix (segments app)
  "Try to strip a format suffix from the last element of SEGMENTS for APP."
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

(defun clime-dispatch-response-status (response)
  "Return RESPONSE's adapter status, defaulting from its outcome."
  (or (plist-get (clime-dispatch-response-adapter-data response) :http-status)
      (plist-get (clime-dispatch-response-adapter-data response) :status)
      (pcase (clime-dispatch-response-outcome response)
        ('completed 200)
        ('usage-error 400)
        ('not-found 404)
        ('rejected 400)
        (_ 500))))

(defun clime-dispatch--response (outcome status body fmt &rest fields)
  "Build an internal dispatch response for OUTCOME and STATUS."
  (apply #'clime-make-dispatch-response
         :outcome outcome
         :body body
         :format fmt
         :content-type (or (plist-get fields :content-type)
                           (clime-dispatch-content-type-for-format fmt))
         :adapter-data (append (plist-get fields :adapter-data)
                               (list :status status))
         fields))

(defun clime-dispatch--fire-response-event
    (app request response start-time &optional event-path event)
  "Finalize and fire one invocation EVENT for terminal RESPONSE.
When EVENT is nil, construct one for dispatch outcomes that never reached the
runner, such as route misses and policy rejections."
  (setq event
        (or event
            (clime-invocation-event--create
             :app app
             :invocation-id clime-run--invocation-id
             :surface (clime-dispatch-request-surface request)
             :path (or event-path (clime-dispatch-request-path request))
             :format (clime-dispatch-response-format response)
             :start-time start-time)))
  (setf (clime-invocation-event-surface event)
        (clime-dispatch-request-surface request)
        (clime-invocation-event-adapter event)
        (clime-dispatch-request-adapter request)
        (clime-invocation-event-response-status event)
        (clime-dispatch-response-status response)
        (clime-invocation-event-phase event)
        (clime-dispatch-response-outcome response)
        (clime-invocation-event-duration event)
        (- (float-time) (or (clime-invocation-event-start-time event)
                            start-time)))
  (when (and event-path
             (memq (clime-dispatch-response-outcome response)
                   '(not-found rejected)))
    (setf (clime-invocation-event-path event) event-path
          (clime-invocation-event-display-path event) nil))
  (when (clime-dispatch-response-error-type response)
    (setf (clime-invocation-event-error-type event)
          (clime-dispatch-response-error-type response)))
  (when-let ((message (or (clime-dispatch-response-error-message response)
                          (and (memq (clime-dispatch-response-outcome response)
                                     '(not-found rejected))
                               (string-trim-right
                                (or (clime-dispatch-response-body response) ""))))))
    (setf (clime-invocation-event-error-message event) message))
  ;; Dispatch owns terminal delivery because it enriches the runner result with
  ;; final adapter status.  The runner finalizer only retains its event.
  (clime-run--fire-lifecycle app event)
  (clime-run--fire-invocation app event))

(defun clime-dispatch--not-found-response (_request segments fmt message)
  "Build a not-found response for SEGMENTS with MESSAGE."
  (clime-make-dispatch-response
   :outcome 'not-found
   :body message
   :format fmt
   :content-type "text/plain; charset=utf-8"
   :error-type 'clime-dispatch-not-found
   :error-message message
   :metadata (list :event-path segments)
   :adapter-data '(:status 404)))

(cl-defun clime-dispatch-run-request
    (app request &optional default-format
         &key policy-rejector default-json-format)
  "Dispatch normalized REQUEST to APP and return a dispatch response.
DEFAULT-FORMAT is used when no path suffix overrides the output format.
POLICY-REJECTOR, when non-nil, is called with NODE, REQUEST, active
format, and start time.  It must return nil or a `clime-dispatch-response'."
  (let ((abort-finalizer nil)
        (clime-run--lifecycle-terminal-delivered nil))
    (unwind-protect
        (let* ((start-time (float-time))
               ;; A normalized adapter request is a new invocation, even when
               ;; its worker was entered through an enclosing CLI invocation.
               (clime-run--invocation-id (clime-run--new-invocation-id))
               (_start-event (setq abort-finalizer
                                   (clime-run--start-lifecycle
                                    app (clime-dispatch-request-surface request)
                                    start-time)))
         (raw-segments (clime-dispatch-request-path request))
         (suffix-result (clime-dispatch-extract-suffix raw-segments app))
         (segments (car suffix-result))
         (suffix-fmt (cdr suffix-result))
         ;; `_api' JSON default is kept in the core deliberately: injected
         ;; introspection nodes dispatch through this same route runner, while
         ;; injection itself stays adapter-owned.
         (api-fmt (when (and (null default-format)
                             (equal (car segments) "_api"))
                    (or (cl-find 'json (clime-app-output-formats app)
                                 :key #'clime-output-format-name)
                        default-json-format
                        clime-dispatch--default-json-format)))
         (active-fmt (or suffix-fmt default-format api-fmt)))
    (condition-case err
        (let* ((tree (clime--prepare-tree app))
               (walk (clime-dispatch-walk-path
                      tree segments (clime-dispatch-request-surface request)))
               (node (plist-get walk :node))
               (path (plist-get walk :path))
               (values (plist-get walk :values))
               (handler (clime-node-handler node)))
          (unless handler
            (signal 'clime-dispatch-not-found
                    (list (format "No handler at path: /%s"
                                  (string-join segments "/")))))
          (when policy-rejector
            (when-let ((rejection (funcall policy-rejector
                                           node request active-fmt start-time)))
              (unless (clime-dispatch-response-p rejection)
                (error "clime-dispatch-run-request: policy hook returned %S"
                       rejection))
              (clime-dispatch--fire-response-event
               app request rejection start-time (clime-dispatch-request-path request))
              (cl-return-from clime-dispatch-run-request rejection)))
          (setq values (clime-dispatch-seed-inputs
                        values (clime-dispatch-request-inputs request)))
          (let* ((invocation-event nil)
                 (clime-run--active-invocation-event nil)
                 (result
                  (catch 'clime-dispatch--not-found
                    (let ((clime-out--active-format
                           (or active-fmt clime-out--active-format))
                          (clime--invocation-surface
                           (clime-dispatch-request-surface request))
                          (clime-run--invocation-event-finalizer
                           (lambda (_event-app event)
                             (setq invocation-event event))))
                      (clime-run-from-values app node path values request)))))
            (setq invocation-event
                  (or invocation-event clime-run--active-invocation-event))
            (if (stringp result)
                (let ((response (clime-dispatch--not-found-response
                                 request segments active-fmt result)))
                  (clime-dispatch--fire-response-event
                   app request response start-time segments invocation-event)
                  response)
              (let* ((exit-code (or (car result) 0))
                     (outcome (pcase exit-code
                                (0 'completed)
                                (2 'usage-error)
                                (_ 'runtime-error)))
                     (status (pcase exit-code
                               (0 200)
                               (2 400)
                               (_ 500))))
                (let ((response
                       (clime-dispatch--response
                        outcome status (cdr result) active-fmt
                        :exit-code exit-code)))
                  (clime-dispatch--fire-response-event
                   app request response start-time path invocation-event)
                  response)))))
      (clime-dispatch-not-found
       (let ((response (clime-dispatch--not-found-response
                        request segments active-fmt (cadr err))))
         (clime-dispatch--fire-response-event
          app request response start-time segments)
         response))
      ;; Long-lived pipe and spool workers catch these errors and continue.
      ;; Emit an explicit lifecycle abort before re-signalling so their start
      ;; records cannot be mistaken for a killed process.
      (error
       (clime-run--fire-lifecycle
        app
        (clime-invocation-event--create
         :app app :invocation-id clime-run--invocation-id
         :surface (clime-dispatch-request-surface request)
         :phase 'aborted :observer 'self
         :error-type (car err) :error-message (error-message-string err)
         :start-time start-time
         :duration (- (float-time) start-time)))
       (signal (car err) (cdr err)))))
      (when clime-run--lifecycle-terminal-delivered
        (clime-run--unregister-abort-finalizer abort-finalizer)))))

(provide 'clime-dispatch)
;;; clime-dispatch.el ends here
