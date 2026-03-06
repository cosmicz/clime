;;; clime-parse.el --- Single-pass CLI argument parser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Deterministic single-pass parser for CLI argument vectors.
;; Consumes a `clime-app' tree and an argv list, producing a
;; `clime-parse-result' with the resolved command, path, and params.

;;; Code:

(require 'cl-lib)
(require 'clime-core)

;;; ─── Error Symbols ──────────────────────────────────────────────────────

(define-error 'clime-usage-error "CLI usage error")
(define-error 'clime-help-requested "CLI help requested")

;;; ─── Parse Result ───────────────────────────────────────────────────────

(cl-defstruct (clime-parse-result (:constructor clime-parse-result--create)
                                  (:copier nil))
  "Result of parsing an argv against a command tree."
  (command nil :documentation "The resolved leaf `clime-command', or nil when parsing ends on a group.")
  (node nil :documentation "The terminal node where parsing ended.  Always set.  Same as command when command is non-nil.")
  (path nil :type list :documentation "List of node names from root to command.")
  (params nil :type list :documentation "Plist of (param-name value) for all scopes."))

;;; ─── Internal Helpers ───────────────────────────────────────────────────

(defun clime--option-like-p (token)
  "Return non-nil if TOKEN looks like an option flag.
Matches anything starting with \"-\" that isn't just \"-\" alone."
  (and (stringp token)
       (> (length token) 1)
       (= (aref token 0) ?-)))

(defun clime--split-long-equals (token)
  "Split \"--name=value\" into (\"--name\" . \"value\"), or nil if no \"=\"."
  (when (and (> (length token) 2)
             (= (aref token 0) ?-)
             (= (aref token 1) ?-))
    (let ((pos (cl-position ?= token)))
      (when pos
        (cons (substring token 0 pos)
              (substring token (1+ pos)))))))

(defun clime--find-option-in-scope (flag current-node root)
  "Find option matching FLAG, first in CURRENT-NODE then in ROOT.
Return (OPTION . SCOPE) where SCOPE is \\='current or \\='root, or nil."
  (let ((opt (clime-node-find-option current-node flag)))
    (if opt
        (cons opt 'current)
      (unless (eq current-node root)
        (let ((root-opt (clime-node-find-option root flag)))
          (when root-opt
            (cons root-opt 'root)))))))

(defun clime--expand-short-bundle (token current-node root)
  "Try to expand TOKEN as a short flag bundle like \"-abc\".
Return a list of single-char flags if all are boolean in scope,
or nil if TOKEN is not a valid bundle."
  (when (and (> (length token) 2)
             (= (aref token 0) ?-)
             (/= (aref token 1) ?-))
    (let ((chars (substring token 1))
          (flags '())
          (all-boolean t))
      (dotimes (i (length chars))
        (let* ((flag (format "-%c" (aref chars i)))
               (found (clime--find-option-in-scope flag current-node root)))
          (if (and found (clime-option-boolean-p (car found)))
              (push flag flags)
            (setq all-boolean nil))))
      (when all-boolean
        (nreverse flags)))))

(defun clime--coerce-value (value type flag-or-name)
  "Coerce string VALUE to TYPE, signaling `clime-usage-error' on failure.
FLAG-OR-NAME is used in error messages."
  (pcase type
    ((or 'string 'nil) value)
    ('integer
     (let ((n (string-to-number value)))
       (unless (and (integerp n)
                    (string-match-p "\\`-?[0-9]+\\'" value))
         (signal 'clime-usage-error
                 (list (format "Expected integer for %s, got \"%s\""
                               flag-or-name value))))
       n))
    ('number
     (let ((n (string-to-number value)))
       (when (and (= n 0) (not (string-match-p "\\`-?0+\\.?0*\\'" value)))
         (signal 'clime-usage-error
                 (list (format "Expected number for %s, got \"%s\""
                               flag-or-name value))))
       n))
    (_ (signal 'clime-usage-error
               (list (format "Unknown type %s for %s" type flag-or-name))))))

(defun clime--consume-option (opt params token-value)
  "Consume option OPT into PARAMS plist, returning updated plist.
TOKEN-VALUE is the string value for value-taking options, or nil for booleans."
  (let ((name (clime-option-name opt))
        (type (clime-option-type opt)))
    (cond
     ;; Count option: increment
     ((clime-option-count opt)
      (let ((current (or (plist-get params name) 0)))
        (plist-put params name (1+ current))))
     ;; Boolean (non-count): set t
     ((clime-option-boolean-p opt)
      (plist-put params name t))
     ;; Multiple: append to list
     ((clime-option-multiple opt)
      (let* ((coerced (clime--coerce-value token-value type (car (clime-option-flags opt))))
             (current (plist-get params name)))
        (plist-put params name (append current (list coerced)))))
     ;; Normal value option
     (t
      (plist-put params name
                 (clime--coerce-value token-value type (car (clime-option-flags opt))))))))

(defun clime--required-args-satisfied-p (node params)
  "Return non-nil if all required positional args of NODE have values in PARAMS."
  (let ((args (clime-node-args node)))
    (cl-every (lambda (arg)
                (or (not (clime-arg-required arg))
                    (plist-member params (clime-arg-name arg))))
              args)))

;;; ─── Stdin Sentinel ────────────────────────────────────────────────────

(defvar clime--stdin-content nil
  "Cached stdin content.  Read once on first `-' sentinel, shared across params.
Reset to nil at the start of each `clime-run' invocation.")

(defvar clime--stdin-app nil
  "The app being parsed, used by `clime--read-stdin' for env-prefix lookup.")

(defun clime--read-stdin ()
  "Read all content from stdin and return it as a trimmed string.
First checks for a pre-buffered stdin file via env var
\({ENV_PREFIX}_STDIN_FILE or CLIME_STDIN_FILE), then falls back to
reading stdin directly via `read-from-minibuffer' (works in batch mode).
Signals `clime-usage-error' if no content is available."
  (or clime--stdin-content
      (setq clime--stdin-content
            (let* ((prefix (and clime--stdin-app
                                (clime-app-p clime--stdin-app)
                                (clime-app-env-prefix clime--stdin-app)))
                   (app-var (and prefix (getenv (concat prefix "_STDIN_FILE"))))
                   (generic-var (getenv "CLIME_STDIN_FILE"))
                   (stdin-file (or app-var generic-var)))
              (let ((raw (cond
                          ;; Read from pre-buffered temp file
                          ((and stdin-file (file-exists-p stdin-file))
                           (with-temp-buffer
                             (insert-file-contents stdin-file)
                             (buffer-string)))
                          ;; Read directly from stdin (batch mode)
                          (noninteractive
                           (let ((lines '())
                                 (line nil))
                             (while (setq line (ignore-errors
                                                 (read-from-minibuffer "")))
                               (push line lines))
                             (when lines
                               (mapconcat #'identity (nreverse lines) "\n")))))))
                (let ((trimmed (and raw (string-trim raw))))
                  (if (or (null trimmed) (string-empty-p trimmed))
                      (signal 'clime-usage-error
                              (list "Stdin sentinel `-' used but no input available"))
                    trimmed)))))))

(defun clime--resolve-stdin-value (value)
  "If VALUE is \"-\", return stdin content; otherwise return VALUE unchanged."
  (if (equal value "-")
      (clime--read-stdin)
    value))

;;; ─── Env Var Provider ──────────────────────────────────────────────────

(defun clime--env-var-for-option (opt app)
  "Return the env var name for OPT, or nil if none applies.
Uses explicit :env on the option, or auto-derives from APP's :env-prefix."
  (or (clime-option-env opt)
      (when (and (clime-app-p app) (clime-app-env-prefix app))
        (concat (clime-app-env-prefix app) "_"
                (upcase (replace-regexp-in-string
                         "-" "_" (symbol-name (clime-option-name opt))))))))

(defun clime--parse-boolean-env (value flag-or-name)
  "Parse VALUE as a boolean env var string.
Return t for truthy values, nil for falsy.
Signal `clime-usage-error' for unrecognized values.
FLAG-OR-NAME is used in error messages."
  (let ((v (downcase value)))
    (cond
     ((member v '("1" "true" "yes")) t)
     ((member v '("0" "false" "no")) nil)
     (t (signal 'clime-usage-error
                (list (format "Invalid boolean value \"%s\" for env var %s (expected 1/true/yes or 0/false/no)"
                              value flag-or-name)))))))

(defun clime--apply-env (nodes params app)
  "Apply env var values for options in NODES not already in PARAMS.
APP is the root app node (for :env-prefix).  Returns updated PARAMS."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let* ((name (clime-option-name opt))
             (env-var (and (not (plist-member params name))
                           (clime--env-var-for-option opt app)))
             (value (and env-var (getenv env-var))))
        (when (and value (not (string-empty-p value)))
          (cond
           ;; Count option: parse as integer
           ((clime-option-count opt)
            (setq params (plist-put params name
                                   (clime--coerce-value value 'integer env-var))))
           ;; Boolean flag: parse truthy/falsy, falsy means unset
           ((clime-option-boolean-p opt)
            (when (clime--parse-boolean-env value env-var)
              (setq params (plist-put params name t))))
           ;; Multiple option: split on comma, coerce each
           ((clime-option-multiple opt)
            (let ((type (clime-option-type opt)))
              (setq params
                    (plist-put params name
                               (mapcar (lambda (v)
                                         (clime--coerce-value
                                          (string-trim v) type env-var))
                                       (split-string value "," t))))))
           ;; Normal value option
           (t
            (setq params
                  (plist-put params name
                             (clime--coerce-value
                              value (clime-option-type opt) env-var)))))))))
  params)

(defun clime--apply-defaults (nodes params)
  "Apply default values for all options and args in NODES not already in PARAMS.
NODES is a list of nodes whose params to process.  Returns updated PARAMS."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let ((name (clime-option-name opt)))
        (unless (plist-member params name)
          (let ((default (clime-option-default opt)))
            (when default
              (setq params (plist-put params name
                                      (if (functionp default)
                                          (funcall default)
                                        default))))))))
    (dolist (arg (clime-node-args node))
      (let ((name (clime-arg-name arg)))
        (unless (plist-member params name)
          (let ((default (clime-arg-default arg)))
            (when default
              (setq params (plist-put params name
                                      (if (functionp default)
                                          (funcall default)
                                        default)))))))))
  params)

(defun clime--check-required (nodes params path)
  "Check that all required params in NODES have values in PARAMS.
PATH is the command path for error messages.  Signals `clime-usage-error'."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (when (and (clime-option-required opt)
                 (not (plist-member params (clime-option-name opt))))
        (signal 'clime-usage-error
                (list (format "Missing required option %s for %s"
                              (car (clime-option-flags opt))
                              (string-join path " "))))))
    (dolist (arg (clime-node-args node))
      (when (and (clime-arg-required arg)
                 (not (plist-member params (clime-arg-name arg))))
        (signal 'clime-usage-error
                (list (format "Missing required argument <%s> for %s"
                              (clime-arg-name arg)
                              (string-join path " "))))))))

;;; ─── Main Parse Function ────────────────────────────────────────────────

(defun clime-parse (app argv)
  "Parse ARGV against APP command tree.
Return a `clime-parse-result' on success.
Signal `clime-usage-error' on parse failures.
Signal `clime-help-requested' for --help/-h/--version."
  (let ((current-node app)
        (root app)
        (option-parsing t)
        (params '())
        (path (list (clime-app-name app)))
        (arg-index 0)
        (visited-nodes (list app))
        (i 0)
        (len (length argv))
        (clime--stdin-app app))
    ;; Set parent refs for direct children (if not already set)
    (clime--set-parent-refs app)
    (while (< i len)
      (let* ((token (nth i argv))
             (child (and (clime-group-only-p current-node)
                         (clime--required-args-satisfied-p current-node params)
                         (clime-group-find-child current-node token))))
        (cond
         ;; 1. End of options
         ((string= token "--")
          (setq option-parsing nil)
          (cl-incf i))

         ;; 2. Help/version intercept
         ((and option-parsing
               (or (string= token "--help") (string= token "-h")))
          (signal 'clime-help-requested
                  (list :node current-node :path path)))

         ((and option-parsing
               (string= token "--version")
               (eq current-node root)
               (clime-app-version root))
          (signal 'clime-help-requested
                  (list :node root :path path :version t)))

         ;; 3. Option-like token
         ((and option-parsing (clime--option-like-p token))
          ;; Try --name=value split
          (if-let ((split (clime--split-long-equals token)))
              ;; --name=value form
              (let* ((flag (car split))
                     (value (clime--resolve-stdin-value (cdr split)))
                     (found (clime--find-option-in-scope flag current-node root)))
                  (unless found
                    (signal 'clime-usage-error
                            (list (format "Unknown option %s for %s"
                                          flag (string-join path " ")))))
                  (when (clime-option-boolean-p (car found))
                    (signal 'clime-usage-error
                            (list (format "Option %s does not take a value" flag))))
                  (setq params (clime--consume-option (car found) params value))
                  (cl-incf i))
              ;; Try short bundle expansion
              (if-let ((bundle (clime--expand-short-bundle token current-node root)))
                  ;; Expanded bundle: process each flag
                  (progn
                    (dolist (flag bundle)
                      (let ((found (clime--find-option-in-scope flag current-node root)))
                        (setq params (clime--consume-option (car found) params nil))))
                    (cl-incf i))
                ;; Single flag
                (let ((found (clime--find-option-in-scope token current-node root)))
                  (unless found
                    (signal 'clime-usage-error
                            (list (format "Unknown option %s for %s"
                                          token (string-join path " ")))))
                  (let ((opt (car found)))
                    (if (clime-option-boolean-p opt)
                        ;; Boolean: no value consumed
                        (progn
                          (setq params (clime--consume-option opt params nil))
                          (cl-incf i))
                      ;; Value option: consume next token
                      (cl-incf i)
                      (when (>= i len)
                        (signal 'clime-usage-error
                                (list (format "Option %s requires a value"
                                              token))))
                      (setq params (clime--consume-option opt params
                                                          (clime--resolve-stdin-value (nth i argv))))
                      (cl-incf i)))))))

         ;; 4. Try group descent
         (child
          (setq current-node child)
          (setq path (append path (list (clime-node-name child))))
          (push child visited-nodes)
          (setq arg-index 0)
          (cl-incf i))

         ;; 5. Positional arg
         (t
          (let ((args (clime-node-args current-node)))
            (if (< arg-index (length args))
                (let ((arg-spec (nth arg-index args)))
                  (if (eq (clime-arg-nargs arg-spec) :rest)
                      ;; Rest arg: collect remaining non-option tokens
                      (let ((rest-values '()))
                        (while (< i len)
                          (let ((tok (nth i argv)))
                            (if (and option-parsing (string= tok "--"))
                                (progn
                                  (setq option-parsing nil)
                                  (cl-incf i))
                              (push (clime--resolve-stdin-value tok) rest-values)
                              (cl-incf i))))
                        (setq params (plist-put params (clime-arg-name arg-spec)
                                               (nreverse rest-values))))
                    ;; Normal positional
                    (let ((coerced (clime--coerce-value
                                    (clime--resolve-stdin-value token)
                                    (clime-arg-type arg-spec)
                                    (format "<%s>" (clime-arg-name arg-spec)))))
                      (setq params (plist-put params (clime-arg-name arg-spec) coerced)))
                    (cl-incf arg-index)
                    (cl-incf i)))
              ;; No more arg specs
              (signal 'clime-usage-error
                      (list (format "Unexpected argument \"%s\" for %s"
                                    token (string-join path " "))))))))))

    ;; Post-parse: if we ended on a group (not a leaf command), check
    ;; if it has an invoke handler, otherwise it's missing a subcommand
    (when (and (clime-group-only-p current-node)
               (not (clime-app-p current-node))
               (clime-group-children current-node)
               (not (clime-node-handler current-node)))
      (signal 'clime-usage-error
              (list (format "Missing subcommand for %s"
                            (string-join path " ")))))

    ;; If we're still at root with children and no subcommand was given
    (when (and (clime-app-p current-node)
               (clime-group-children current-node)
               (not (clime-node-handler current-node))
               (= (length path) 1))
      (signal 'clime-help-requested
              (list :node current-node :path path)))

    ;; Apply env vars, then defaults, then check required
    (setq params (clime--apply-env visited-nodes params root))
    (setq params (clime--apply-defaults visited-nodes params))
    (clime--check-required visited-nodes params path)

    ;; Build result
    (clime-parse-result--create
     :command (if (clime-command-p current-node) current-node nil)
     :node current-node
     :path path
     :params params)))

;;; ─── Parent Ref Setup ───────────────────────────────────────────────────

(defun clime--set-parent-refs (node)
  "Recursively set parent refs for children of NODE.
Also runs ancestor collision checks after parent refs are established."
  (clime--set-parent-refs-1 node)
  (clime-check-ancestor-collisions node))

(defun clime--set-parent-refs-1 (node)
  "Set parent refs for children of NODE (internal recursive helper)."
  (when (clime-group-p node)
    (dolist (entry (clime-group-children node))
      (let ((child (cdr entry)))
        (setf (clime-node-parent child) node)
        (clime--set-parent-refs-1 child)))))

(provide 'clime-parse)
;;; clime-parse.el ends here
