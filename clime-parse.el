;;; clime-parse.el --- Single-pass CLI argument parser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
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
  (command nil :documentation "Resolved leaf command, or nil for group.")
  (node nil :documentation "Terminal node where parsing ended.")
  (path nil :type list :documentation "List of node names from root to command.")
  (params nil :type list :documentation "Plist of (param-name value) for all scopes.")
  (visited-nodes nil :type list :documentation "Nodes visited during parse (for finalization).")
  (finalized nil :type boolean :documentation "Non-nil after pass-2 finalization."))

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

(defun clime--find-option-in-scope (flag current-node _root)
  "Find option matching FLAG, first in CURRENT-NODE then in ancestors.
Walk the parent chain from CURRENT-NODE upward.
Return (OPTION . SCOPE) where SCOPE is \\='current or \\='ancestor, or nil."
  (let ((opt (clime-node-find-option current-node flag)))
    (if opt
        (cons opt 'current)
      (let ((node (clime-node-parent current-node)))
        (while (and node (not opt))
          (setq opt (clime-node-find-option node flag))
          (unless opt (setq node (clime-node-parent node))))
        (when opt
          (cons opt 'ancestor))))))

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

(defun clime--transform-value (value type choices coerce flag-or-name)
  "Coerce VALUE by TYPE, validate against CHOICES, apply COERCE.
CHOICES may be a list or a function returning a list.
Dynamic choices (functions) are skipped here and deferred to
`clime-parse-finalize' (pass 2), enabling a setup hook to
configure state before validation.
FLAG-OR-NAME is used in error messages."
  (let* ((result (clime--coerce-value value type flag-or-name))
         ;; Only validate static choices in pass 1; defer dynamic (functionp)
         (resolved (and choices (not (functionp choices)) choices)))
    (when (and resolved (not (member result resolved)))
      (signal 'clime-usage-error
              (list (format "Invalid value \"%s\" for %s (choose from: %s)"
                            result flag-or-name
                            (mapconcat (lambda (c) (format "%s" c))
                                       resolved ", ")))))
    (if coerce (funcall coerce result) result)))

(defun clime--try-consume-option (tok argv i len params option-parsing
                                      current-node root path)
  "Try to consume TOK as a known option from ARGV at index I.
Handles `--', `--help'/`-h', `--flag=value', and single known flags.
Returns (PARAMS I OPTION-PARSING) if consumed, or nil if TOK is not
a recognized option.  Signals `clime-help-requested' for help flags.
LEN is the length of ARGV.  CURRENT-NODE and ROOT define option scope.
PATH is the command path for help signals."
  (cond
   ;; -- disables option parsing
   ((and option-parsing (string= tok "--"))
    (list params (1+ i) nil))
   ;; --help / -h triggers help
   ((and option-parsing (or (string= tok "--help") (string= tok "-h")))
    (signal 'clime-help-requested
            (list :node current-node :path path)))
   ;; --flag=value syntax with known option
   ((and option-parsing
         (clime--option-like-p tok)
         (clime--split-long-equals tok))
    (let* ((split (clime--split-long-equals tok))
           (flag (car split))
           (value (clime--resolve-stdin-value (cdr split)))
           (found (clime--find-option-in-scope flag current-node root)))
      (when found
        (when (clime-option-boolean-p (car found))
          (signal 'clime-usage-error
                  (list (format "Option %s does not take a value" flag))))
        (list (clime--consume-option (car found) params value)
              (1+ i)
              option-parsing))))
   ;; Known single option
   ((and option-parsing
         (clime--option-like-p tok)
         (clime--find-option-in-scope tok current-node root))
    (let* ((found (clime--find-option-in-scope tok current-node root))
           (opt (car found)))
      (if (clime-option-boolean-p opt)
          (list (clime--consume-option opt params nil)
                (1+ i)
                option-parsing)
        ;; Value option: consume next token
        (let ((next-i (1+ i)))
          (when (>= next-i len)
            (signal 'clime-usage-error
                    (list (format "Option %s requires a value" tok))))
          (list (clime--consume-option opt params
                                       (clime--resolve-stdin-value (nth next-i argv)))
                (+ i 2)
                option-parsing)))))))

(defun clime--consume-option (opt params token-value)
  "Consume option OPT into PARAMS plist, returning updated plist.
TOKEN-VALUE is the string value for value-taking options, or nil for booleans."
  (let ((name (clime-option-name opt))
        (type (clime-option-type opt))
        (choices (clime-option-choices opt))
        (coerce (clime-option-coerce opt)))
    (cond
     ;; Count option: increment
     ((clime-option-count opt)
      (let ((current (or (plist-get params name) 0)))
        (plist-put params name (1+ current))))
     ;; Boolean (non-count): set t
     ((clime-option-boolean-p opt)
      (plist-put params name t))
     ;; Multiple: append to list (split by separator if set)
     ((clime-option-multiple opt)
      (let* ((sep (clime-option-separator opt))
             (raw-vals (if sep
                          (split-string token-value sep t)
                        (list token-value)))
             (flag (car (clime-option-flags opt)))
             (vals (mapcar (lambda (v)
                             (clime--transform-value v type choices coerce flag))
                           raw-vals))
             (current (plist-get params name)))
        (plist-put params name (append current vals))))
     ;; Normal value option
     (t
      (plist-put params name
                 (clime--transform-value token-value type choices coerce
                                         (car (clime-option-flags opt))))))))

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
           ;; Multiple option: split on separator (or comma), transform each
           ((clime-option-multiple opt)
            (let ((type (clime-option-type opt))
                  (choices (clime-option-choices opt))
                  (coerce (clime-option-coerce opt))
                  (sep (or (clime-option-separator opt) ",")))
              (setq params
                    (plist-put params name
                               (mapcar (lambda (v)
                                         (clime--transform-value
                                          (string-trim v) type choices
                                          coerce env-var))
                                       (split-string value sep t))))))
           ;; Normal value option
           (t
            (setq params
                  (plist-put params name
                             (clime--transform-value
                              value (clime-option-type opt)
                              (clime-option-choices opt)
                              (clime-option-coerce opt)
                              env-var)))))))))
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
                                      (clime--resolve-value default))))))))
    (dolist (arg (clime-node-args node))
      (let ((name (clime-arg-name arg)))
        (unless (plist-member params name)
          (let ((default (clime-arg-default arg)))
            (when default
              (setq params (plist-put params name
                                      (clime--resolve-value default)))))))))
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

(defun clime-parse (app argv &optional skip-finalize)
  "Parse ARGV against APP command tree.
Return a `clime-parse-result' on success.
Signal `clime-usage-error' on parse failures.
Signal `clime-help-requested' for --help/-h/--version.

When SKIP-FINALIZE is non-nil, return an unfinalized result (pass 1 only).
The caller must call `clime-parse-finalize' to complete pass 2.
This enables a setup hook to run between passes."
  (let ((current-node app)
        (root app)
        (option-parsing t)
        (params '())
        (path (list (or (clime-app-argv0 app) (clime-app-name app))))
        (arg-index 0)
        (visited-nodes (list app))
        (i 0)
        (len (length argv))
        (clime--stdin-app app))
    ;; Set parent refs for direct children (if not already set)
    (clime--set-parent-refs app)
    (condition-case err
        (progn
    (while (< i len)
      (let* ((token (nth i argv))
             (child-path (and (clime-branch-p current-node)
                              (clime--required-args-satisfied-p current-node params)
                              (clime-group-find-child-path current-node token)))
             (child (car (last child-path))))
        (cond
         ;; 1. End of options
         ((string= token "--")
          (setq option-parsing nil)
          (cl-incf i))

         ;; 2. Help/version intercept
         ((and option-parsing
               (or (string= token "--help") (string= token "-h")))
          ;; Look ahead: resolve remaining tokens as subcommand path
          (let ((help-node current-node)
                (help-path (copy-sequence path))
                (j (1+ i)))
            (while (< j len)
              (let* ((next (nth j argv))
                     (found (and (clime-branch-p help-node)
                                 (clime-group-find-child-path help-node next))))
                (if found
                    (progn
                      (dolist (node found)
                        (setq help-node node)
                        (setq help-path (append help-path (list (clime-node-name node)))))
                      (cl-incf j))
                  (setq j len))))  ;; stop on non-command token
            (signal 'clime-help-requested
                    (list :node help-node :path help-path))))

         ((and option-parsing
               (string= token "--version")
               (eq current-node root)
               (clime-app-version root))
          (signal 'clime-help-requested
                  (list :node root :path path :version t)))

         ;; 3. Rest arg pending — delegate to rest collector
         ;; (which handles known options internally)
         ((let ((args (clime-node-args current-node)))
            (and (< arg-index (length args))
                 (eq (clime-arg-nargs (nth arg-index args)) :rest)))
          (let ((arg-spec (nth arg-index (clime-node-args current-node)))
                (rest-values '()))
            (while (< i len)
              (let* ((tok (nth i argv))
                     (consumed (clime--try-consume-option
                                tok argv i len params option-parsing
                                current-node root path)))
                (if consumed
                    (setq params (nth 0 consumed)
                          i (nth 1 consumed)
                          option-parsing (nth 2 consumed))
                  ;; Not a known option: collect as rest value
                  (push (clime--resolve-stdin-value tok) rest-values)
                  (cl-incf i))))
            (setq params (plist-put params (clime-arg-name arg-spec)
                                   (nreverse rest-values)))))

         ;; 4. Option-like token
         ((and option-parsing (clime--option-like-p token))
          (let ((consumed (clime--try-consume-option
                           token argv i len params option-parsing
                           current-node root path)))
            (if consumed
                (setq params (nth 0 consumed)
                      i (nth 1 consumed)
                      option-parsing (nth 2 consumed))
              ;; Not a known option — try short bundle, then error
              (if-let* ((bundle (clime--expand-short-bundle token current-node root)))
                  (progn
                    (dolist (flag bundle)
                      (let ((found (clime--find-option-in-scope flag current-node root)))
                        (setq params (clime--consume-option (car found) params nil))))
                    (cl-incf i))
                ;; Unknown option error (includes --flag=value with unknown flag)
                (let ((split (clime--split-long-equals token)))
                  (signal 'clime-usage-error
                          (list (format "Unknown option %s for %s"
                                        (if split (car split) token)
                                        (string-join path " ")))))))))

         ;; 4. Try group descent (child-path includes inline groups)
         (child
          (dolist (node child-path)
            (setq current-node node)
            (setq path (append path (list (clime-node-name node))))
            (push node visited-nodes))
          (setq arg-index 0)
          (cl-incf i))

         ;; 5. Positional arg (non-rest)
         (t
          (let ((args (clime-node-args current-node)))
            (if (< arg-index (length args))
                (let ((arg-spec (nth arg-index args)))
                  (let ((coerced (clime--transform-value
                                  (clime--resolve-stdin-value token)
                                  (clime-arg-type arg-spec)
                                  (clime-arg-choices arg-spec)
                                  (clime-arg-coerce arg-spec)
                                  (format "<%s>" (clime-arg-name arg-spec)))))
                    (setq params (plist-put params (clime-arg-name arg-spec) coerced)))
                  (cl-incf arg-index)
                  (cl-incf i))
              ;; No more arg specs
              (signal 'clime-usage-error
                      (list (format "Unexpected argument \"%s\" for %s"
                                    token (string-join path " "))))))))))

    ;; Post-parse: if we ended on a group (not a leaf command), check
    ;; if it has an invoke handler, otherwise it's missing a subcommand
    (when (and (clime-branch-p current-node)
               (not (clime-app-p current-node))
               (clime-group-children current-node)
               (not (clime-node-handler current-node)))
      (signal 'clime-help-requested
              (list :node current-node :path path)))

    ;; If we're still at root with children and no subcommand was given
    (when (and (clime-app-p current-node)
               (clime-group-children current-node)
               (not (clime-node-handler current-node))
               (= (length path) 1))
      (signal 'clime-help-requested
              (list :node current-node :path path)))

    ;; Build pass-1 result
    (let ((result (clime-parse-result--create
                   :command (if (clime-command-p current-node) current-node nil)
                   :node current-node
                   :path path
                   :params params
                   :visited-nodes visited-nodes)))
      (if skip-finalize
          result
        (clime-parse-finalize result))))
      ;; Enrich usage errors with the current parse path for hints
      (clime-usage-error
       (signal 'clime-usage-error
               (list (cadr err) path))))))

;;; ─── Pass-2 Finalization ────────────────────────────────────────────────

(defun clime--validate-dynamic-choices (nodes params)
  "Validate PARAMS against dynamic (function) :choices in NODES.
Only checks options and args whose :choices slot is a function,
since static choices were already validated in pass 1."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let ((choices (clime-option-choices opt)))
        (when (functionp choices)
          (let ((val (plist-get params (clime-option-name opt)))
                (resolved (funcall choices)))
            (when (and val resolved)
              (let ((vals (if (clime-option-multiple opt) val (list val))))
                (dolist (v vals)
                  (unless (member v resolved)
                    (signal 'clime-usage-error
                            (list (format "Invalid value \"%s\" for %s (choose from: %s)"
                                          v (car (clime-option-flags opt))
                                          (mapconcat (lambda (c) (format "%s" c))
                                                     resolved ", "))))))))))))
    (dolist (arg (clime-node-args node))
      (let ((choices (clime-arg-choices arg)))
        (when (functionp choices)
          (let ((val (plist-get params (clime-arg-name arg)))
                (resolved (funcall choices)))
            (when (and val resolved (not (member val resolved)))
              (signal 'clime-usage-error
                      (list (format "Invalid value \"%s\" for <%s> (choose from: %s)"
                                    val (clime-arg-name arg)
                                    (mapconcat (lambda (c) (format "%s" c))
                                               resolved ", ")))))))))))

(defun clime--run-conformers (nodes params)
  "Run :conform functions for options and args in NODES against PARAMS.
Called in pass 2 after dynamic choices validation.  Skips nil values.
Returns updated PARAMS plist.  Each conformer receives the current value
and returns the conformed value; signaled errors become `clime-usage-error'."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let ((cfn (clime-option-conform opt)))
        (when cfn
          (let* ((name (clime-option-name opt))
                 (val (plist-get params name)))
            (when val
              (condition-case err
                  (let ((conformed (funcall cfn val)))
                    (setq params (plist-put params name conformed)))
                (error
                 (signal 'clime-usage-error
                         (list (format "Invalid value for %s: %s"
                                       (car (clime-option-flags opt))
                                       (error-message-string err)))))))))))
    (dolist (arg (clime-node-args node))
      (let ((cfn (clime-arg-conform arg)))
        (when cfn
          (let* ((name (clime-arg-name arg))
                 (val (plist-get params name)))
            (when val
              (condition-case err
                  (let ((conformed (funcall cfn val)))
                    (setq params (plist-put params name conformed)))
                (error
                 (signal 'clime-usage-error
                         (list (format "Invalid value for <%s>: %s"
                                       (clime-arg-name arg)
                                       (error-message-string err))))))))))))
  params)

(defun clime-parse-finalize (result)
  "Finalize RESULT from pass-1 parse (pass 2).
Validates dynamic choices, runs conformers, applies env vars, applies
defaults, and checks required params.  Returns the updated RESULT."
  (when (clime-parse-result-finalized result)
    (error "clime-parse-finalize: result already finalized"))
  (let* ((visited-nodes (clime-parse-result-visited-nodes result))
         (params (clime-parse-result-params result))
         (path (clime-parse-result-path result))
         (root (cl-find-if #'clime-app-p visited-nodes)))
    ;; Validate dynamic choices (functions resolved now, after setup)
    (clime--validate-dynamic-choices visited-nodes params)
    ;; Run :conform functions (after choices, before env/defaults)
    (setq params (clime--run-conformers visited-nodes params))
    ;; Apply env vars, then defaults, then check required
    (setq params (clime--apply-env visited-nodes params root))
    (setq params (clime--apply-defaults visited-nodes params))
    (clime--check-required visited-nodes params path)
    ;; Update and mark finalized
    (setf (clime-parse-result-params result) params)
    (setf (clime-parse-result-finalized result) t)
    result))

(provide 'clime-parse)
;;; clime-parse.el ends here
