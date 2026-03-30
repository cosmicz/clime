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
  (display-path nil :type list :documentation "User-facing path excluding inline group names.")
  (params nil :type list :documentation "Plist of (param-name value) for all scopes.")
  (values nil :type list :documentation "Values map: alist of (NAME . (VALUE . SOURCE)) pairs.")
  (tree nil :documentation "Deep-copied app tree with :value/:source slots populated.")
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

(defun clime--negated-flag-p (flag)
  "If FLAG is a --no-X negation, return the positive flag --X, else nil.
Only applies to long flags.  Does not double-negate --no-no-X."
  (when (and (> (length flag) 5)  ;; longer than "--no-"
             (string-prefix-p "--no-" flag)
             (not (string-prefix-p "--no-no-" flag)))
    (concat "--" (substring flag 5))))

(defun clime--find-option-in-scope (flag current-node _root)
  "Find option matching FLAG, first in CURRENT-NODE then in ancestors.
Walk the parent chain from CURRENT-NODE upward.
For --no-X flags, checks if the positive --X option is negatable.
Return (OPTION . SCOPE) where SCOPE is \\='current, \\='ancestor,
\\='negated-current, or \\='negated-ancestor.  Returns nil if not found.
Locked options (from alias :vals) are skipped."
  (let ((opt (clime-node-find-option current-node flag)))
    (when (and opt (clime-option-locked opt)) (setq opt nil))
    (if opt
        (cons opt 'current)
      (let ((node (clime-node-parent current-node)))
        (while (and node (not opt))
          (setq opt (clime-node-find-option node flag))
          (when (and opt (clime-option-locked opt)) (setq opt nil))
          (unless opt (setq node (clime-node-parent node))))
        (if opt
            (cons opt 'ancestor)
          ;; Try --no-X → --X negation lookup
          (let ((pos-flag (clime--negated-flag-p flag)))
            (when pos-flag
              (let ((pos-opt (clime-node-find-option current-node pos-flag))
                    (pos-node nil))
                (when (and pos-opt (clime-option-locked pos-opt)) (setq pos-opt nil))
                (unless pos-opt
                  (setq pos-node (clime-node-parent current-node))
                  (while (and pos-node (not pos-opt))
                    (setq pos-opt (clime-node-find-option pos-node pos-flag))
                    (when (and pos-opt (clime-option-locked pos-opt)) (setq pos-opt nil))
                    (unless pos-opt (setq pos-node (clime-node-parent pos-node)))))
                (when (and pos-opt (clime-option-negatable pos-opt))
                  (cons pos-opt (if pos-node 'negated-ancestor 'negated-current)))))))))))

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

(defun clime--try-consume-option (tok argv i len values option-parsing
                                      current-node root path)
  "Try to consume TOK as a known option from ARGV at index I.
Handles `--', `--help'/`-h', `--flag=value', and single known flags.
Returns (VALUES I OPTION-PARSING) if consumed, or nil if TOK is not
a recognized option.  Signals `clime-help-requested' for help flags.
LEN is the length of ARGV.  CURRENT-NODE and ROOT define option scope.
PATH is the command path for help signals."
  (cond
   ;; -- disables option parsing
   ((and option-parsing (string= tok "--"))
    (list values (1+ i) nil))
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
        (when (or (clime-option-boolean-p (car found))
                  (clime--negated-scope-p (cdr found)))
          (signal 'clime-usage-error
                  (list (format "Option %s does not take a value" flag))))
        (list (clime--consume-option (car found) values value)
              (1+ i)
              option-parsing))))
   ;; Known single option (including --no-X negation)
   ((and option-parsing
         (clime--option-like-p tok)
         (clime--find-option-in-scope tok current-node root))
    (let* ((found (clime--find-option-in-scope tok current-node root))
           (opt (car found))
           (negated (clime--negated-scope-p (cdr found))))
      (if (or negated (clime-option-boolean-p opt))
          (list (clime--consume-option opt values nil negated)
                (1+ i)
                option-parsing)
        ;; Value option: consume next token
        (let ((next-i (1+ i)))
          (when (>= next-i len)
            (signal 'clime-usage-error
                    (list (format "Option %s requires a value" tok))))
          (list (clime--consume-option opt values
                                       (clime--resolve-stdin-value (nth next-i argv)))
                (+ i 2)
                option-parsing)))))))

(defun clime--negated-scope-p (scope)
  "Return non-nil if SCOPE indicates a negated option lookup."
  (memq scope '(negated-current negated-ancestor)))

(defun clime--consume-option (opt values token-value &optional negated)
  "Consume option OPT into VALUES map, returning updated VALUES.
TOKEN-VALUE is the string value for value-taking options, or nil for booleans.
When NEGATED is non-nil, explicitly set the param to nil (for --no-X flags)."
  (let ((dep (clime-option-deprecated opt)))
    (when dep
      (let ((flag (car (clime-option-flags opt))))
        (message "Warning: %s is deprecated%s"
                 flag
                 (if (stringp dep) (format ". %s" dep) "")))))
  (let ((name (clime-option-name opt))
        (type (clime-option-type opt))
        (choices (clime-option-choices opt))
        (coerce (clime-option-coerce opt))
        val)
    (cond
     ;; Negated flag (--no-X): explicitly set to nil
     (negated
      (setq val nil))
     ;; Count option: increment
     ((clime-option-count opt)
      (setq val (1+ (or (clime-values-value values name) 0))))
     ;; Boolean (non-count): set t
     ((clime-option-boolean-p opt)
      (setq val t))
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
             (current (clime-values-value values name)))
        (setq val (append current vals))))
     ;; Normal value option
     (t
      (setq val (clime--transform-value token-value type choices coerce
                                        (car (clime-option-flags opt))))))
    (setf (clime-param-value opt) val
          (clime-param-source opt) 'user)
    (clime-values-set values name val 'user)))

(defun clime--required-args-satisfied-p (node values)
  "Return non-nil if all required positional args of NODE have values in VALUES."
  (let ((args (clime-node-args node)))
    (cl-every (lambda (arg)
                (or (not (clime-arg-required arg))
                    (clime-values-get values (clime-arg-name arg))))
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

(defun clime--apply-env (nodes values app)
  "Apply env var values for options in NODES not already in VALUES.
APP is the root app node (for :env-prefix).  Returns updated VALUES."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let* ((name (clime-option-name opt))
             (env-var (and (not (clime-values-get values name))
                           (clime--env-var-for-option opt app)))
             (value (and env-var (getenv env-var))))
        (when (and value (not (string-empty-p value)))
          (let ((env-val
                 (cond
                  ;; Count option: parse as integer
                  ((clime-option-count opt)
                   (clime--coerce-value value 'integer env-var))
                  ;; Boolean flag: parse truthy/falsy, falsy means unset
                  ((clime-option-boolean-p opt)
                   (and (clime--parse-boolean-env value env-var) t))
                  ;; Multiple option: split on separator (or comma), transform each
                  ((clime-option-multiple opt)
                   (let ((type (clime-option-type opt))
                         (choices (clime-option-choices opt))
                         (coerce (clime-option-coerce opt))
                         (sep (or (clime-option-separator opt) ",")))
                     (mapcar (lambda (v)
                               (clime--transform-value
                                (string-trim v) type choices coerce env-var))
                             (split-string value sep t))))
                  ;; Normal value option
                  (t
                   (clime--transform-value
                    value (clime-option-type opt)
                    (clime-option-choices opt)
                    (clime-option-coerce opt)
                    env-var)))))
            ;; Boolean falsy returns nil from parse-boolean-env — skip
            (when (or env-val (not (clime-option-boolean-p opt)))
              (setf (clime-param-value opt) env-val
                    (clime-param-source opt) 'env)
              (setq values (clime-values-set values name env-val 'env))))))))
  values)

(defun clime--apply-defaults (nodes values)
  "Apply default values for options and args in NODES not already in VALUES.
NODES is a list of nodes whose params to process.  Returns updated VALUES.
Walks inline group children to reach options in mutex/zip groups."
  (dolist (node nodes)
    (dolist (opt (clime-node-all-options node))
      (let ((name (clime-option-name opt)))
        (unless (clime-values-get values name)
          (let ((default (clime-option-default opt)))
            (when default
              (let ((val (clime--resolve-value default)))
                (setf (clime-param-value opt) val
                      (clime-param-source opt) 'default)
                (setq values (clime-values-set values name val 'default))))))))
    (dolist (arg (clime-node-args node))
      (let ((name (clime-arg-name arg)))
        (unless (clime-values-get values name)
          (let ((default (clime-arg-default arg)))
            (when default
              (let ((val (clime--resolve-value default)))
                (setf (clime-param-value arg) val
                      (clime-param-source arg) 'default)
                (setq values (clime-values-set values name val 'default)))))))))
  values)

(defun clime--check-required (nodes values path)
  "Check that all required params in NODES have values in VALUES.
PATH is the command path for error messages.  Signals `clime-usage-error'."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (when (and (clime-option-required opt)
                 (not (clime-values-get values (clime-option-name opt))))
        (signal 'clime-usage-error
                (list (format "Missing required option %s for %s"
                              (car (clime-option-flags opt))
                              (string-join path " "))))))
    (dolist (arg (clime-node-args node))
      (when (and (clime-arg-required arg)
                 (not (clime-values-get values (clime-arg-name arg))))
        (signal 'clime-usage-error
                (list (format "Missing required argument <%s> for %s"
                              (clime-arg-name arg)
                              (string-join path " "))))))))

(defun clime--find-unsatisfied-requires (nodes values)
  "Return list of error strings for unmet :requires constraints.
Checks all options across NODES against VALUES.  Returns nil when
all constraints are satisfied."
  (let ((all-opts '())
        (errors '()))
    ;; Collect all options for flag lookup
    (dolist (node nodes)
      (dolist (opt (clime-node-options node))
        (push opt all-opts)))
    ;; Check each option with :requires
    (dolist (node nodes)
      (dolist (opt (clime-node-options node))
        (when-let* ((reqs (clime-option-requires opt)))
          (when (clime-values-get values (clime-option-name opt))
            (let ((missing (cl-remove-if
                            (lambda (req-name)
                              (clime-values-get values req-name))
                            reqs)))
              (when missing
                (let* ((flag (car (clime-option-flags opt)))
                       (missing-flags
                        (mapcar (lambda (name)
                                  (let ((req-opt (cl-find-if
                                                  (lambda (o)
                                                    (eq (clime-option-name o) name))
                                                  all-opts)))
                                    (if req-opt
                                        (car (clime-option-flags req-opt))
                                      (format "--%s" name))))
                                missing)))
                  (push (format "%s requires %s"
                                flag
                                (mapconcat #'identity missing-flags ", "))
                        errors))))))))
    (nreverse errors)))

(defun clime--check-requires (nodes values)
  "Check :requires constraints across NODES for VALUES.
Signal `clime-usage-error' if an option with :requires is set but any
of its required options are absent from VALUES."
  (when-let ((errors (clime--find-unsatisfied-requires nodes values)))
    (signal 'clime-usage-error (list (car errors)))))

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
        (values '())
        (path (list (or (clime-app-argv0 app) (clime-app-name app))))
        (display-path (list (or (clime-app-argv0 app) (clime-app-name app))))
        (arg-index 0)
        (i 0)
        (len (length argv))
        (clime--stdin-app app))
    ;; Set parent refs for direct children (if not already set)
    (clime--set-parent-refs app)
    ;; Resolve alias commands (idempotent)
    (clime--resolve-aliases app)
    ;; Deep-copy tree so :value/:source mutations are isolated
    (let ((tree-copy (clime--deep-copy-tree app)))
      (setq app tree-copy
            current-node tree-copy
            root tree-copy))
    (condition-case err
        (progn
    (while (< i len)
      (let* ((token (nth i argv))
             (child-path (and (clime-branch-p current-node)
                              (clime--required-args-satisfied-p current-node values)
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
                                tok argv i len values option-parsing
                                current-node root path)))
                (if consumed
                    (setq values (nth 0 consumed)
                          i (nth 1 consumed)
                          option-parsing (nth 2 consumed))
                  ;; Not a known option: collect as rest value
                  (push (clime--resolve-stdin-value tok) rest-values)
                  (cl-incf i))))
            (let ((val (nreverse rest-values))
                  (name (clime-arg-name arg-spec)))
              (setf (clime-param-value arg-spec) val
                    (clime-param-source arg-spec) 'user)
              (setq values (clime-values-set values name val 'user)))))

         ;; 4. Option-like token
         ((and option-parsing (clime--option-like-p token))
          (let ((consumed (clime--try-consume-option
                           token argv i len values option-parsing
                           current-node root path)))
            (if consumed
                (setq values (nth 0 consumed)
                      i (nth 1 consumed)
                      option-parsing (nth 2 consumed))
              ;; Not a known option — try short bundle, then error
              (if-let* ((bundle (clime--expand-short-bundle token current-node root)))
                  (progn
                    (dolist (flag bundle)
                      (let ((found (clime--find-option-in-scope flag current-node root)))
                        (setq values (clime--consume-option (car found) values nil))))
                    (cl-incf i))
                ;; Unknown option error (includes --flag=value with unknown flag)
                (let ((split (clime--split-long-equals token)))
                  (signal 'clime-usage-error
                          (list (format "Unknown option %s for %s"
                                        (if split (car split) token)
                                        (string-join display-path " ")))))))))

         ;; 4. Try group descent (child-path includes inline groups)
         (child
          (dolist (node child-path)
            (setq current-node node)
            (setq path (append path (list (clime-node-name node))))
            (unless (clime-node-inline node)
              (setq display-path (append display-path (list (clime-node-name node))))))
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
                    (let ((name (clime-arg-name arg-spec)))
                      (setf (clime-param-value arg-spec) coerced
                            (clime-param-source arg-spec) 'user)
                      (setq values (clime-values-set values name coerced 'user))))
                  (cl-incf arg-index)
                  (cl-incf i))
              ;; No more arg specs
              (signal 'clime-usage-error
                      (list (format "Unexpected argument \"%s\" for %s"
                                    token (string-join display-path " "))))))))))

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

    ;; Build pass-1 result (derive params from values map)
    (let ((result (clime-parse-result--create
                   :command (if (clime-command-p current-node) current-node nil)
                   :node current-node
                   :path path
                   :display-path display-path
                   :params (clime-values-plist values)
                   :values values
                   :tree app)))
      (if skip-finalize
          result
        (clime-parse-finalize result))))
      ;; Enrich usage errors with the current parse path for hints
      (clime-usage-error
       (signal 'clime-usage-error
               (list (cadr err) display-path))))))

;;; ─── Pass-2 Finalization ────────────────────────────────────────────────

(defun clime--validate-dynamic-choices (nodes values)
  "Validate VALUES against dynamic (function) :choices in NODES.
Only checks options and args whose :choices slot is a function,
since static choices were already validated in pass 1."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let ((choices (clime-option-choices opt)))
        (when (functionp choices)
          (let ((val (clime-values-value values (clime-option-name opt)))
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
          (let ((val (clime-values-value values (clime-arg-name arg)))
                (resolved (funcall choices)))
            (when (and val resolved (not (member val resolved)))
              (signal 'clime-usage-error
                      (list (format "Invalid value \"%s\" for <%s> (choose from: %s)"
                                    val (clime-arg-name arg)
                                    (mapconcat (lambda (c) (format "%s" c))
                                               resolved ", ")))))))))))

(defun clime--call-conform (cfn val param)
  "Call conform function CFN with VAL and optionally PARAM.
If CFN accepts exactly 1 argument, call (CFN VAL).
Otherwise call (CFN VAL PARAM) for backward compatibility."
  (let ((max-args (cdr (func-arity cfn))))
    (if (and (numberp max-args) (= max-args 1))
        (funcall cfn val)
      (funcall cfn val param))))

(defun clime--run-conformers (nodes values)
  "Run :conform functions for options and args in NODES against VALUES.
Called in pass 2 after dynamic choices validation and env var application.
Skips params with no value.  Returns updated VALUES map.  Each conformer
receives (value) or (value, param) depending on arity, and returns the
conformed value; signaled errors are written as :error entries."
  (dolist (node nodes)
    (dolist (opt (clime-node-options node))
      (let ((cfn (clime-option-conform opt)))
        (when cfn
          (let* ((name (clime-option-name opt))
                 (val (clime-values-value values name)))
            (when val
              (condition-case err
                  (let* ((conformed (clime--call-conform cfn val opt))
                         (src (or (clime-values-source values name) 'user)))
                    (setf (clime-param-value opt) conformed)
                    (setq values (clime-values-set values name conformed src)))
                (error
                 (setq values
                       (clime-values-set-error
                        values name
                        (format "Invalid value for %s: %s"
                                (car (clime-option-flags opt))
                                (error-message-string err)))))))))))
    (dolist (arg (clime-node-args node))
      (let ((cfn (clime-arg-conform arg)))
        (when cfn
          (let* ((name (clime-arg-name arg))
                 (val (clime-values-value values name)))
            (when val
              (condition-case err
                  (let* ((conformed (clime--call-conform cfn val arg))
                         (src (or (clime-values-source values name) 'user)))
                    (setf (clime-param-value arg) conformed)
                    (setq values (clime-values-set values name conformed src)))
                (error
                 (setq values
                       (clime-values-set-error
                        values name
                        (format "Invalid value for <%s>: %s"
                                (clime-arg-name arg)
                                (error-message-string err))))))))))))
  values)

(defun clime--apply-node-conform (node values)
  "Run NODE's :conform functions on VALUES, return updated VALUES.
:conform is a list of functions, each taking (values, node).
Signaled errors are written as :error entries on attributed params,
or on a synthetic :conform-error/NODE-NAME key for unattributed errors."
  (let ((fns (clime-node-conform node)))
    (when fns
      (when (functionp fns) (setq fns (list fns)))
      (dolist (fn fns)
        (condition-case err
            (setq values (funcall fn values node))
          (clime-usage-error
           (let* ((data (cdr err))
                  (msg (car data))
                  (plist (cdr data))
                  (params-attr (plist-get plist :params)))
             (if params-attr
                 ;; Attributed: write error on each named param
                 (dolist (name params-attr)
                   (setq values (clime-values-set-error values name msg)))
               ;; Unattributed: write on synthetic key
               (let ((key (intern (format ":conform-error/%s" (clime-node-name node)))))
                 (setq values (clime-values-set-error values key msg))))))
          (error
           (let ((key (intern (format ":conform-error/%s" (clime-node-name node)))))
             (setq values (clime-values-set-error values key (error-message-string err)))))))))
  values)

(defun clime--conform-inline-children (node dispatch-nodes values)
  "Postorder-conform inline group children of NODE not on DISPATCH-NODES.
Recurses into nested inline groups before conforming each."
  (when (clime-group-p node)
    (dolist (entry (clime-group-children node))
      (let ((child (cdr entry)))
        (when (and (clime-group-p child)
                   (clime-node-inline child)
                   (not (memq child dispatch-nodes)))
          ;; Recurse first (postorder: children before parent)
          (setq values (clime--conform-inline-children child dispatch-nodes values))
          ;; Then conform this inline child
          (setq values (clime--apply-node-conform child values))))))
  values)

(defun clime--run-node-conformers (nodes values)
  "Postorder-conform visited NODES, also descending into inline groups.
NODES is the scope list (leaf first, from node ancestors).
At each node, inline group children not on the dispatch path are
conformed first (recursively, postorder), then the node itself."
  (dolist (node nodes)
    ;; Conform inline group children not on the dispatch path
    (setq values (clime--conform-inline-children node nodes values))
    ;; Conform the node itself
    (setq values (clime--apply-node-conform node values)))
  values)

(defun clime-parse-finalize (result)
  "Finalize RESULT from pass-1 parse (pass 2).
Validates dynamic choices, applies env vars, runs conformers, applies
defaults, and checks required params.  Returns the updated RESULT."
  (when (clime-parse-result-finalized result)
    (error "clime-parse-finalize: result already finalized"))
  (let* ((node (clime-parse-result-node result))
         (scope (cons node (clime-node-ancestors node)))
         (values (or (clime-parse-result-values result) '()))
         (display-path (clime-parse-result-display-path result))
         (root (clime-parse-result-tree result)))
    ;; Inject locked vals (from alias :vals resolution).
    ;; Walk leaf→root; two sources:
    ;; (1) locked options with :source 'app (from alias :vals),
    ;; (2) locked-vals alist on nodes (for group-level locked vals).
    (dolist (n scope)
      (dolist (opt (clime-node-all-options n))
        (when (and (clime-option-locked opt)
                   (eq (clime-param-source opt) 'app))
          (setq values (clime-values-merge values (clime-param-name opt)
                                           (clime-param-value opt) 'app))))
      (dolist (entry (clime-node-locked-vals n))
        (setq values (clime-values-merge values (car entry)
                                         (cdr entry) 'app))))
    ;; Validate dynamic choices (functions resolved now, after setup)
    (clime--validate-dynamic-choices scope values)
    ;; Apply env vars (external input, needs conforming)
    (setq values (clime--apply-env scope values root))
    ;; Run option/arg :conform functions (after env, before defaults —
    ;; defaults are developer-authored and should already be valid)
    (setq values (clime--run-conformers scope values))
    ;; Run node-level :conform (leaf→root walk, operates on values map)
    (setq values (clime--run-node-conformers scope values))
    ;; Store values on result before error check (so errors are inspectable)
    (setf (clime-parse-result-values result) values)
    ;; Check for accumulated conformer errors before proceeding
    (let ((errors (clime-values-errors values)))
      (when errors
        (signal 'clime-usage-error
                (list (mapconcat #'cdr errors "; ")))))
    ;; Check :requires constraints
    (clime--check-requires scope values)
    ;; Apply defaults
    (setq values (clime--apply-defaults scope values))
    (clime--check-required scope values display-path)
    ;; Derive params plist from values map and mark finalized
    (setf (clime-parse-result-params result) (clime-values-plist values))
    (setf (clime-parse-result-values result) values)
    (setf (clime-parse-result-finalized result) t)
    result))

(provide 'clime-parse)
;;; clime-parse.el ends here
