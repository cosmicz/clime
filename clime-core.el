;;; clime-core.el --- Command tree data model for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core data structures for the CLI command tree: options, arguments,
;; commands, groups, and the app root.  Provides constructors with
;; validation, predicates, and tree-query functions.

;;; Code:

(require 'cl-lib)

(declare-function json-encode "json")

;;; ─── Param (abstract base) ──────────────────────────────────────────────

(cl-defstruct (clime-param (:constructor nil)
                            (:copier nil))
  "Abstract base for anything that produces a value in params/ctx."
  (name nil :type symbol :documentation "Canonical param name.")
  (help nil :type (or string null) :documentation "One-line help text.")
  (required nil :type boolean)
  (default nil :documentation "Default value, or a function for lazy defaults."))

;;; ─── Option ─────────────────────────────────────────────────────────────

(cl-defstruct (clime-option (:include clime-param)
                            (:constructor clime-option--create)
                            (:copier nil))
  "A CLI option (flag) specification."
  (flags nil :type list :documentation "List of flag strings, e.g. (\"--verbose\" \"-v\").")
  (type 'string :type symbol :documentation "Type converter symbol.")
  (nargs nil :type (or integer null) :documentation "Arg count: nil=1, 0=boolean, N=fixed.")
  (env nil :type (or string null) :documentation "Env var name override.")
  (count nil :type boolean :documentation "If non-nil, flag is a counter (-vvv = 3).")
  (multiple nil :type boolean :documentation "If non-nil, repeated flags collect into a list.")
  (choices nil :documentation "Allowed values, or a function returning them (resolved at parse time).")
  (coerce nil :type (or function null) :documentation "Custom transform applied after type coercion.")
  (conform nil :type (or function null) :documentation "Pass-2 conformer; receives value, returns conformed value or signals error.")
  (separator nil :type (or string null) :documentation "Split each value by this string; implies :multiple t.")
  (category nil :type (or string null) :documentation "Help display category label.")
  (hidden nil :type boolean :documentation "If non-nil, omit from help.")
  (deprecated nil :documentation "Deprecation notice: string (migration hint) or t (generic warning).")
  (negatable nil :type boolean :documentation "If non-nil, auto-generate --no-X variant. Implies boolean (nargs 0).")
  (requires nil :type list :documentation "List of option name symbols that must also be set when this option is used."))

(defun clime-make-option (&rest args)
  "Create a `clime-option' with validation.
Required keyword args: :name (symbol), :flags (non-empty list of strings).
ARGS is a plist of slot values."
  (let ((name (plist-get args :name))
        (flags (plist-get args :flags)))
    (unless name
      (error "clime-make-option: :name is required"))
    (unless (and flags (listp flags) (> (length flags) 0))
      (error "clime-make-option: :flags must be a non-empty list of strings"))
    (when (plist-get args :negatable)
      (let ((nargs (plist-get args :nargs)))
        (when (and nargs (> nargs 0))
          (error "clime-make-option: :negatable is incompatible with :nargs %d" nargs)))
      (when (plist-get args :count)
        (error "clime-make-option: :negatable is incompatible with :count")))
    (clime--check-required-default "Option" args)
    (apply #'clime-option--create args)))

(defun clime--check-required-default (kind args)
  "Warn if ARGS plist has both :required and :default.
KIND is a string like \"Option\" or \"Arg\" for the warning message."
  (when (and (plist-get args :required) (plist-get args :default))
    (display-warning 'clime
      (format "%s `%s': :required is vacuous when :default is set"
              kind (plist-get args :name)))))

(defun clime--merge-template (template &rest overrides)
  "Merge option TEMPLATE plist with OVERRIDES plist.
OVERRIDES take precedence over TEMPLATE values."
  (let ((result (copy-sequence template)))
    (cl-loop for (key val) on overrides by #'cddr
             do (setq result (plist-put result key val)))
    result))

(defun clime-option-boolean-p (option)
  "Return non-nil if OPTION is a boolean/count flag (consumes no value)."
  (or (clime-option-count option)
      (clime-option-negatable option)
      (eq (clime-option-type option) 'boolean)
      (eql (clime-option-nargs option) 0)))

(defun clime--resolve-value (value)
  "Resolve VALUE, calling it if it is a function.
Used for lazy slots like :choices and :default."
  (if (functionp value) (funcall value) value))

;;; ─── Arg ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-arg (:include clime-param)
                         (:constructor clime-arg--create)
                         (:copier nil))
  "A CLI positional argument specification."
  (type 'string :type symbol :documentation "Type converter symbol.")
  (nargs nil :documentation "Arg count: nil=1, integer N, or :rest.")
  (choices nil :documentation "Allowed values, or a function returning them (resolved at parse time).")
  (coerce nil :type (or function null) :documentation "Custom transform applied after type coercion.")
  (conform nil :type (or function null) :documentation "Pass-2 conformer; receives value, returns conformed value or signals error.")
  (deprecated nil :documentation "Deprecation notice: string (migration hint) or t (generic warning)."))

(defun clime-make-arg (&rest args)
  "Create a `clime-arg' with validation.
Required keyword arg: :name (symbol).
ARGS is a plist of slot values.
Defaults :required to t (unlike options which default to nil)."
  (unless (plist-get args :name)
    (error "clime-make-arg: :name is required"))
  ;; Args default to required (unlike options/params)
  (unless (plist-member args :required)
    (setq args (plist-put (cl-copy-list args) :required t)))
  (clime--check-required-default "Arg" args)
  (apply #'clime-arg--create args))

;;; ─── Node-Level Conform Checks ──────────────────────────────────────────

(defun clime-check-exclusive (group-name member-names &optional default required)
  "Return a node conformer that enforces at-most-one exclusivity.
GROUP-NAME is a symbol for the derived value key.  MEMBER-NAMES is a list
of param name symbols (e.g. \\='(json csv)).  DEFAULT, when non-nil, is
injected under GROUP-NAME when no member is set.  REQUIRED, when non-nil,
signals an error if no member is set.  The returned function takes
\(params, node) and returns updated PARAMS with the winner's name
injected under GROUP-NAME."
  (lambda (params _node)
    (let ((set-names (cl-remove-if-not
                      (lambda (k) (plist-member params k))
                      member-names)))
      (when (> (length set-names) 1)
        (signal 'clime-usage-error
                (list (format "Options %s are mutually exclusive"
                              (mapconcat (lambda (k) (format "%s" k))
                                         set-names ", ")))))
      (cond
       ((= (length set-names) 1)
        (let ((winner (car set-names)))
          (if (plist-get params winner)
              ;; Truthy winner: suppress defaults on siblings, set derived key
              (progn
                (dolist (k member-names)
                  (unless (or (eq k winner) (plist-member params k))
                    (setq params (plist-put params k nil))))
                (plist-put params group-name winner))
            ;; Falsy (e.g. negated flag): explicitly set but not a winner —
            ;; don't suppress siblings or inject derived key
            params)))
       (required
        (signal 'clime-usage-error
                (list (format "One of %s is required"
                              (mapconcat (lambda (k) (format "%s" k))
                                         member-names ", ")))))
       (default
        (plist-put params group-name default))
       (t params)))))

(defun clime-check-paired (group-name member-names &optional required)
  "Return a node conformer that enforces all-or-none with equal cardinality.
GROUP-NAME is a symbol for the zipped value key.  MEMBER-NAMES is a list
of param name symbols (e.g. \\='(skip reason)).  REQUIRED, when non-nil,
signals an error if no members are set.  The returned function takes
\(params, node) and returns updated PARAMS with a zipped alist injected
under GROUP-NAME."
  (lambda (params _node)
    (let* ((set-names (cl-remove-if-not
                       (lambda (k) (plist-member params k))
                       member-names))
           (any-set (> (length set-names) 0))
           (all-set (= (length set-names) (length member-names))))
      ;; Required check
      (when (and required (not any-set))
        (signal 'clime-usage-error
                (list (format "Options %s are required"
                              (mapconcat (lambda (k) (format "%s" k))
                                         member-names ", ")))))
      ;; Partial presence check
      (when (and any-set (not all-set))
        (let ((missing (cl-remove-if
                        (lambda (k) (plist-member params k))
                        member-names)))
          (signal 'clime-usage-error
                  (list (format "%s requires %s"
                                (car set-names)
                                (mapconcat (lambda (k) (format "%s" k))
                                           missing ", "))))))
      ;; Cardinality check
      (when all-set
        (let ((counts (mapcar (lambda (k)
                                (let ((val (plist-get params k)))
                                  (if (listp val) (length val) 0)))
                              member-names)))
          (unless (apply #'= counts)
            (let ((parts (mapcar (lambda (k)
                                   (format "%s (%d)" k
                                           (let ((val (plist-get params k)))
                                             (if (listp val) (length val) 0))))
                                 member-names)))
              (signal 'clime-usage-error
                      (list (format "Paired options must be used the same number of times: %s"
                                    (mapconcat #'identity parts ", "))))))
          ;; Build zipped alist
          (let* ((n (car counts))
                 (zipped '()))
            (dotimes (i n)
              (let ((row '()))
                (dolist (k member-names)
                  (let ((vals (plist-get params k)))
                    (push (cons k (nth i vals)) row)))
                (push (nreverse row) zipped)))
            (plist-put params group-name (nreverse zipped))))))))

;;; ─── Node (base) ────────────────────────────────────────────────────────

(cl-defstruct (clime-node (:constructor nil)
                          (:copier nil))
  "Abstract base for CLI tree nodes (commands, groups, apps)."
  (name nil :type string :documentation "Node name as typed by user.")
  (aliases nil :type list :documentation "Alternative names for this node.")
  (help nil :type (or string null) :documentation "One-line description.")
  (options nil :type list :documentation "List of `clime-option' structs.")
  (args nil :type list :documentation "Ordered list of `clime-arg' structs.")
  (parent nil :documentation "Parent node ref, or nil for root.")
  (hidden nil :type boolean :documentation "If non-nil, omit from help.")
  (category nil :type (or string null) :documentation "Help display category label.")
  (inline nil :type boolean :documentation "If non-nil, promote children to parent level for dispatch and help.")
  (handler nil :type (or function null) :documentation "Handler function, called with context.")
  (epilog nil :type (or string null) :documentation "Free-form text appended after auto-generated help.")
  (deprecated nil :documentation "Deprecation notice: string (migration hint) or t (generic warning).")
  (locked-vals nil :type list :documentation "Alist of (name . value) injected into params during finalize.  Locked vals hide their options from CLI and help.")
  (conform nil :type (or function list null)
           :documentation "Transform/validate hook (run during finalization).
Single function or list of functions, each: (params, node) → params.
For lists, functions are called in sequence, threading params."))

;;; ─── Alias ──────────────────────────────────────────────────────────────

(cl-defstruct (clime-alias (:include clime-node)
                           (:constructor clime-alias--create)
                           (:copier nil))
  "Alias reference to another node in the CLI tree.
Lives in `children' before resolution.  During `clime--resolve-aliases',
each alias is replaced with a copy of its target command (or group)."
  (target nil :type list :documentation "Path to target command, e.g. (\"agents\" \"start\").")
  (defaults nil :type list :documentation "Alist of (name . value) to override defaults on copied options.")
  (vals nil :type list :documentation "Alist of (name . value) for locked params, hidden from CLI and help."))

;;; ─── Command ────────────────────────────────────────────────────────────

(cl-defstruct (clime-command (:include clime-node)
                             (:constructor clime-command--create)
                             (:copier nil))
  "A leaf command in the CLI tree.")

(defun clime-make-command (&rest args)
  "Create a `clime-command' with validation.
Required keyword args: :name (string), :handler (function).
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-command: :name is required"))
  (unless (plist-get args :handler)
    (error "clime-make-command: :handler is required"))
  (apply #'clime-command--create args))

;;; ─── Group ──────────────────────────────────────────────────────────────

(cl-defstruct (clime-group (:include clime-node)
                           (:constructor clime-group--create)
                           (:copier nil))
  "A branch node (subcommand group) in the CLI tree."
  (children nil :type list :documentation "Alist of (name . node) for subcommands/subgroups."))

(defun clime--set-direct-parents (node)
  "Set the :parent of each direct child in NODE's children alist to NODE."
  (when (clime-group-p node)
    (dolist (entry (clime-group-children node))
      (setf (clime-node-parent (cdr entry)) node))))

(defun clime--set-parent-refs-1 (node)
  "Recursively set parent refs for children of NODE."
  (when (clime-group-p node)
    (dolist (entry (clime-group-children node))
      (let ((child (cdr entry)))
        (setf (clime-node-parent child) node)
        (clime--set-parent-refs-1 child)))))

(defun clime--set-parent-refs (node)
  "Recursively set parent refs for children of NODE.
Also runs ancestor collision checks after parent refs are established."
  (clime--set-parent-refs-1 node)
  (clime-check-ancestor-collisions node))

;;; ─── Alias Resolution ───────────────────────────────────────────────────

(defun clime--resolve-alias-1 (alias app visited)
  "Resolve a single ALIAS node against APP root.
VISITED is a list of already-visited node names for cycle detection.
Returns the resolved target command (not an alias)."
  (let ((target-path (clime-alias-target alias)))
    (unless target-path
      (error "clime--resolve-alias-1: alias %s has no target" (clime-node-name alias)))
    (when (member (clime-node-name alias) visited)
      (error "Circular alias chain: %s" (string-join (append visited (list (clime-node-name alias))) " → ")))
    ;; Walk the path from root
    (let ((node app))
      (dolist (step target-path)
        (let ((child (and (clime-group-p node)
                          (clime-group-find-child node step))))
          (unless child
            (error "Alias %s: target path %s not found at step \"%s\""
                   (clime-node-name alias)
                   (mapconcat #'identity target-path " → ")
                   step))
          (setq node child)))
      ;; Must resolve to a command, not a group
      (unless (clime-command-p node)
        (error "Alias %s: target %s is not a command"
               (clime-node-name alias)
               (mapconcat #'identity target-path " → ")))
      ;; If target is itself an alias, resolve transitively
      (if (clime-alias-p node)
          (clime--resolve-alias-1 node app (cons (clime-node-name alias) visited))
        node))))

(defun clime--resolve-aliases (app)
  "Resolve all alias commands in APP's tree.
Walk the tree, find commands with non-nil `alias', resolve them
by copying args, options, and handler from the target.  Idempotent."
  (clime--resolve-aliases-walk app app))

(defun clime--resolve-aliases-walk (node app)
  "Walk NODE's subtree resolving aliases against APP root.
Alias nodes are replaced in-place in the children alist with
resolved command copies."
  (when (clime-group-p node)
    (dolist (entry (clime-group-children node))
      (let ((child (cdr entry)))
        (cond
         ;; Alias node: resolve and replace
         ((clime-alias-p child)
          (let* ((target (clime--resolve-alias-1 child app nil))
                 (defaults (clime-alias-defaults child))
                 (vals (clime-alias-vals child))
                 ;; Build resolved command with alias's metadata overlay
                 (resolved (clime-command--create
                            :name (clime-node-name child)
                            :handler (clime-node-handler target)
                            :args (clime-node-args target)
                            :options (copy-sequence (clime-node-options target))
                            :help (or (clime-node-help child)
                                      (clime-node-help target))
                            :epilog (or (clime-node-epilog child)
                                        (clime-node-epilog target))
                            :category (clime-node-category child)
                            :hidden (clime-node-hidden child)
                            :aliases (clime-node-aliases child)
                            :deprecated (clime-node-deprecated child))))
            ;; Apply :defaults — patch :default on copied options
            (dolist (dfl defaults)
              (let* ((name (car dfl))
                     (val (cdr dfl))
                     (opt (cl-find-if (lambda (o) (eq (clime-option-name o) name))
                                      (clime-node-options resolved))))
                (unless opt
                  (error "Alias %s: :defaults names unknown option `%s'"
                         (clime-node-name child) name))
                ;; Replace with a copy to avoid mutating target's option
                (let ((new-opt (copy-sequence opt)))
                  (setf (clime-option-default new-opt) val)
                  (setf (clime-node-options resolved)
                        (mapcar (lambda (o) (if (eq o opt) new-opt o))
                                (clime-node-options resolved))))))
            ;; Apply :vals — remove options from list (hidden from CLI/help)
            (dolist (val-entry vals)
              (let* ((name (car val-entry))
                     (opt (cl-find-if (lambda (o) (eq (clime-option-name o) name))
                                      (clime-node-options resolved))))
                (unless opt
                  (error "Alias %s: :vals names unknown option `%s'"
                         (clime-node-name child) name))
                (setf (clime-node-options resolved)
                      (cl-remove-if (lambda (o) (eq (clime-option-name o) name))
                                    (clime-node-options resolved)))))
            ;; Store vals for injection during finalize
            (when vals
              (setf (clime-node-locked-vals resolved) vals))
            ;; Replace alias with resolved command in children
            (setcdr entry resolved)
            ;; Set parent ref (set-parent-refs ran before resolution)
            (setf (clime-node-parent resolved) node)))
         ;; Group: recurse
         ((clime-group-p child)
          (clime--resolve-aliases-walk child app)))))))

(defun clime-make-group (&rest args)
  "Create a `clime-group' with validation.
Required keyword arg: :name (string).
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-group: :name is required"))
  (let ((group (apply #'clime-group--create args)))
    (clime--set-direct-parents group)
    group))

(defun clime-branch-p (node)
  "Return non-nil if NODE is a branch (group or app, not a command)."
  (and (clime-group-p node)
       (not (clime-command-p node))))

(define-obsolete-function-alias 'clime-group-only-p #'clime-branch-p "0.3.0")

;;; ─── Output Format ──────────────────────────────────────────────────────

(cl-defstruct (clime-output-format (:include clime-option)
                                    (:constructor clime-output-format--create)
                                    (:copier nil))
  "Output format option.  Inherits all `clime-option' slots.
Declares an output mode (e.g. JSON, YAML) as a CLI flag with
per-format finalize and streaming behavior."
  (finalize nil :type (or function null) :documentation "Envelope function: (items retval errors) → data | nil.")
  (streaming nil :type boolean :documentation "When non-nil, `clime-output' emits immediately (no accumulator).")
  (encoder nil :type (or function null) :documentation "Encoder function: data → string.")
  (error-handler nil :type (or function null) :documentation "Error handler: (msg) → side effect.  Called by `clime-output-error'."))

(defun clime-make-output-format (&rest args)
  "Create a `clime-output-format' with defaults.
Sets :nargs 0 (boolean) and :category \"Output\" unless overridden.
Defaults :encoder to JSON encoder and :error-handler to JSON error
emitter when not provided.
ARGS is a plist of slot values."
  (unless (plist-member args :nargs)
    (setq args (plist-put args :nargs 0)))
  (unless (plist-member args :category)
    (setq args (plist-put args :category "Output")))
  (unless (plist-member args :encoder)
    (setq args (plist-put args :encoder
                          (lambda (data) (concat (json-encode data) "\n")))))
  (unless (plist-member args :error-handler)
    (setq args (plist-put args :error-handler
                          (lambda (msg) (princ (concat (json-encode `((error . ,msg))) "\n"))))))
  (apply #'clime-output-format--create args))

;;; ─── App ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-app (:include clime-group)
                         (:constructor clime-app--create)
                         (:copier nil))
  "Root application node.  Extends `clime-group' with app-level metadata."
  (version nil :type (or string null) :documentation "Application version string.")
  (env-prefix nil :type (or string null) :documentation "Prefix for auto-derived env var names.")
  (json-mode nil :type boolean :documentation "Whether --json is a built-in root option.  Deprecated: use `clime-output-format'.")
  (output-formats nil :type list :documentation "List of `clime-output-format' structs.")
  (argv0 nil :type (or string null) :documentation "Program name for usage output (set from CLIME_ARGV0).")
  (setup nil :type (or function null) :documentation "Hook called after pass-1 parse, before dynamic validation and handler."))

(defun clime-make-app (&rest args)
  "Create a `clime-app' with validation.
Required keyword arg: :name (string).
When :json-mode is non-nil, a default JSON `clime-output-format' is
synthesized (deprecated; prefer `clime-output-format' DSL form).
Output formats are added to both :output-formats and :options.
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-app: :name is required"))
  (let ((output-formats (plist-get args :output-formats))
        (json-mode (plist-get args :json-mode)))
    ;; Error if both :json-mode and an explicit json output-format
    (when (and json-mode
               (cl-some (lambda (fmt) (eq (clime-output-format-name fmt) 'json))
                        output-formats))
      (error "clime-make-app: cannot use both :json-mode and clime-output-format for json"))
    ;; :json-mode t → synthesize default json output-format
    (when (and json-mode
               (not (cl-some (lambda (fmt) (eq (clime-output-format-name fmt) 'json))
                             output-formats)))
      (push (clime-make-output-format :name 'json
                                       :flags '("--json")
                                       :help "Output as JSON")
            output-formats)
      (setq args (plist-put args :output-formats output-formats)))
    ;; Add output-formats to :options (they ARE options)
    (when output-formats
      (let* ((existing-options (plist-get args :options))
             (new-opts (cl-remove-if
                        (lambda (fmt)
                          (cl-some (lambda (opt)
                                     (equal (clime-option-flags opt)
                                            (clime-option-flags fmt)))
                                   existing-options))
                        output-formats)))
        (setq args (plist-put args :options
                              (append existing-options new-opts))))
      ;; Auto-exclusivity: 2+ output formats get clime-check-exclusive
      (when (>= (length output-formats) 2)
        (let* ((member-names (mapcar #'clime-option-name output-formats))
               (exclusive-fn (clime-check-exclusive 'clime--output-format member-names))
               (existing-conform (plist-get args :conform)))
          (setq args (plist-put args :conform
                                (if existing-conform
                                    (if (functionp existing-conform)
                                        (list existing-conform exclusive-fn)
                                      (append existing-conform (list exclusive-fn)))
                                  exclusive-fn)))))))
  (let ((app (apply #'clime-app--create args)))
    (clime--set-direct-parents app)
    (clime--resolve-aliases app)
    (clime--validate-tree app)
    app))

;;; ─── Context ────────────────────────────────────────────────────────────

(cl-defstruct (clime-context (:constructor clime-context--create)
                             (:copier nil))
  "Runtime context passed to command handlers."
  (app nil :documentation "The root `clime-app'.")
  (command nil :documentation "The resolved `clime-command'.")
  (path nil :type list :documentation "List of node names from root to command.")
  (params nil :type list :documentation "Plist of parsed param values."))

(defun clime-ctx-get (ctx name)
  "Get the value of param NAME from context CTX."
  (plist-get (clime-context-params ctx) name))

(defun clime-param (ctx name &optional default)
  "Get param NAME from CTX, returning DEFAULT if not set.
Unlike `clime-ctx-get', distinguishes between explicitly nil and
absent — returns DEFAULT only when NAME is not in the params plist.
Useful for negatable flags where nil means explicitly disabled."
  (let ((params (clime-context-params ctx)))
    (if (plist-member params name)
        (plist-get params name)
      default)))

(defmacro clime-let (ctx bindings &rest body)
  "Destructure context CTX into BINDINGS and evaluate BODY.
Each element of BINDINGS is either:
  SYMBOL          — binds SYMBOL to (clime-ctx-get CTX \\='SYMBOL)
  (VAR PARAM)     — binds VAR to (clime-ctx-get CTX \\='PARAM)

Example:
  (clime-let ctx (package force (tags tag))
    (message \"%s %s %s\" package force tags))"
  (declare (indent 2))
  (let ((ctx-sym (if (symbolp ctx) ctx (gensym "ctx"))))
    `(let ,(mapcar (lambda (binding)
                     (if (consp binding)
                         `(,(car binding) (clime-ctx-get ,ctx-sym ',(cadr binding)))
                       `(,binding (clime-ctx-get ,ctx-sym ',binding))))
                   bindings)
       ,@body)))

(defun clime-params-plist (ctx &rest names)
  "Return keyword plist from CTX params.
With NAMES, include only those params (omitting nil values);
otherwise include all params as-is."
  (let ((params (clime-context-params ctx))
        (result '()))
    (if names
        (dolist (name names)
          (let ((val (plist-get params name)))
            (when val
              (push (intern (format ":%s" name)) result)
              (push val result))))
      (cl-loop for (key val) on params by #'cddr
               do (push (intern (format ":%s" key)) result)
                  (push val result)))
    (nreverse result)))

;;; ─── Tree Queries ───────────────────────────────────────────────────────

(defun clime-node-find-option (node flag)
  "Find the `clime-option' in NODE whose flags contain FLAG string.
Return the option struct, or nil if not found."
  (cl-find-if (lambda (opt)
                (member flag (clime-option-flags opt)))
              (clime-node-options node)))

(defun clime-group-find-child (group name)
  "Find a child node in GROUP by NAME or alias.
If not found directly, falls through to the default group's children.
Return the child node, or nil if not found."
  (let ((children (clime-group-children group)))
    (or (cdr (assoc name children))
        (cl-some (lambda (entry)
                   (let ((child (cdr entry)))
                     (when (member name (clime-node-aliases child))
                       child)))
                 children)
        ;; Fall through to default group's children
        (cl-some (lambda (entry)
                   (let ((child (cdr entry)))
                     (when (and (clime-group-p child)
                                (clime-node-inline child))
                       (clime-group-find-child child name))))
                 children))))

(defun clime-group-find-child-path (group name)
  "Find a child node in GROUP by NAME, returning the full descent path.
Like `clime-group-find-child', but returns a list of all nodes traversed
when descending through inline groups.  Returns nil if not found.
For direct children, returns a single-element list.
For inline group lookups, includes the inline group(s) and the leaf."
  (let ((children (clime-group-children group)))
    (or
     ;; Direct match by name
     (let ((entry (assoc name children)))
       (when entry (list (cdr entry))))
     ;; Match by alias
     (cl-some (lambda (entry)
                (let ((child (cdr entry)))
                  (when (member name (clime-node-aliases child))
                    (list child))))
              children)
     ;; Fall through to inline group's children
     (cl-some (lambda (entry)
                (let ((child (cdr entry)))
                  (when (and (clime-group-p child)
                             (clime-node-inline child))
                    (let ((sub-path (clime-group-find-child-path child name)))
                      (when sub-path
                        (cons child sub-path))))))
              children))))

(defun clime-node-collect (node &rest args)
  "Collect items from NODE's subtree into a flat list.
Each item is (TYPE ITEM ...) where TYPE is :option, :command, or :group.
For :option items, the owning node is included: (:option OPT OWNER-NODE).

Keyword ARGS:
  :recurse-p — predicate on child node; which groups to descend into.
               Default: `clime-node-inline'.
  :match-p   — predicate on (TYPE ITEM); which items to include.
               Default: include all non-hidden.
  :max-depth — integer recursion limit, nil for unlimited."
  (let ((recurse-p (or (plist-get args :recurse-p) #'clime-node-inline))
        (match-p (plist-get args :match-p))
        (max-depth (plist-get args :max-depth)))
    (clime-node-collect--1 node recurse-p match-p max-depth 0)))

(defun clime-node-collect--1 (node recurse-p match-p max-depth depth)
  "Internal recursive collector for `clime-node-collect'.
NODE is the current group, DEPTH tracks recursion level."
  (let ((items '())
        (at-depth-limit (and max-depth (>= depth max-depth))))
    ;; Collect options from this node (with owning node ref)
    (dolist (opt (clime-node-options node))
      (unless (clime-option-hidden opt)
        (when (or (null match-p) (funcall match-p :option opt))
          (push (list :option opt node) items))))
    ;; Walk children
    (when (clime-branch-p node)
      (dolist (entry (clime-group-children node))
        (let ((child (cdr entry)))
          (cond
           ;; Recurse into matching groups (unless at depth limit)
           ((and (not at-depth-limit)
                 (clime-group-p child)
                 (funcall recurse-p child))
            (when (or (null match-p) (funcall match-p :group child))
              (push (list :group child) items))
            (let ((sub (clime-node-collect--1 child recurse-p match-p
                                              max-depth (1+ depth))))
              (setq items (nconc (nreverse items) sub))
              (setq items (nreverse items))))
           ;; Non-inline group or command child
           (t
            (unless (clime-node-hidden child)
              (when (or (null match-p) (funcall match-p :command child))
                (push (list :command child) items))))))))
    (nreverse items)))

(defun clime-node-ancestors (node)
  "Return list of ancestor nodes from NODE's parent to root."
  (let ((parent (clime-node-parent node))
        (ancestors '()))
    (while parent
      (push parent ancestors)
      (setq parent (clime-node-parent parent)))
    (nreverse ancestors)))

(defun clime-node-all-ancestor-flags (node)
  "Collect all option flags from ancestors of NODE.
Walk the parent chain, collecting each ancestor's option flags into a flat list."
  (cl-mapcan (lambda (ancestor)
               (cl-mapcan (lambda (opt)
                            (copy-sequence (clime-option-flags opt)))
                          (clime-node-options ancestor)))
             (clime-node-ancestors node)))

;;; ─── Tree Validation ────────────────────────────────────────────────────

(defun clime--validate-node-options (node)
  "Check NODE's options for duplicate flags and duplicate names.
Signals `error' on duplicates."
  (let ((flags (make-hash-table :test #'equal))
        (names (make-hash-table :test #'eq))
        (node-name (clime-node-name node)))
    (dolist (opt (clime-node-options node))
      ;; Duplicate option names
      (let ((name (clime-option-name opt)))
        (when (gethash name names)
          (error "Duplicate option name `%s' on node %s" name node-name))
        (puthash name t names))
      ;; Duplicate flags
      (dolist (flag (clime-option-flags opt))
        (when (gethash flag flags)
          (error "Duplicate flag %s on node %s" flag node-name))
        (puthash flag t flags)))))

(defun clime--validate-node-children (node)
  "Check NODE's children for duplicate names."
  (when (clime-group-p node)
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (entry (clime-group-children node))
        (let ((name (car entry)))
          (when (gethash name seen)
            (error "Duplicate child name \"%s\" in group %s"
                   name (clime-node-name node)))
          (puthash name t seen))))))

(defun clime--validate-tree (root)
  "Validate ROOT and all descendants.
Checks per-node duplicates and ancestor flag collisions."
  (clime--validate-tree-1 root nil nil))

(defun clime--validate-tree-1 (node ancestor-flag-owners path)
  "Recursive walker for `clime--validate-tree'.
NODE is the current node.  ANCESTOR-FLAG-OWNERS and PATH track
ancestor collisions."
  (let* ((name (clime-node-name node))
         (path (append (or path '()) (list name))))
    ;; Per-node structural checks
    (clime--validate-node-options node)
    (clime--validate-node-children node)
    ;; Ancestor flag collision check
    (let ((local-flags (cl-mapcan (lambda (opt)
                                    (copy-sequence (clime-option-flags opt)))
                                  (clime-node-options node))))
      (dolist (flag local-flags)
        (let ((collision (assoc flag ancestor-flag-owners)))
          (when collision
            (error "Flag collision: %s in %s collides with ancestor %s"
                   flag (string-join path " ") (cdr collision)))))
      ;; Recurse into children
      (when (clime-branch-p node)
        (let ((merged (append (mapcar (lambda (f) (cons f name)) local-flags)
                              ancestor-flag-owners)))
          (dolist (entry (clime-group-children node))
            (clime--validate-tree-1 (cdr entry) merged path)))))))

;;; ─── Collision Checks ──────────────────────────────────────────────────

(defun clime-check-ancestor-collisions (node &optional ancestor-flag-owners path)
  "Check NODE's option flags for collisions with ancestor flags.
ANCESTOR-FLAG-OWNERS is an alist of (flag . ancestor-name) accumulated
from parent scopes.  PATH is a list of node name strings for error messages.
Signals `error' if a local flag collides with an ancestor flag.
Sibling collisions are allowed."
  (let* ((name (clime-node-name node))
         (path (append (or path '()) (list name)))
         (local-flags (cl-mapcan (lambda (opt)
                                   (copy-sequence (clime-option-flags opt)))
                                 (clime-node-options node))))
    ;; Check each local flag against ancestors
    (dolist (flag local-flags)
      (let ((collision (assoc flag ancestor-flag-owners)))
        (when collision
          (error "Flag collision: %s in %s collides with ancestor %s"
                 flag (string-join path " ") (cdr collision)))))
    ;; Build merged flag owners for children
    (let ((merged (append (mapcar (lambda (f) (cons f name)) local-flags)
                          ancestor-flag-owners)))
      (when (clime-branch-p node)
        (dolist (entry (clime-group-children node))
          (clime-check-ancestor-collisions (cdr entry) merged path))))))

(provide 'clime-core)
;;; clime-core.el ends here
