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

;;; ─── Option ─────────────────────────────────────────────────────────────

(cl-defstruct (clime-option (:constructor clime-option--create)
                            (:copier nil))
  "A CLI option (flag) specification."
  (name nil :type symbol :documentation "Canonical param name.")
  (flags nil :type list :documentation "List of flag strings, e.g. (\"--verbose\" \"-v\").")
  (type 'string :type symbol :documentation "Type converter symbol.")
  (help nil :type (or string null) :documentation "One-line help text.")
  (required nil :type boolean)
  (default nil :documentation "Default value, or a function for lazy defaults.")
  (nargs nil :type (or integer null) :documentation "Arg count: nil=1, 0=boolean, N=fixed.")
  (env nil :type (or string null) :documentation "Env var name override.")
  (count nil :type boolean :documentation "If non-nil, flag is a counter (-vvv = 3).")
  (multiple nil :type boolean :documentation "If non-nil, repeated flags collect into a list.")
  (choices nil :documentation "Allowed values, or a function returning them (resolved at parse time).")
  (coerce nil :type (or function null) :documentation "Custom transform applied after type coercion.")
  (validate nil :type (or function null) :documentation "Pass-2 validator; receives final value, should signal error on failure.")
  (separator nil :type (or string null) :documentation "Split each value by this string; implies :multiple t.")
  (category nil :type (or string null) :documentation "Help display category label.")
  (hidden nil :type boolean :documentation "If non-nil, omit from help."))

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
    (apply #'clime-option--create args)))

(defun clime-option-boolean-p (option)
  "Return non-nil if OPTION is a boolean/count flag (consumes no value)."
  (or (clime-option-count option)
      (eq (clime-option-type option) 'boolean)
      (eql (clime-option-nargs option) 0)))

(defun clime--resolve-value (value)
  "Resolve VALUE, calling it if it is a function.
Used for lazy slots like :choices and :default."
  (if (functionp value) (funcall value) value))

;;; ─── Arg ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-arg (:constructor clime-arg--create)
                         (:copier nil))
  "A CLI positional argument specification."
  (name nil :type symbol :documentation "Canonical param name.")
  (type 'string :type symbol :documentation "Type converter symbol.")
  (help nil :type (or string null) :documentation "One-line help text.")
  (required t :type boolean)
  (default nil :documentation "Default value, or a function for lazy defaults.")
  (nargs nil :documentation "Arg count: nil=1, integer N, or :rest.")
  (choices nil :documentation "Allowed values, or a function returning them (resolved at parse time).")
  (coerce nil :type (or function null) :documentation "Custom transform applied after type coercion.")
  (validate nil :type (or function null) :documentation "Pass-2 validator; receives final value, should signal error on failure."))

(defun clime-make-arg (&rest args)
  "Create a `clime-arg' with validation.
Required keyword arg: :name (symbol).
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-arg: :name is required"))
  (apply #'clime-arg--create args))

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
  (epilog nil :type (or string null) :documentation "Free-form text appended after auto-generated help."))

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

;;; ─── App ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-app (:include clime-group)
                         (:constructor clime-app--create)
                         (:copier nil))
  "Root application node.  Extends `clime-group' with app-level metadata."
  (version nil :type (or string null) :documentation "Application version string.")
  (env-prefix nil :type (or string null) :documentation "Prefix for auto-derived env var names.")
  (json-mode nil :type boolean :documentation "Whether --json is a built-in root option.")
  (argv0 nil :type (or string null) :documentation "Program name for usage output (set from CLIME_ARGV0).")
  (setup nil :type (or function null) :documentation "Hook called after pass-1 parse, before dynamic validation and handler."))

(defun clime-make-app (&rest args)
  "Create a `clime-app' with validation.
Required keyword arg: :name (string).
When :json-mode is non-nil, a --json boolean option is auto-injected.
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-app: :name is required"))
  ;; Auto-inject --json option when :json-mode is enabled
  (when (and (plist-get args :json-mode)
             (not (cl-some (lambda (opt)
                             (member "--json" (clime-option-flags opt)))
                           (plist-get args :options))))
    (let ((opt (clime-make-option :name 'json
                                  :flags '("--json")
                                  :nargs 0
                                  :help "Output as JSON"
                                  :category "Output")))
      (setq args (plist-put args :options
                            (append (plist-get args :options) (list opt))))))
  (let ((app (apply #'clime-app--create args)))
    (clime--set-direct-parents app)
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
