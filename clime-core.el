;;; clime-core.el --- Command tree data model for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Octavian <cosmicz@protonmail.com>

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
  (group nil :type (or string null) :documentation "Help display group label.")
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

;;; ─── Arg ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-arg (:constructor clime-arg--create)
                         (:copier nil))
  "A CLI positional argument specification."
  (name nil :type symbol :documentation "Canonical param name.")
  (type 'string :type symbol :documentation "Type converter symbol.")
  (help nil :type (or string null) :documentation "One-line help text.")
  (required t :type boolean)
  (default nil :documentation "Default value, or a function for lazy defaults.")
  (nargs nil :documentation "Arg count: nil=1, integer N, or :rest."))

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
  (handler nil :type (or function null) :documentation "Handler function, called with context."))

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

(defun clime-make-group (&rest args)
  "Create a `clime-group' with validation.
Required keyword arg: :name (string).
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-group: :name is required"))
  (apply #'clime-group--create args))

(defun clime-group-only-p (node)
  "Return non-nil if NODE is a group but not a command.
True for groups and apps, false for commands."
  (and (clime-group-p node)
       (not (clime-command-p node))))

;;; ─── App ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-app (:include clime-group)
                         (:constructor clime-app--create)
                         (:copier nil))
  "Root application node.  Extends `clime-group' with app-level metadata."
  (version nil :type (or string null) :documentation "Application version string.")
  (env-prefix nil :type (or string null) :documentation "Prefix for auto-derived env var names.")
  (json-mode nil :type boolean :documentation "Whether --json is a built-in root option.")
  (argv0 nil :type (or string null) :documentation "Program name for usage output (set from CLIME_ARGV0)."))

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
                                  :group "Output")))
      (setq args (plist-put args :options
                            (append (plist-get args :options) (list opt))))))
  (apply #'clime-app--create args))

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

;;; ─── Tree Queries ───────────────────────────────────────────────────────

(defun clime-node-find-option (node flag)
  "Find the `clime-option' in NODE whose flags contain FLAG string.
Return the option struct, or nil if not found."
  (cl-find-if (lambda (opt)
                (member flag (clime-option-flags opt)))
              (clime-node-options node)))

(defun clime-group-find-child (group name)
  "Find a child node in GROUP by NAME or alias.
Return the child node, or nil if not found."
  (let ((children (clime-group-children group)))
    (or (cdr (assoc name children))
        (cl-some (lambda (entry)
                   (let ((child (cdr entry)))
                     (when (member name (clime-node-aliases child))
                       child)))
                 children))))

(defun clime-node-all-ancestor-flags (node)
  "Collect all option flags from ancestors of NODE.
Walk the parent chain, collecting each ancestor's option flags into a flat list."
  (let ((parent (clime-node-parent node))
        (flags '()))
    (while parent
      (dolist (opt (clime-node-options parent))
        (setq flags (append (clime-option-flags opt) flags)))
      (setq parent (clime-node-parent parent)))
    flags))

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
      (when (clime-group-only-p node)
        (dolist (entry (clime-group-children node))
          (clime-check-ancestor-collisions (cdr entry) merged path))))))

(provide 'clime-core)
;;; clime-core.el ends here
