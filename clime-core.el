;;; clime-core.el --- Command tree data model for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

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

;;; ─── Command ────────────────────────────────────────────────────────────

(cl-defstruct (clime-command (:constructor clime-command--create)
                             (:copier nil))
  "A leaf command in the CLI tree."
  (name nil :type string :documentation "Command name as typed by user.")
  (aliases nil :type list :documentation "Alternative names for this command.")
  (help nil :type (or string null) :documentation "One-line description.")
  (options nil :type list :documentation "List of `clime-option' structs.")
  (args nil :type list :documentation "Ordered list of `clime-arg' structs.")
  (parent nil :documentation "Parent node ref, or nil for root.")
  (hidden nil :type boolean :documentation "If non-nil, omit from help.")
  (handler nil :type (or function null) :documentation "Handler function, called with context."))

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

(cl-defstruct (clime-group (:constructor clime-group--create)
                           (:copier nil))
  "A branch node (subcommand group) in the CLI tree."
  (name nil :type string :documentation "Group name as typed by user.")
  (aliases nil :type list :documentation "Alternative names for this group.")
  (help nil :type (or string null) :documentation "One-line description.")
  (options nil :type list :documentation "List of `clime-option' structs.")
  (args nil :type list :documentation "Ordered list of `clime-arg' structs.")
  (parent nil :documentation "Parent node ref, or nil for root.")
  (hidden nil :type boolean :documentation "If non-nil, omit from help.")
  (children nil :type list :documentation "Alist of (name . node) for subcommands/subgroups.")
  (invoke nil :type (or function null) :documentation "Optional handler when group invoked without subcommand."))

(defun clime-make-group (&rest args)
  "Create a `clime-group' with validation.
Required keyword arg: :name (string).
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-group: :name is required"))
  (apply #'clime-group--create args))

;;; ─── App ────────────────────────────────────────────────────────────────

(cl-defstruct (clime-app (:include clime-group)
                         (:constructor clime-app--create)
                         (:copier nil))
  "Root application node.  Extends `clime-group' with app-level metadata."
  (version nil :type (or string null) :documentation "Application version string.")
  (env-prefix nil :type (or string null) :documentation "Prefix for auto-derived env var names.")
  (json-mode nil :type boolean :documentation "Whether --json is a built-in root option."))

(defun clime-make-app (&rest args)
  "Create a `clime-app' with validation.
Required keyword arg: :name (string).
ARGS is a plist of slot values."
  (unless (plist-get args :name)
    (error "clime-make-app: :name is required"))
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

;;; ─── Predicates ─────────────────────────────────────────────────────────

(defun clime-node-p (obj)
  "Return non-nil if OBJ is a command, group, or app node."
  (or (clime-command-p obj)
      (clime-group-p obj)))

;;; ─── Tree Queries ───────────────────────────────────────────────────────

(defun clime-node-find-option (node flag)
  "Find the `clime-option' in NODE whose flags contain FLAG string.
Return the option struct, or nil if not found."
  (let ((options (if (clime-command-p node)
                     (clime-command-options node)
                   (clime-group-options node))))
    (cl-find-if (lambda (opt)
                  (member flag (clime-option-flags opt)))
                options)))

(defun clime-group-find-child (group name)
  "Find a child node in GROUP by NAME or alias.
Return the child node, or nil if not found."
  (let ((children (clime-group-children group)))
    (or (cdr (assoc name children))
        ;; Scan aliases
        (cl-some (lambda (entry)
                   (let ((child (cdr entry)))
                     (when (member name
                                   (if (clime-command-p child)
                                       (clime-command-aliases child)
                                     (clime-group-aliases child)))
                       child)))
                 children))))

(defun clime-node-all-ancestor-flags (node)
  "Collect all option flags from ancestors of NODE.
Walk the parent chain, collecting each ancestor's option flags into a flat list."
  (let ((parent (if (clime-command-p node)
                    (clime-command-parent node)
                  (clime-group-parent node)))
        (flags '()))
    (while parent
      (let ((options (if (clime-command-p parent)
                         (clime-command-options parent)
                       (clime-group-options parent))))
        (dolist (opt options)
          (setq flags (append (clime-option-flags opt) flags))))
      (setq parent (if (clime-command-p parent)
                       (clime-command-parent parent)
                     (clime-group-parent parent))))
    flags))

(provide 'clime-core)
;;; clime-core.el ends here
