;;; clime-dsl.el --- Declarative DSL for building CLI apps  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Macro DSL for declaratively defining CLI command trees.
;; The primary authoring interface for clime applications.
;;
;; Usage:
;;   (clime-app myapp
;;     :version "1.0"
;;     (clime-option verbose ("-v" "--verbose") :count t)
;;     (clime-command show
;;       :help "Show a resource"
;;       (clime-arg id :help "Resource ID")
;;       (clime-handler (ctx)
;;         (message "ID: %s" (clime-ctx-get ctx 'id)))))

;;; Code:

(require 'cl-lib)
(require 'clime-core)

;;; ─── Body Parser ────────────────────────────────────────────────────────

(defun clime--extract-keywords (body valid-keys)
  "Extract keyword args from BODY, returning (KEYWORDS . REST).
KEYWORDS is a plist of recognized keyword args from VALID-KEYS.
REST is the remaining non-keyword forms.
Resolves :doc as an alias for :help (error if both are present)."
  (let ((keywords '())
        (rest '())
        (items body))
    (while items
      (let ((item (car items)))
        (if (and (keywordp item) (memq item valid-keys) (cdr items))
            (progn
              (setq keywords (plist-put keywords item (cadr items)))
              (setq items (cddr items)))
          (push item rest)
          (setq items (cdr items)))))
    ;; Resolve :doc → :help alias
    (when (plist-get keywords :doc)
      (when (plist-get keywords :help)
        (error "Cannot use both :doc and :help on the same form"))
      (setq keywords (plist-put keywords :help (plist-get keywords :doc)))
      (cl-remf keywords :doc))
    (cons keywords (nreverse rest))))

(defun clime--classify-body (forms)
  "Classify FORMS into options, args, children, and handler.
Returns a plist (:options :args :children :handler)."
  (let (options args children output-formats handler)
    (dolist (form forms)
      (when (consp form)
        (pcase (car form)
          ('clime-option (push (clime--build-option (cdr form)) options))
          ('clime-arg (push (clime--build-arg (cdr form)) args))
          ('clime-command (push (clime--build-command (cdr form)) children))
          ('clime-alias-for (push (clime--build-alias-for (cdr form)) children))
          ('clime-group (push (clime--build-group (cdr form)) children))
          ('clime-output-format (push (clime--build-output-format (cdr form)) output-formats))
          ('clime-handler (setq handler (clime--build-handler (cdr form))))
          (_ (error "Unknown DSL form: %S" (car form))))))
    (list :options (nreverse options)
          :args (nreverse args)
          :children (nreverse children)
          :output-formats (nreverse output-formats)
          :handler handler)))

;;; ─── DSL Shorthands ────────────────────────────────────────────────────

(defun clime--normalize-bool-flag (plist)
  "Normalize :bool/:flag in PLIST to :nargs 0.
:bool t is the preferred form.  :flag t is deprecated (emits a warning).
Returns the updated plist with :bool/:flag removed and :nargs 0 set."
  (let ((has-bool (plist-get plist :bool))
        (has-flag (plist-get plist :flag)))
    (when (and has-bool has-flag)
      (error "clime-option: use :bool or :flag, not both"))
    (when has-flag
      (message "Warning: :flag is deprecated, use :bool instead")
      (setq plist (cl-copy-list plist))
      (cl-remf plist :flag)
      (setq plist (plist-put plist :nargs 0)))
    (when has-bool
      (setq plist (cl-copy-list plist))
      (cl-remf plist :bool)
      (setq plist (plist-put plist :nargs 0)))
    plist))

;;; ─── Parameter Templates ───────────────────────────────────────────────

;;;###autoload
(defmacro clime-defopt (name &rest plist)
  "Define NAME as a reusable option template.
Expands to (defvar clime--opt-NAME PLIST).
PLIST contains default option slot values (evaluated at load time).
Must not contain :name or :flags (those are per-instance).
Supports DSL shorthands: :bool t normalizes to :nargs 0,
:separator implies :multiple t."
  (declare (indent 1))
  (let ((var-sym (intern (format "clime--opt-%s" name))))
    ;; Validate: reject per-instance slots
    (when (plist-member plist :name)
      (error "clime-defopt %s: :name is per-instance, not allowed in templates" name))
    (when (plist-member plist :flags)
      (error "clime-defopt %s: :flags is per-instance, not allowed in templates" name))
    ;; Normalize :bool/:flag t → :nargs 0
    (setq plist (clime--normalize-bool-flag plist))
    ;; :separator implies :multiple t
    (when (and (plist-get plist :separator)
               (not (plist-member plist :multiple)))
      (setq plist (plist-put (cl-copy-list plist) :multiple t)))
    `(defvar ,var-sym (list ,@plist))))

;;;###autoload
(defmacro clime-defarg (name &rest plist)
  "Define NAME as a reusable argument template.
Expands to (defvar clime--arg-NAME PLIST).
PLIST contains default arg slot values (evaluated at load time).
Must not contain :name (per-instance)."
  (declare (indent 1))
  (let ((var-sym (intern (format "clime--arg-%s" name))))
    (when (plist-member plist :name)
      (error "clime-defarg %s: :name is per-instance, not allowed in templates" name))
    `(defvar ,var-sym (list ,@plist))))

;;; ─── Form Builders ──────────────────────────────────────────────────────

(defun clime--build-option (args)
  "Build a `clime-option' constructor form from DSL ARGS.
ARGS is (NAME FLAGS &rest PLIST).
Supports :bool t as shorthand for :nargs 0 (boolean flag).
Supports :from TEMPLATE-NAME to inherit defaults from a `clime-defopt' template."
  (let* ((name (car args))
         (flags (cadr args))
         (plist (cddr args))
         (from-name (plist-get plist :from)))
    ;; Strip :from from plist (not a real slot)
    (when from-name
      (setq plist (cl-copy-list plist))
      (cl-remf plist :from))
    ;; :bool/:flag t → :nargs 0 (DSL-only shorthand, not a real slot)
    (setq plist (clime--normalize-bool-flag plist))
    ;; :separator implies :multiple t
    (when (and (plist-get plist :separator)
               (not (plist-member plist :multiple)))
      (setq plist (plist-put (cl-copy-list plist) :multiple t)))
    (if from-name
        (let ((template-sym (intern (format "clime--opt-%s" from-name))))
          `(apply #'clime-make-option
                  (clime--merge-template ,template-sym
                                         :name ',name :flags ',flags ,@plist)))
      `(clime-make-option :name ',name :flags ',flags ,@plist))))

(defun clime--build-arg (args)
  "Build a `clime-arg' constructor form from DSL ARGS.
ARGS is (NAME &rest PLIST).
Supports :from TEMPLATE-NAME to inherit defaults from a `clime-defarg' template."
  (let* ((name (car args))
         (plist (cdr args))
         (from-name (plist-get plist :from)))
    (when from-name
      (setq plist (cl-copy-list plist))
      (cl-remf plist :from))
    (if from-name
        (let ((template-sym (intern (format "clime--arg-%s" from-name))))
          `(apply #'clime-make-arg
                  (clime--merge-template ,template-sym
                                         :name ',name ,@plist)))
      `(clime-make-arg :name ',name ,@plist))))

(defun clime--build-output-format (args)
  "Build a `clime-output-format' constructor form from DSL ARGS.
ARGS is (NAME FLAGS &rest PLIST)."
  (let* ((name (car args))
         (flags (cadr args))
         (plist (cddr args)))
    `(clime-make-output-format :name ',name :flags ',flags ,@plist)))

(defun clime--build-handler (args)
  "Build a lambda form from DSL ARGS.
ARGS is (ARGLIST &rest BODY)."
  (let ((arglist (car args))
        (body (cdr args)))
    `(lambda ,arglist ,@body)))

(defun clime--normalize-aliases (aliases)
  "Convert ALIASES list to strings.  Symbols become their name."
  (mapcar (lambda (a) (if (stringp a) a (symbol-name a))) aliases))

(defun clime--build-command (args)
  "Build a `clime-command' constructor form from DSL ARGS.
ARGS is (NAME &rest BODY)."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:help :doc :aliases :hidden :epilog :category :deprecated)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms))
         (handler (plist-get classified :handler)))
    (unless handler
      (error "clime-command %s: missing clime-handler" name))
    `(cons ,name-str
           (clime-make-command
            :name ,name-str
            :handler ,handler
            ,@(when (plist-get keywords :help)
                `(:help ,(plist-get keywords :help)))
            ,@(when (plist-get keywords :aliases)
                `(:aliases ',(clime--normalize-aliases (plist-get keywords :aliases))))
            ,@(when (plist-get keywords :hidden)
                `(:hidden ,(plist-get keywords :hidden)))
            ,@(when (plist-get keywords :epilog)
                `(:epilog ,(plist-get keywords :epilog)))
            ,@(when (plist-get keywords :category)
                `(:category ,(plist-get keywords :category)))
            ,@(when (plist-get keywords :deprecated)
                `(:deprecated ,(plist-get keywords :deprecated)))
            ,@(when (plist-get classified :options)
                `(:options (list ,@(plist-get classified :options))))
            ,@(when (plist-get classified :args)
                `(:args (list ,@(plist-get classified :args))))))))

(defun clime--build-alias-for (args)
  "Build a `clime-command' alias-for form from DSL ARGS.
ARGS is (NAME (PATH...) &rest KEYWORDS).
PATH is a list of symbols naming the target command path.
Supports :defaults and :vals alists for preset/locked option values."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (path-form (cadr args))
         (path-strings (mapcar #'symbol-name path-form))
         (extracted (clime--extract-keywords
                     (cddr args)
                     '(:help :doc :aliases :hidden :category :deprecated
                       :defaults :vals)))
         (keywords (car extracted)))
    `(cons ,name-str
           (clime-alias--create
            :name ,name-str
            :target ',path-strings
            ,@(when (plist-get keywords :defaults)
                `(:defaults ,(plist-get keywords :defaults)))
            ,@(when (plist-get keywords :vals)
                `(:vals ,(plist-get keywords :vals)))
            ,@(when (plist-get keywords :help)
                `(:help ,(plist-get keywords :help)))
            ,@(when (plist-get keywords :aliases)
                `(:aliases ',(clime--normalize-aliases (plist-get keywords :aliases))))
            ,@(when (plist-get keywords :hidden)
                `(:hidden ,(plist-get keywords :hidden)))
            ,@(when (plist-get keywords :category)
                `(:category ,(plist-get keywords :category)))
            ,@(when (plist-get keywords :deprecated)
                `(:deprecated ,(plist-get keywords :deprecated)))))))

(defun clime--build-group (args)
  "Build a `clime-group' constructor form from DSL ARGS.
ARGS is (NAME &rest BODY)."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:help :doc :aliases :hidden :inline :epilog :category :deprecated)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms)))
    `(cons ,name-str
           (clime-make-group
            :name ,name-str
            ,@(when (plist-get keywords :help)
                `(:help ,(plist-get keywords :help)))
            ,@(when (plist-get keywords :aliases)
                `(:aliases ',(clime--normalize-aliases (plist-get keywords :aliases))))
            ,@(when (plist-get keywords :hidden)
                `(:hidden ,(plist-get keywords :hidden)))
            ,@(when (plist-get keywords :inline)
                `(:inline ,(plist-get keywords :inline)))
            ,@(when (plist-get keywords :epilog)
                `(:epilog ,(plist-get keywords :epilog)))
            ,@(when (plist-get keywords :category)
                `(:category ,(plist-get keywords :category)))
            ,@(when (plist-get keywords :deprecated)
                `(:deprecated ,(plist-get keywords :deprecated)))
            ,@(when (plist-get classified :options)
                `(:options (list ,@(plist-get classified :options))))
            ,@(when (plist-get classified :args)
                `(:args (list ,@(plist-get classified :args))))
            ,@(when (plist-get classified :children)
                `(:children (list ,@(plist-get classified :children))))
            ,@(when (plist-get classified :handler)
                `(:handler ,(plist-get classified :handler)))))))

;;; ─── Top-Level Macro ────────────────────────────────────────────────────

;;;###autoload
(defmacro clime-app (name &rest body)
  "Define a CLI application NAME with BODY.
NAME is an unquoted symbol that becomes both the variable name
and the app name string.

BODY is a mix of keyword args and child forms:
  :version STRING  — app version
  :env-prefix STRING — prefix for env var auto-derivation
  :help STRING — app description
  :json-mode BOOL — enable built-in --json option (deprecated)

Child forms:
  (clime-option NAME FLAGS &rest PLIST)
  (clime-arg NAME &rest PLIST)
  (clime-command NAME &rest BODY)
  (clime-group NAME &rest BODY)
  (clime-output-format NAME FLAGS &rest PLIST)"
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     body
                     '(:version :env-prefix :help :doc :json-mode :epilog :setup)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms)))
    `(defvar ,name
       (clime-make-app
        :name ,name-str
        ,@(when (plist-get keywords :version)
            `(:version ,(plist-get keywords :version)))
        ,@(when (plist-get keywords :env-prefix)
            `(:env-prefix ,(plist-get keywords :env-prefix)))
        ,@(when (plist-get keywords :help)
            `(:help ,(plist-get keywords :help)))
        ,@(when (plist-get keywords :json-mode)
            `(:json-mode ,(plist-get keywords :json-mode)))
        ,@(when (plist-get keywords :epilog)
            `(:epilog ,(plist-get keywords :epilog)))
        ,@(when (plist-get keywords :setup)
            `(:setup ,(plist-get keywords :setup)))
        ,@(when (plist-get classified :output-formats)
            `(:output-formats (list ,@(plist-get classified :output-formats))))
        ,@(when (plist-get classified :options)
            `(:options (list ,@(plist-get classified :options))))
        ,@(when (plist-get classified :args)
            `(:args (list ,@(plist-get classified :args))))
        ,@(when (plist-get classified :children)
            `(:children (list ,@(plist-get classified :children))))
        ,@(when (plist-get classified :handler)
            `(:handler ,(plist-get classified :handler)))))))

;;; ─── DSL Form Macros ───────────────────────────────────────────────────

;; Each DSL form is a real macro that produces its struct when evaluated
;; standalone.  Inside `clime-app', `clime--classify-body' still
;; processes forms by pattern matching — these macros are not expanded
;; there.  The macro definitions serve two purposes:
;;   1. Standalone evaluation (REPL, testing, programmatic construction)
;;   2. Keyword completion in `emacs-lisp-mode' (Emacs discovers &key args)

;; Leaf forms use `cl-defmacro' with &key for keyword completion.
;; Container forms use `defmacro' with &rest body (keywords interleaved
;; with child forms prevent &key usage).

(cl-defmacro clime-option (name flags
                           &rest plist
                           &key bool flag from type help required default
                           nargs env count multiple choices coerce conform
                           separator category hidden deprecated mutex
                           negatable requires zip
                           &allow-other-keys)
  "Define a CLI option NAME with FLAGS.
NAME is a symbol — the canonical parameter name.
FLAGS is a list of flag strings, e.g. (\"--verbose\" \"-v\").

Keyword arguments:
  :bool t           Boolean flag (shorthand for :nargs 0)
  :count t          Stackable counter flag (-vvv = 3)
  :multiple t       Collect repeated values into a list
  :separator SEP    Split value by SEP (implies :multiple)
  :negatable t      Generate --no-X variant
  :required t       Option must be provided
  :requires SYMS    Other options that must also be set
  :mutex SYM        Mutual exclusion group
  :zip SYM          Paired option group
  :nargs N          Number of arguments (0 = boolean)
  :type SYM         Type converter (\\='string, \\='integer, \\='number)
  :choices LIST     Allowed values or function
  :coerce FN        Transform after type coercion
  :conform FN       Pass-2 validation/normalization
  :default VAL      Default value when not provided
  :env STR          Override env var name
  :category STR     Help display category
  :hidden t         Omit from help
  :deprecated STR   Deprecation message or t
  :help STR         One-line help text
  :from SYM         Inherit from `clime-defopt' template"
  (declare (indent 2))
  (ignore bool flag from type help required default nargs env count
          multiple choices coerce conform separator category hidden
          deprecated mutex negatable requires zip)
  (clime--build-option (cons name (cons flags plist))))

(cl-defmacro clime-arg (name
                        &rest plist
                        &key from type help required default nargs
                        choices coerce conform deprecated
                        &allow-other-keys)
  "Define a positional argument NAME.
NAME is a symbol — the canonical parameter name.

Keyword arguments:
  :type SYM         Type converter (\\='string, \\='integer, \\='number)
  :choices LIST     Allowed values or function
  :coerce FN        Transform after type coercion
  :conform FN       Pass-2 validation/normalization
  :default VAL      Default value when not provided
  :nargs N          Number of arguments (:rest for rest args)
  :required BOOL    Whether arg is required (default t)
  :deprecated STR   Deprecation message or t
  :help STR         One-line help text
  :from SYM         Inherit from `clime-defarg' template"
  (declare (indent 1))
  (ignore from type help required default nargs choices coerce conform
          deprecated)
  (clime--build-arg (cons name plist)))

(defmacro clime-command (name &rest body)
  "Define a subcommand NAME with BODY.
NAME is a symbol — the command name.

BODY is a mix of keyword args and child forms:
  :help STRING       — command description
  :aliases LIST      — alternative names
  :hidden BOOL       — omit from help
  :epilog STRING     — text after help
  :category STRING   — help display category
  :deprecated STRING — deprecation message or t

Child forms:
  (clime-option NAME FLAGS &rest PLIST)
  (clime-arg NAME &rest PLIST)
  (clime-handler (CTX) &rest BODY)"
  (declare (indent 1))
  (clime--build-command (cons name body)))

(cl-defmacro clime-alias-for (name path
                              &rest plist
                              &key help doc aliases hidden category
                              deprecated defaults vals
                              &allow-other-keys)
  "Define an alias NAME for the command at PATH.
NAME is a symbol — the alias name.
PATH is a list of symbols naming the target command.

Keyword arguments:
  :help STR         Help text (inherits from target if omitted)
  :aliases LIST     Alternative names
  :hidden t         Omit from help
  :category STR     Help display category
  :deprecated STR   Deprecation message or t
  :defaults ALIST   Preset option values (user can override)
  :vals ALIST       Locked option values (hidden from CLI)"
  (declare (indent 2))
  (ignore help doc aliases hidden category deprecated defaults vals)
  (clime--build-alias-for (cons name (cons path plist))))

(defmacro clime-group (name &rest body)
  "Define a command group NAME with BODY.
NAME is a symbol — the group name.

BODY is a mix of keyword args and child forms:
  :help STRING       — group description
  :aliases LIST      — alternative names
  :hidden BOOL       — omit from help
  :inline BOOL       — promote children to parent level
  :epilog STRING     — text after help
  :category STRING   — help display category
  :deprecated STRING — deprecation message or t

Child forms:
  (clime-option NAME FLAGS &rest PLIST)
  (clime-arg NAME &rest PLIST)
  (clime-command NAME &rest BODY)
  (clime-group NAME &rest BODY)
  (clime-alias-for NAME PATH &rest KEYWORDS)
  (clime-handler (CTX) &rest BODY)"
  (declare (indent 1))
  (clime--build-group (cons name body)))

(cl-defmacro clime-output-format (name flags
                                  &rest plist
                                  &key help finalize streaming encoder error-handler
                                  mutex hidden category deprecated
                                  &allow-other-keys)
  "Declare an output format NAME with FLAGS.
NAME is a symbol — the format name (e.g. json, yaml).
FLAGS is a list of flag strings, e.g. (\"--json\").

Inherits all `clime-option' behavior (mutex, hidden, category, etc.)
since output formats ARE options.

Keyword arguments:
  :help STR         Help text for the flag
  :finalize FN      Envelope function: (items retval errors) → data | nil
  :streaming t      Emit immediately, bypass accumulator
  :encoder FN       Data → string encoder
  :error-handler FN Error handler: (msg) → side effect
  :mutex SYM        Mutual exclusion group
  :hidden t         Omit from help
  :category STR     Help display category
  :deprecated STR   Deprecation message or t"
  (declare (indent 2))
  (ignore help finalize streaming encoder error-handler mutex hidden category deprecated)
  (clime--build-output-format (cons name (cons flags plist))))

(defmacro clime-handler (arglist &rest body)
  "Define a command handler with ARGLIST and BODY.
ARGLIST is typically (CTX) — receives the parse context.
BODY is the handler implementation."
  (declare (indent 1))
  `(lambda ,arglist ,@body))

(provide 'clime-dsl)
;;; clime-dsl.el ends here
