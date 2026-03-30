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

;;; ─── Bare Boolean Normalization ────────────────────────────────────────

(defconst clime--boolean-keywords
  '(:bool :flag :count :multiple :negatable :hidden :required
          :optional :rest :inline :json-mode :streaming :deprecated :env)
  "DSL keywords that accept bare form as shorthand for t.
A bare boolean keyword (not followed by a non-keyword, non-cons value)
is normalized to keyword t before further processing.")

(defun clime--normalize-bare-booleans (items boolean-keys)
  "Normalize bare boolean keywords in ITEMS.
If a keyword in BOOLEAN-KEYS appears without a value (followed by
another keyword, a cons, or end of list), insert t as its value.
Non-boolean keywords consume the next item as their value (so e.g.
`:nargs :rest' is not misinterpreted).
Returns a new list; ITEMS is not modified."
  (let (result)
    (while items
      (let ((item (car items)))
        (cond
         ;; Boolean keyword with bare usage (next is keyword/cons/end)
         ((and (memq item boolean-keys)
               (let ((next (cadr items)))
                 (or (null (cdr items))
                     (keywordp next)
                     (consp next))))
          (push item result)
          (push t result)
          (setq items (cdr items)))
         ;; Any keyword (boolean with explicit value, or non-boolean):
         ;; consume key + value as a pair
         ((keywordp item)
          (push item result)
          (setq items (cdr items))
          (when items
            (push (car items) result)
            (setq items (cdr items))))
         ;; Non-keyword item (DSL form, etc.)
         (t
          (push item result)
          (setq items (cdr items))))))
    (nreverse result)))

;;; ─── :optional → :required Normalization ────────────────────────────────

(defun clime--normalize-optional (plist)
  "Normalize :optional into :required in PLIST.
If :optional is truthy and :required is also present, signal an error.
If :optional is truthy, strip :optional and set :required nil.
If :optional is nil (explicit), strip :optional and set :required t.
Returns a new plist; PLIST is not modified."
  (if (not (plist-member plist :optional))
      plist
    (let ((opt-val (plist-get plist :optional))
          (new (cl-copy-list plist)))
      (cl-remf new :optional)
      (when (plist-member plist :required)
        (error ":optional and :required are mutually exclusive"))
      (setq new (plist-put new :required (not opt-val)))
      new)))

;;; ─── Body Parser ────────────────────────────────────────────────────────

(defun clime--extract-keywords (body valid-keys)
  "Extract keyword args from BODY, returning (KEYWORDS . REST).
KEYWORDS is a plist of recognized keyword args from VALID-KEYS.
REST is the remaining non-keyword forms.
Resolves :doc as an alias for :help (error if both are present)."
  (let ((keywords '())
        (rest '())
        (items (clime--normalize-bare-booleans body clime--boolean-keywords)))
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

(defun clime--expanded-constructor (form)
  "Return the constructor symbol from an expanded DSL FORM.
Handles direct calls like (clime-make-option ...),
`apply' wrappers like (apply #\\='clime-make-option ...),
`cons' wrappers like (cons NAME (clime-make-command ...)),
and `function' wrappers like #\\='(lambda ...)."
  (pcase form
    (`(apply ,(or `(function ,fn) `(quote ,fn)) . ,_) fn)
    (`(function (lambda . ,_)) 'lambda)
    (`(function ,(pred symbolp)) 'function)
    (`(quote ,(pred symbolp)) 'quote)
    (`(cons ,_ ,inner) (clime--expanded-constructor inner))
    (`(,head . ,_) head)
    (_ nil)))

(defconst clime--constructor-category
  '((clime-make-option        . :options)
    (clime-make-arg            . :args)
    (clime-make-command        . :children)
    (clime-alias--create       . :children)
    (clime-make-group          . :children)
    (clime-make-output-format  . :output-formats)
    (lambda                    . :handler)
    (function                  . :handler)
    (quote                     . :handler))
  "Map from expanded constructor symbol to `clime--classify-body' category.")

(defun clime--classify-body (forms)
  "Classify FORMS into options, args, children, and handler.
Returns a plist (:options :args :children :handler).
Resolves DSL aliases via `macroexpand' and classifies by the
constructor symbol in the expanded form."
  (let (options args children output-formats (conform-fns nil) handler)
    (dolist (form forms)
      (when (consp form)
        (pcase (car form)
          ;; All forms: macroexpand to resolve aliases (including
          ;; clime-mutex/clime-zip which expand to inline groups),
          ;; then classify by the constructor in the expanded form.
          (_
           (let* ((expanded (macroexpand form))
                  (ctor (clime--expanded-constructor expanded))
                  (category (cdr (assq ctor clime--constructor-category))))
             (pcase category
               (:options        (push expanded options))
               (:args           (push expanded args))
               (:children       (push expanded children))
               (:output-formats (push expanded output-formats))
               (:handler        (setq handler expanded))
               (_               (error "Unknown DSL form: %S" (car form)))))))))
    (list :options (nreverse options)
          :args (nreverse args)
          :children (nreverse children)
          :output-formats (nreverse output-formats)
          :conform (nreverse conform-fns)
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
      (display-warning
       'clime ":flag is deprecated, use :bool instead")
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
Supports DSL shorthands: :bool normalizes to :nargs 0,
:separator implies :multiple t."
  (declare (indent 1))
  (setq plist (clime--normalize-bare-booleans plist clime--boolean-keywords))
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

(defalias 'clime-defoption 'clime-defopt)

;;;###autoload
(defmacro clime-defarg (name &rest plist)
  "Define NAME as a reusable argument template.
Expands to (defvar clime--arg-NAME PLIST).
PLIST contains default arg slot values (evaluated at load time).
Must not contain :name (per-instance)."
  (declare (indent 1))
  (setq plist (clime--normalize-bare-booleans plist clime--boolean-keywords))
  (let ((var-sym (intern (format "clime--arg-%s" name))))
    (when (plist-member plist :name)
      (error "clime-defarg %s: :name is per-instance, not allowed in templates" name))
    `(defvar ,var-sym (list ,@plist))))

(defalias 'clime-defargument 'clime-defarg)

;;; ─── Composable Form Definitions ────────────────────────────────────────

;;;###autoload
(defmacro clime-defcommand (name &rest body)
  "Define NAME as a variable holding a composable command form.
NAME serves as both the defvar symbol and the command name string.
The stored value is (cons NAME-STRING COMMAND-STRUCT), directly
usable in a container's :children keyword."
  (declare (indent 1))
  `(defvar ,name (clime-command ,name ,@body)))

;;;###autoload
(defmacro clime-defgroup (name &rest body)
  "Define NAME as a variable holding a composable group form.
NAME serves as both the defvar symbol and the group name string.
The stored value is (cons NAME-STRING GROUP-STRUCT), directly
usable in a container's :children keyword."
  (declare (indent 1))
  `(defvar ,name (clime-group ,name ,@body)))

;;; ─── Emit Helpers ──────────────────────────────────────────────────────

(defun clime--emit-kw (keywords keys)
  "Return a flat plist of non-nil entries from KEYWORDS for KEYS.
Uses `plist-member' so explicitly-false values (e.g. :hidden nil)
are preserved.  Only truly absent keys are omitted."
  (let (result)
    (dolist (key keys)
      (when (plist-member keywords key)
        (push key result)
        (push (plist-get keywords key) result)))
    (nreverse result)))

(defun clime--emit-body (classified keys)
  "Return constructor pairs for non-nil classified body KEYS.
Collections (:options, :args, :children, :output-formats) wrap values
in (list ...).  :handler emits its value bare.  :conform is always a list."
  (let (result)
    (dolist (key keys)
      (let ((val (plist-get classified key)))
        (when val
          (push key result)
          (push (pcase key
                  (:handler val)
                  (:conform `(list ,@val))
                  (_ `(list ,@val)))
                result))))
    (nreverse result)))

(defun clime--merge-kw-body (kw-expr body-forms)
  "Merge keyword list expression KW-EXPR with classified BODY-FORMS.
Returns a form that produces the merged list at runtime, or nil if both
are empty.  KW-EXPR is a runtime expression; BODY-FORMS are macro-time
constructor forms to wrap in (list ...)."
  (cond
   ((and kw-expr body-forms) `(append ,kw-expr (list ,@body-forms)))
   (kw-expr kw-expr)
   (body-forms `(list ,@body-forms))
   (t nil)))

(defun clime--emit-merged (keywords classified keys)
  "Emit constructor pairs merging KEYWORDS and CLASSIFIED for KEYS.
For collection slots (:options, :args, :children, :output-formats),
merges keyword-provided runtime lists with classified body forms.
For :handler and :conform, emits from classified only (no merge)."
  (let (result)
    (dolist (key keys)
      (pcase key
        ((or :handler :conform)
         (let ((val (plist-get classified key)))
           (when val
             (push key result)
             (push (pcase key
                     (:handler val)
                     (:conform `(list ,@val)))
                   result))))
        (_
         (let ((merged (clime--merge-kw-body
                        (plist-get keywords key)
                        (plist-get classified key))))
           (when merged
             (push key result)
             (push merged result))))))
    (nreverse result)))

(defun clime--prepare-aliases (keywords)
  "Return a copy of KEYWORDS with :aliases normalized and quoted.
If :aliases is absent, returns KEYWORDS unchanged."
  (if (plist-get keywords :aliases)
      (plist-put (cl-copy-list keywords) :aliases
                 `',(clime--normalize-aliases (plist-get keywords :aliases)))
    keywords))

;;; ─── Form Builders ──────────────────────────────────────────────────────

(defun clime--build-option (args)
  "Build a `clime-option' constructor form from DSL ARGS.
ARGS is (NAME FLAGS &rest PLIST).
Supports :bool t as shorthand for :nargs 0 (boolean flag).
Supports :from TEMPLATE-NAME to inherit defaults from a `clime-defopt' template."
  (let* ((name (car args))
         (flags (cadr args))
         (plist (clime--normalize-bare-booleans (cddr args) clime--boolean-keywords))
         (from-name (plist-get plist :from)))
    ;; Strip :from from plist (not a real slot)
    (when from-name
      (setq plist (cl-copy-list plist))
      (cl-remf plist :from))
    ;; :optional → :required nil (DSL-only shorthand, not a real slot)
    (setq plist (clime--normalize-optional plist))
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
         (plist (clime--normalize-bare-booleans (cdr args) clime--boolean-keywords))
         (from-name (plist-get plist :from)))
    (when from-name
      (setq plist (cl-copy-list plist))
      (cl-remf plist :from))
    ;; :optional → :required nil (DSL-only shorthand, not a real slot)
    (setq plist (clime--normalize-optional plist))
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
         (plist (clime--normalize-bare-booleans (cddr args) clime--boolean-keywords)))
    `(clime-make-output-format :name ',name :flags ',flags ,@plist)))

(defun clime--expanded-option-plist (expanded)
  "Return the keyword plist from EXPANDED option constructor form.
For direct calls like (clime-make-option :name \\='x :flags \\='(...) ...),
returns the plist portion.  For `apply' (template) forms like
\(apply #\\='clime-make-option (clime--merge-template TMPL :name \\='x ...)),
extracts the trailing keywords from the merge-template call."
  (pcase expanded
    (`(clime-make-option . ,plist) plist)
    (`(apply ,_ (clime--merge-template ,_ . ,plist)) plist)
    (_ nil)))

(defun clime--build-mutex-conform (args)
  "Build an exclusive group from DSL ARGS.
ARGS is (NAME &rest BODY) where BODY contains keyword args and
clime-option forms.  Returns a (cons NAME-STR (clime-make-group ...))
form suitable for :children classification.
Resolves DSL aliases via `macroexpand'."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:required :help :doc :default)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (default (plist-get keywords :default))
         (required (plist-get keywords :required))
         (option-forms '())
         (member-names '()))
    (dolist (form body-forms)
      (when (consp form)
        (let* ((expanded (macroexpand form))
               (ctor (clime--expanded-constructor expanded)))
          (if (eq ctor 'clime-make-option)
              (let ((plist (clime--expanded-option-plist expanded)))
                (push (cadr (plist-get plist :name)) member-names)
                (push expanded option-forms))
            (error "clime-mutex %s: only option forms allowed, got %S"
                   name (car form))))))
    (setq member-names (nreverse member-names))
    (when (and required default)
      (display-warning
       'clime
       (format "clime-mutex `%s': :default is vacuous when :required is set"
               name)))
    (when required
      (dolist (opt-form (reverse option-forms))
        (let ((plist (clime--expanded-option-plist opt-form)))
          (when (and plist (plist-get plist :default))
            (display-warning
             'clime
             (format "clime-mutex `%s': option `%s' has :default, \
which is vacuous — defaults apply after exclusivity check"
                     name (cadr (plist-get plist :name))))))))
    `(cons ,name-str
           (clime-make-group
            :name ,name-str
            :inline t
            :conform (list (clime-check-exclusive ',name ',member-names ,default ,required))
            :options (list ,@(nreverse option-forms))))))

(defun clime--build-zip-conform (args)
  "Build a paired group from DSL ARGS.
ARGS is (NAME &rest BODY) where BODY contains keyword args and
clime-option forms.  Returns a (cons NAME-STR (clime-make-group ...))
form suitable for :children classification.
Resolves DSL aliases via `macroexpand'."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:required :help :doc)))
         (body-forms (cdr extracted))
         (required (plist-get (car extracted) :required))
         (option-forms '())
         (member-names '()))
    (dolist (form body-forms)
      (when (consp form)
        (let* ((expanded (macroexpand form))
               (ctor (clime--expanded-constructor expanded)))
          (if (eq ctor 'clime-make-option)
              (let* ((plist (clime--expanded-option-plist expanded))
                     (opt-name (cadr (plist-get plist :name))))
                (push opt-name member-names)
                ;; Zip options are implicitly :multiple — inject into form
                ;; before macroexpanding, so the builder sees it.
                (let ((multi-expanded
                       (macroexpand (append form (list :multiple t)))))
                  (push multi-expanded option-forms)))
            (error "clime-zip %s: only option forms allowed, got %S"
                   name (car form))))))
    (setq member-names (nreverse member-names))
    (when required
      (dolist (opt-form (reverse option-forms))
        (let ((plist (clime--expanded-option-plist opt-form)))
          (when (and plist (plist-get plist :default))
            (display-warning
             'clime
             (format "clime-zip `%s': option `%s' has :default, \
which is vacuous — defaults apply after paired check"
                     name (cadr (plist-get plist :name))))))))
    `(cons ,name-str
           (clime-make-group
            :name ,name-str
            :inline t
            :conform (list (clime-check-paired ',name ',member-names ,required))
            :options (list ,@(nreverse option-forms))))))

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
                     '(:help :doc :aliases :hidden :epilog :examples :category :deprecated
                             :options :args)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms))
         (handler (plist-get classified :handler)))
    (unless handler
      (error "clime-command %s: missing clime-handler" name))
    (setq keywords (clime--prepare-aliases keywords))
    `(cons ,name-str
           (clime-make-command
            :name ,name-str
            :handler ,handler
            ,@(clime--emit-kw keywords '(:help :aliases :hidden :epilog :examples :category :deprecated))
            ,@(clime--emit-merged keywords classified '(:conform :options :args :children))))))

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
    (setq keywords (clime--prepare-aliases keywords))
    `(cons ,name-str
           (clime-alias--create
            :name ,name-str
            :target ',path-strings
            ,@(clime--emit-kw keywords '(:defaults :vals :help :aliases :hidden :category :deprecated))))))

(defun clime--build-group (args)
  "Build a `clime-group' constructor form from DSL ARGS.
ARGS is (NAME &rest BODY)."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:help :doc :aliases :hidden :inline :epilog :examples :category :deprecated
                             :options :args :children)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms)))
    (setq keywords (clime--prepare-aliases keywords))
    `(cons ,name-str
           (clime-make-group
            :name ,name-str
            ,@(clime--emit-kw keywords '(:help :aliases :hidden :inline :epilog :examples :category :deprecated))
            ,@(clime--emit-merged keywords classified '(:conform :options :args :children :handler))))))

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
                     '(:version :env-prefix :help :doc :json-mode :epilog :examples :setup
                                :options :args :children :output-formats)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms)))
    `(progn
       (defvar ,name nil ,(format "CLI app defined by `clime-app'."))
       (setq ,name
             (clime-make-app
              :name ,name-str
              ,@(clime--emit-kw keywords '(:version :env-prefix :help :json-mode :epilog :examples :setup))
              ,@(clime--emit-merged keywords classified '(:output-formats :conform :options :args :children :handler)))))))

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

(cl-defmacro clime-opt (name
                        flags
                        &rest plist
                        &key bool flag from type help required optional
                        default nargs env count multiple choices coerce
                        conform separator category hidden deprecated
                        negatable requires
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
  :optional t       Option is not required (exclusive with :required)
  :requires SYMS    Other options that must also be set
  :nargs N          Number of arguments (0 = boolean)
  :type SYM         Type converter (\\='string, \\='integer, \\='number)
  :choices LIST     Allowed values or function
  :coerce FN        Transform after type coercion
  :conform FN       Pass-2 validation/normalization
  :default VAL      Default value when not provided
  :env STR|t        Env var suffix (prefixed by :env-prefix) or t to auto-derive
  :category STR     Help display category
  :hidden t         Omit from help
  :deprecated STR   Deprecation message or t
  :help STR         One-line help text
  :from SYM         Inherit from `clime-defopt' template"
  (declare (indent 2))
  (ignore bool flag from type help required optional default nargs env count
          multiple choices coerce conform separator category hidden
          deprecated negatable requires)
  (clime--build-option (cons name (cons flags plist))))

(defalias 'clime-option 'clime-opt)
(put 'clime-option 'lisp-indent-function 2)

(cl-defmacro clime-arg (name
                        &rest plist
                        &key from type help required optional default
                        nargs choices coerce conform deprecated
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
  :optional BOOL    Arg is not required (exclusive with :required)
  :deprecated STR   Deprecation message or t
  :help STR         One-line help text
  :from SYM         Inherit from `clime-defarg' template"
  (declare (indent 1))
  (ignore from type help required optional default nargs choices coerce
          conform deprecated)
  (clime--build-arg (cons name plist)))

(defalias 'clime-argument 'clime-arg)
(put 'clime-argument 'lisp-indent-function 1)

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

(cl-defmacro clime-alias-for (name
                              path
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

(cl-defmacro clime-output-format (name
                                  flags
                                  &rest plist
                                  &key help finalize streaming encoder error-handler
                                  hidden category deprecated
                                  &allow-other-keys)
  "Declare an output format NAME with FLAGS.
NAME is a symbol — the format name (e.g. json, yaml).
FLAGS is a list of flag strings, e.g. (\"--json\").

Inherits all `clime-option' behavior (hidden, category, etc.)
since output formats ARE options.

Keyword arguments:
  :help STR         Help text for the flag
  :finalize FN      Envelope function: (items retval errors) → data | nil
  :streaming t      Emit immediately, bypass accumulator
  :encoder FN       Data → string encoder
  :error-handler FN Error handler: (msg) → side effect
  :hidden t         Omit from help
  :category STR     Help display category
  :deprecated STR   Deprecation message or t"
  (declare (indent 2))
  (ignore help finalize streaming encoder error-handler hidden category deprecated)
  (clime--build-output-format (cons name (cons flags plist))))

(defmacro clime-mutex (name &rest body)
  "Define an exclusive (mutual exclusion) group NAME with BODY.
NAME is a symbol — the group name (used as key in ctx).
BODY is a mix of keyword args and clime-option forms.

The winner's option name is injected into params under NAME.
Expands to an inline group with conformer via `clime-check-exclusive'."
  (declare (indent 1))
  (clime--build-mutex-conform (cons name body)))

(defmacro clime-zip (name &rest body)
  "Define a paired (zip) group NAME with BODY.
NAME is a symbol — the group name (used as key in ctx).
BODY is a mix of keyword args and clime-option forms.

Wrapped options auto-get :multiple set.
A zipped alist is injected into params under NAME.
Expands to an inline group with conformer via `clime-check-paired'."
  (declare (indent 1))
  (clime--build-zip-conform (cons name body)))

(defmacro clime-handler (arglist-or-fn &rest body)
  "Define a command handler.
With ARGLIST-OR-FN as a list and BODY, produces (lambda ARGLIST-OR-FN BODY).
With a single function reference, passes it through:
  (clime-handler #\\='my-fn)  → #\\='my-fn
  (clime-handler \\='my-fn)   → \\='my-fn"
  (declare (indent 1))
  (if (and (null body)
           (or (not (listp arglist-or-fn))
               (memq (car-safe arglist-or-fn) '(function quote))))
      arglist-or-fn
    `(lambda ,arglist-or-fn ,@body)))

(provide 'clime-dsl)
;;; clime-dsl.el ends here
