;;; clime-param-type.el --- Parameter type registry and resolution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a registry-based type system for clime.  Type specs (nil,
;; symbols, or S-expression lists) resolve to type plists containing
;; :parse, :describe, and optionally :choices.  Built-in and user-defined
;; types are registered via `clime-register-type' or `clime-deftype'.

;;; Code:

;;; ─── Error Condition ───────────────────────────────────────────────────

(define-error 'clime-type-error "Clime type error")

;;; ─── Registry ──────────────────────────────────────────────────────────

(defvar clime-type-registry (make-hash-table :test 'eq)
  "Map type-name symbols to constructor functions.
Each constructor receives the cdr of a type spec as arguments and
returns a type plist (:parse FN :describe STR [:choices LIST]).")

;;; ─── Validation ────────────────────────────────────────────────────────

(defun clime-type-plist-p (plist)
  "Return non-nil if PLIST is a valid resolved type plist.
A valid type plist has a function at :parse and a string at :describe."
  (and (plistp plist)
       (functionp (plist-get plist :parse))
       (stringp (plist-get plist :describe))))

;;; ─── Registration ──────────────────────────────────────────────────────

(defun clime-register-type (name constructor)
  "Register CONSTRUCTOR under NAME in the type registry.
CONSTRUCTOR is a function that accepts the cdr of a type spec
and returns a type plist (:parse FN :describe STR [:choices LIST]).
Signals error if NAME is not a symbol or CONSTRUCTOR is not a function."
  (unless (symbolp name)
    (signal 'clime-type-error
            (list (format "Type name must be a symbol, got %S" name))))
  (unless (functionp constructor)
    (signal 'clime-type-error
            (list (format "Type constructor must be a function, got %S" constructor))))
  (puthash name constructor clime-type-registry))

;;; ─── Resolution ────────────────────────────────────────────────────────

(defun clime-resolve-type (spec)
  "Resolve type SPEC to a type plist (:parse FN :describe STR [:choices LIST]).
SPEC is nil (string passthrough), a symbol (registry lookup),
or a list (HEAD . ARGS) where HEAD is a registered type name."
  (cond
   ((null spec)
    (list :parse #'identity :describe "string"))
   ((symbolp spec)
    (let ((ctor (gethash spec clime-type-registry)))
      (unless ctor
        (signal 'clime-type-error
                (list (format "Unknown type `%s'" spec))))
      (funcall ctor)))
   ((consp spec)
    (let* ((head (car spec))
           (args (cdr spec))
           (ctor (gethash head clime-type-registry)))
      (unless ctor
        (signal 'clime-type-error
                (list (format "Unknown type `%s'" head))))
      (apply ctor args)))
   (t
    (signal 'clime-type-error
            (list (format "Invalid type spec: %S" spec))))))

;;; ─── Definition Macro ──────────────────────────────────────────────────

(defmacro clime-deftype (name arglist docstring &rest body)
  "Define and register a type constructor NAME.
ARGLIST receives the cdr of the type spec (e.g., for `(integer 1 100)',
ARGLIST gets `(1 100)').  BODY must return a type plist
\(:parse FN :describe STR [:choices LIST]).

Example:
  (clime-deftype port ()
    \"TCP port number.\"
    (clime-resolve-type \\='(integer 1 65535)))"
  (declare (indent defun) (doc-string 3))
  (let ((fn-name (intern (format "clime-type--%s" name))))
    `(progn
       (defun ,fn-name ,arglist
         ,docstring
         ,@body)
       (clime-register-type ',name #',fn-name))))

;;; ─── Built-in Types ─────────────────────────────────────────────────────

(clime-deftype string ()
  "String passthrough (default type)."
  (list :parse #'identity :describe "string"))

(clime-deftype integer ()
  "Strict integer parser."
  (list :parse (lambda (value)
                 (let ((n (string-to-number value)))
                   (unless (and (integerp n)
                                (string-match-p "\\`-?[0-9]+\\'" value))
                     (error "Expected integer, got \"%s\"" value))
                   n))
        :describe "integer"))

(clime-deftype number ()
  "Number parser (integer or float)."
  (list :parse (lambda (value)
                 (let ((n (string-to-number value)))
                   (when (and (= n 0)
                              (not (string-match-p "\\`-?0+\\.?0*\\'" value)))
                     (error "Expected number, got \"%s\"" value))
                   n))
        :describe "number"))

(clime-deftype boolean ()
  "Boolean parser (truthy/falsy strings)."
  (list :parse (lambda (value)
                 (pcase (downcase (string-trim value))
                   ((or "1" "true" "yes" "on" "t") t)
                   ((or "0" "false" "no" "off" "nil" "") nil)
                   (_ (error "Expected boolean, got \"%s\"" value))))
        :describe "boolean"))

(provide 'clime-param-type)
;;; clime-param-type.el ends here
