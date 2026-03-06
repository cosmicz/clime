;;; clime-dsl.el --- Declarative DSL for building CLI apps  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

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
REST is the remaining non-keyword forms."
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
    (cons keywords (nreverse rest))))

(defun clime--classify-body (forms)
  "Classify FORMS into options, args, children, and handler.
Returns a plist (:options :args :children :handler)."
  (let (options args children handler)
    (dolist (form forms)
      (when (consp form)
        (pcase (car form)
          ('clime-option (push (clime--build-option (cdr form)) options))
          ('clime-arg (push (clime--build-arg (cdr form)) args))
          ('clime-command (push (clime--build-command (cdr form)) children))
          ('clime-group (push (clime--build-group (cdr form)) children))
          ('clime-handler (setq handler (clime--build-handler (cdr form))))
          (_ (error "Unknown DSL form: %S" (car form))))))
    (list :options (nreverse options)
          :args (nreverse args)
          :children (nreverse children)
          :handler handler)))

;;; ─── Form Builders ──────────────────────────────────────────────────────

(defun clime--build-option (args)
  "Build a `clime-option' constructor form from DSL ARGS.
ARGS is (NAME FLAGS &rest PLIST)."
  (let* ((name (car args))
         (flags (cadr args))
         (plist (cddr args)))
    `(clime-make-option :name ',name :flags ',flags ,@plist)))

(defun clime--build-arg (args)
  "Build a `clime-arg' constructor form from DSL ARGS.
ARGS is (NAME &rest PLIST)."
  (let* ((name (car args))
         (plist (cdr args)))
    `(clime-make-arg :name ',name ,@plist)))

(defun clime--build-handler (args)
  "Build a lambda form from DSL ARGS.
ARGS is (ARGLIST &rest BODY)."
  (let ((arglist (car args))
        (body (cdr args)))
    `(lambda ,arglist ,@body)))

(defun clime--build-command (args)
  "Build a `clime-command' constructor form from DSL ARGS.
ARGS is (NAME &rest BODY)."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:help :aliases :hidden)))
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
                `(:aliases ',(plist-get keywords :aliases)))
            ,@(when (plist-get keywords :hidden)
                `(:hidden ,(plist-get keywords :hidden)))
            ,@(when (plist-get classified :options)
                `(:options (list ,@(plist-get classified :options))))
            ,@(when (plist-get classified :args)
                `(:args (list ,@(plist-get classified :args))))))))

(defun clime--build-group (args)
  "Build a `clime-group' constructor form from DSL ARGS.
ARGS is (NAME &rest BODY)."
  (let* ((name (car args))
         (name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     (cdr args)
                     '(:help :aliases :hidden)))
         (keywords (car extracted))
         (body-forms (cdr extracted))
         (classified (clime--classify-body body-forms)))
    `(cons ,name-str
           (clime-make-group
            :name ,name-str
            ,@(when (plist-get keywords :help)
                `(:help ,(plist-get keywords :help)))
            ,@(when (plist-get keywords :aliases)
                `(:aliases ',(plist-get keywords :aliases)))
            ,@(when (plist-get keywords :hidden)
                `(:hidden ,(plist-get keywords :hidden)))
            ,@(when (plist-get classified :options)
                `(:options (list ,@(plist-get classified :options))))
            ,@(when (plist-get classified :args)
                `(:args (list ,@(plist-get classified :args))))
            ,@(when (plist-get classified :children)
                `(:children (list ,@(plist-get classified :children))))
            ,@(when (plist-get classified :handler)
                `(:invoke ,(plist-get classified :handler)))))))

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
  :json-mode BOOL — enable built-in --json option

Child forms:
  (clime-option NAME FLAGS &rest PLIST)
  (clime-arg NAME &rest PLIST)
  (clime-command NAME &rest BODY)
  (clime-group NAME &rest BODY)"
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (extracted (clime--extract-keywords
                     body
                     '(:version :env-prefix :help :json-mode)))
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
        ,@(when (plist-get classified :options)
            `(:options (list ,@(plist-get classified :options))))
        ,@(when (plist-get classified :args)
            `(:args (list ,@(plist-get classified :args))))
        ,@(when (plist-get classified :children)
            `(:children (list ,@(plist-get classified :children))))
        ,@(when (plist-get classified :handler)
            `(:invoke ,(plist-get classified :handler)))))))

(provide 'clime-dsl)
;;; clime-dsl.el ends here
