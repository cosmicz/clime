;;; clime-contract.el --- Surface-aware command metadata policy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module owns discovery policy, not a second command-tree data model.
;; Adapters retain the live `clime-app' and use these functions to select
;; eligible nodes and safe declaration values before rendering their own wire
;; formats.  The only derived metadata is a canonical path, which a node does
;; not carry itself.

;;; Code:

(require 'cl-lib)
(require 'clime-core)

(defconst clime-contract--policy-keys
  '(:surface :tree-mode :visibility :value-mode)
  "Required and exhaustive keys accepted by contract policy functions.")

(defconst clime-contract--tree-modes '(declaration prepared))
(defconst clime-contract--visibility-modes '(all visible public))
(defconst clime-contract--value-modes '(static-safe declared))

(defun clime-contract--validate-policy (policy)
  "Validate and return explicit POLICY.

Policy validation is intentionally repeated at every public boundary."
  (unless (proper-list-p policy)
    (error "clime-contract: policy must be a property list"))
  (let ((rest policy) seen)
    (while rest
      (unless (cdr rest)
        (error "clime-contract: policy must be a property list"))
      (unless (memq (car rest) clime-contract--policy-keys)
        (error "clime-contract: unknown policy key %S" (car rest)))
      (when (memq (car rest) seen)
        (error "clime-contract: duplicate policy key %S" (car rest)))
      (push (car rest) seen)
      (setq rest (cddr rest))))
  (dolist (key clime-contract--policy-keys)
    (unless (plist-member policy key)
      (error "clime-contract: missing required policy key %S" key)))
  (unless (memq (plist-get policy :surface) clime-surface-symbols)
    (error "clime-contract: unknown surface %S" (plist-get policy :surface)))
  (unless (memq (plist-get policy :tree-mode) clime-contract--tree-modes)
    (error "clime-contract: unknown tree mode %S" (plist-get policy :tree-mode)))
  (unless (memq (plist-get policy :visibility) clime-contract--visibility-modes)
    (error "clime-contract: unknown visibility mode %S"
           (plist-get policy :visibility)))
  (unless (memq (plist-get policy :value-mode) clime-contract--value-modes)
    (error "clime-contract: unknown value mode %S"
           (plist-get policy :value-mode)))
  policy)

(defun clime-contract--prepared-tree (app)
  "Return an alias-prepared copy of APP without modifying APP.

`clime--prepare-tree' prepares the registered tree before copying it.  The
contract must remain read-only, so preparation happens wholly on this copy."
  (let ((copy (clime--deep-copy-tree app)))
    (clime--set-parent-refs copy)
    (clime--resolve-aliases-walk copy copy)
    (clime--set-parent-refs copy)
    copy))

(defun clime-contract--tree (app policy)
  "Return APP or a prepared copy according to validated POLICY."
  (pcase (plist-get policy :tree-mode)
    ('declaration app)
    ('prepared (clime-contract--prepared-tree app))))

(defun clime-contract--node-visible-p (node visibility)
  "Return non-nil when NODE remains under VISIBILITY."
  (or (eq visibility 'all) (not (clime-node-hidden node))))

(defun clime-contract--option-visible-p (option visibility)
  "Return non-nil when OPTION remains under VISIBILITY."
  (and (or (eq visibility 'all) (not (clime-option-hidden option)))
       (or (not (eq visibility 'public)) (not (clime-option-locked option)))))

(defun clime-contract--child-path (node path)
  "Return NODE's canonical PATH below its parent PATH."
  (if (clime-node-inline node)
      (copy-sequence path)
    (append path (list (clime-node-name node)))))

(defun clime-contract--collect-nodes (node path policy)
  "Return stable eligible (NODE . PATH) pairs below NODE."
  (when (and (clime-node-surface-eligible-p node (plist-get policy :surface))
             (clime-contract--node-visible-p node
                                             (plist-get policy :visibility)))
    (append (list (cons node (copy-sequence path)))
            (when (clime-group-p node)
              (cl-mapcan
               (lambda (entry)
                 (let ((child (cdr entry)))
                   (clime-contract--collect-nodes
                    child (clime-contract--child-path child path) policy)))
               (clime-group-children node))))))

;;;###autoload
(defun clime-contract-nodes (app &rest policy)
  "Return APP's stable, policy-filtered (NODE . CANONICAL-PATH) traversal.

The root path is nil; app roots and inline groups do not add a segment.
Prepared mode returns nodes from a private prepared copy, so aliases retain
their alias path and locked/default overlays without mutating APP."
  (unless (clime-app-p app)
    (error "clime-contract: APP must be a clime-app"))
  (setq policy (clime-contract--validate-policy policy))
  (clime-contract--collect-nodes (clime-contract--tree app policy) nil policy))

;;;###autoload
(defun clime-contract-find (app path &rest policy)
  "Return APP's policy-eligible (NODE . PATH) pair for canonical PATH.

Returns nil when PATH is unavailable under POLICY."
  (setq policy (clime-contract--validate-policy policy))
  (unless (clime-app-p app)
    (error "clime-contract: APP must be a clime-app"))
  (cl-find path (apply #'clime-contract-nodes app policy)
           :key #'cdr :test #'equal))

;;;###autoload
(defun clime-contract-options (node &rest policy)
  "Return NODE's declaration options allowed by POLICY visibility."
  (setq policy (clime-contract--validate-policy policy))
  (unless (clime-node-p node)
    (error "clime-contract: NODE must be a clime node"))
  (cl-remove-if-not
   (lambda (option)
     (clime-contract--option-visible-p option (plist-get policy :visibility)))
   (clime-node-options node)))

(defun clime-contract--scope-option-owners (node &optional owner)
  "Return declaration (OPTION . OWNER) pairs from NODE and inline groups.

Inline-group options retain the enclosing non-inline OWNER path."
  (setq owner (or owner node))
  (append (mapcar (lambda (option) (cons option owner))
                  (clime-node-options node))
          (when (clime-group-p node)
            (cl-mapcan
             (lambda (entry)
               (let ((child (cdr entry)))
                 (if (and (clime-group-p child) (clime-node-inline child))
                     (clime-contract--scope-option-owners child owner)
                   nil)))
             (clime-group-children node)))))

(defun clime-contract--canonical-path (node)
  "Derive NODE's canonical path from its parent chain."
  (let ((path nil))
    (dolist (scope (append (reverse (clime-node-ancestors node)) (list node)) path)
      (unless (or (clime-app-p scope) (clime-node-inline scope))
        (setq path (append path (list (clime-node-name scope))))))))

;;;###autoload
(defun clime-contract-effective-options (command app &rest policy)
  "Return effective (OPTION . OWNER-PATH) pairs for COMMAND in APP.

Options are ordered from root scope to command scope; inline-group options
belong to the surrounding command path."
  (setq policy (clime-contract--validate-policy policy))
  (unless (and (clime-command-p command) (clime-app-p app))
    (error "clime-contract: COMMAND and APP are required"))
  ;; COMMAND may be a prepared node returned by `clime-contract-find'.  Its
  ;; parent chain is authoritative and avoids a second APP traversal.
  (cl-remove-if-not
   (lambda (pair)
     (clime-contract--option-visible-p (car pair)
                                       (plist-get policy :visibility)))
   (mapcar (lambda (pair)
             (cons (car pair)
                   (clime-contract--canonical-path (cdr pair))))
           (cl-mapcan #'clime-contract--scope-option-owners
                      (append (reverse (clime-node-ancestors command))
                              (list command))))))

(defun clime-contract--safe-literal-p (value)
  "Return non-nil when VALUE is declaration data safe for static export."
  (or (stringp value) (numberp value) (keywordp value) (eq value t)
      (and (symbolp value) (not (functionp value)))
      (and (proper-list-p value)
           (cl-every #'clime-contract--safe-literal-p value))))

(defun clime-contract--param-choices (param)
  "Return PARAM's declaration choices without resolving them."
  (if (clime-option-p param)
      (clime-option-choices param)
    (clime-arg-choices param)))

;;;###autoload
(defun clime-contract-safe-default (param &rest policy)
  "Return PARAM's safe declaration default, or nil when dynamic or opaque."
  (setq policy (clime-contract--validate-policy policy))
  (unless (clime-param-p param)
    (error "clime-contract: PARAM must be a clime parameter"))
  (let ((value (clime-param-default param)))
    (when (and (not (functionp value))
               (or (eq (plist-get policy :value-mode) 'declared)
                   (clime-contract--safe-literal-p value)))
      value)))

;;;###autoload
(defun clime-contract-safe-choices (param &rest policy)
  "Return PARAM's literal choices, `dynamic', or nil without evaluation."
  (setq policy (clime-contract--validate-policy policy))
  (unless (clime-param-p param)
    (error "clime-contract: PARAM must be a clime parameter"))
  (let ((choices (clime-contract--param-choices param)))
    (cond ((null choices) nil)
          ((or (functionp choices) (not (listp choices))) 'dynamic)
          (t choices))))

;;;###autoload
(defun clime-contract-describe-type (type &rest policy)
  "Return a safe declaration-only description for TYPE.

This deliberately does not call the type registry: custom type constructors
are executable code.  Function-valued types are opaque and return nil."
  (setq policy (clime-contract--validate-policy policy))
  (cond ((null type) "string")
        ((and (functionp type) (not (symbolp type))) nil)
        ((symbolp type) (symbol-name type))
        ((and (consp type) (symbolp (car type))) (symbol-name (car type)))
        (t nil)))

(provide 'clime-contract)
;;; clime-contract.el ends here
