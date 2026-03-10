;;; clime-help.el --- Help formatter for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Auto-generated help text for CLI nodes.  Pure formatting — takes a
;; node and path, returns a string.  No parsing, no dispatch.

;;; Code:

(require 'cl-lib)
(require 'clime-core)

;;; ─── Utilities ──────────────────────────────────────────────────────────

(defun clime-help--first-line (text)
  "Return the first line of TEXT, or TEXT if single-line."
  (if (and text (string-match "\n" text))
      (substring text 0 (match-beginning 0))
    text))

;;; ─── Column Alignment ─────────────────────────────────────────────────

(defconst clime-help--indent "  "
  "Indentation prefix for help entries.")

(defconst clime-help--min-gap 4
  "Minimum gap between left column and help text.")

(defun clime-help--format-table (rows)
  "Format ROWS as a two-column table.
Each row is (LEFT-STRING . HELP-STRING).  Left column is padded
to the widest entry plus `clime-help--min-gap'."
  (if (null rows)
      ""
    (let* ((max-left (cl-reduce #'max rows :key (lambda (r) (length (car r)))))
           (pad (+ max-left clime-help--min-gap)))
      (mapconcat
       (lambda (row)
         (let ((left (car row))
               (help (or (cdr row) "")))
           (if (string-empty-p help)
               (concat clime-help--indent left)
             (concat clime-help--indent
                     left
                     (make-string (max 1 (- pad (length left))) ?\s)
                     help))))
       rows "\n"))))

;;; ─── Section Formatters ───────────────────────────────────────────────

(defun clime-help--format-usage (node path)
  "Build the usage line for NODE at PATH."
  (let* ((path-str (string-join path " "))
         (has-options (not (null (clime-node-options node))))
         (has-children (and (clime-group-only-p node)
                            (clime-group-children node)))
         (args (clime-node-args node))
         (parts (list (format "Usage: %s" path-str))))
    (when has-options
      (push "[OPTIONS]" parts))
    (when has-children
      (push "COMMAND" parts))
    (dolist (arg args)
      (let* ((name-str (format "<%s>" (clime-arg-name arg)))
             (rest-p (eq (clime-arg-nargs arg) :rest))
             (req (clime-arg-required arg)))
        (push (cond
               (rest-p (format "[%s...]" name-str))
               (req name-str)
               (t (format "[%s]" name-str)))
              parts)))
    (string-join (nreverse parts) " ")))

(defun clime-help--choices-suffix (choices)
  "Return a choices annotation string, or nil if CHOICES is empty.
CHOICES may be a list or a function returning a list."
  (let ((resolved (clime--resolve-value choices)))
    (when resolved
      (format "(choices: %s)"
              (mapconcat (lambda (c) (format "%s" c)) resolved ", ")))))

(defun clime-help--append-choices (help-text choices)
  "Append choices annotation to HELP-TEXT if CHOICES is non-nil."
  (let ((suffix (clime-help--choices-suffix choices)))
    (if suffix
        (if (and help-text (not (string-empty-p help-text)))
            (concat help-text " " suffix)
          suffix)
      (or help-text ""))))

(defun clime-help--format-arguments (args)
  "Format the Arguments section for ARGS list."
  (let ((visible (cl-remove-if #'null args)))
    (when visible
      (let ((rows (mapcar
                   (lambda (arg)
                     (let ((name (format "<%s>" (clime-arg-name arg)))
                           (help (clime-help--append-choices
                                  (clime-arg-help arg)
                                  (clime-arg-choices arg))))
                       (cons name help)))
                   visible)))
        (concat "Arguments:\n" (clime-help--format-table rows))))))

(defun clime-help--format-option-flags (opt)
  "Format the flag column for OPT.
Short flags first, then long.  Appends VALUE for value-taking options.
Appends repeat indicator for :count and :multiple options."
  (let* ((flags (clime-option-flags opt))
         (shorts (cl-remove-if-not (lambda (f) (and (= (length f) 2)
                                                     (= (aref f 0) ?-)))
                                   flags))
         (longs (cl-remove-if (lambda (f) (and (= (length f) 2)
                                                (= (aref f 0) ?-)))
                              flags))
         (sorted (append shorts longs))
         (base (string-join sorted ", "))
         (boolean-p (clime-option-boolean-p opt)))
    (concat base
            (unless boolean-p " VALUE")
            (cond
             ((clime-option-count opt) " ...")
             ((clime-option-multiple opt) " ...")
             (t "")))))

(defun clime-help--format-options (options)
  "Format the Options section for OPTIONS list.
Respects :category labels and :hidden flags."
  (let ((visible (cl-remove-if #'clime-option-hidden options)))
    (when visible
      (let ((grouped (make-hash-table :test 'equal))
            (group-order '()))
        ;; Partition by group label
        (dolist (opt visible)
          (let ((grp (or (clime-option-category opt) "")))
            (unless (gethash grp grouped)
              (push grp group-order))
            (push opt (gethash grp grouped))))
        (setq group-order (nreverse group-order))
        ;; Format each group
        (let ((sections '()))
          (dolist (grp group-order)
            (let* ((opts (nreverse (gethash grp grouped)))
                   (rows (mapcar
                          (lambda (opt)
                            (cons (clime-help--format-option-flags opt)
                                  (clime-help--append-choices
                                   (clime-option-help opt)
                                   (clime-option-choices opt))))
                          opts))
                   (header (if (string-empty-p grp) "Options:" (format "%s:" grp))))
              (push (concat header "\n" (clime-help--format-table rows)) sections)))
          (string-join (nreverse sections) "\n\n"))))))

(defun clime-help--compose-category (parent child)
  "Compose PARENT and CHILD category strings into a path.
Returns CHILD if PARENT is nil, PARENT if CHILD is nil,
or \"PARENT / CHILD\" if both are set."
  (cond
   ((and parent child) (concat parent " / " child))
   (parent parent)
   (t child)))

(defun clime-help--effective-children (node &optional parent-category)
  "Return the effective children alist for NODE.
Inline groups are recursively replaced by their children (promoted to
parent level).  Category paths are composed: a group's :category is
combined with PARENT-CATEGORY from enclosing inline groups.
Children inherit the composed category unless they have their own."
  (cl-mapcan (lambda (entry)
               (let ((child (cdr entry)))
                 (if (and (clime-group-p child)
                          (clime-node-inline child))
                     (let ((composed (clime-help--compose-category
                                     parent-category
                                     (clime-node-category child))))
                       (clime-help--effective-children child composed))
                   ;; Leaf entry: apply inherited category
                   (if (and parent-category
                            (not (clime-node-category child)))
                       (let ((copy (copy-sequence child)))
                         (setf (clime-node-category copy) parent-category)
                         (list (cons (car entry) copy)))
                     ;; Child has own category — compose with parent
                     (if (and parent-category
                              (clime-node-category child))
                         (let ((copy (copy-sequence child)))
                           (setf (clime-node-category copy)
                                 (clime-help--compose-category
                                  parent-category
                                  (clime-node-category child)))
                           (list (cons (car entry) copy)))
                       (list entry))))))
             (clime-group-children node)))

(defun clime-help--option-row (opt)
  "Format OPT as a (left . help) table row."
  (cons (clime-help--format-option-flags opt)
        (clime-help--append-choices
         (clime-option-help opt)
         (clime-option-choices opt))))

(defun clime-help--command-row (entry)
  "Format children alist ENTRY as a (left . help) table row."
  (cons (car entry)
        (clime-help--first-line
         (or (clime-node-help (cdr entry)) ""))))

(defun clime-help--format-sections (options children)
  "Format OPTIONS and CHILDREN into interleaved category sections.
Options and children sharing a :category are grouped under one heading.
Uncategorized options use \"Options:\", uncategorized children use
\"Commands:\".  Sections appear in first-occurrence order."
  (let ((grouped (make-hash-table :test 'equal))
        (order '()))
    ;; Collect options
    (dolist (opt (cl-remove-if #'clime-option-hidden options))
      (let ((key (or (clime-option-category opt) :options)))
        (unless (gethash key grouped)
          (push key order)
          (puthash key (list nil nil) grouped))  ; (options . children)
        (push opt (car (gethash key grouped)))))
    ;; Collect children
    (dolist (entry (cl-remove-if (lambda (e) (clime-node-hidden (cdr e))) children))
      (let ((key (or (clime-node-category (cdr entry)) :commands)))
        (unless (gethash key grouped)
          (push key order)
          (puthash key (list nil nil) grouped))
        (push entry (cadr (gethash key grouped)))))
    (setq order (nreverse order))
    ;; Render sections
    (let ((sections '()))
      (dolist (key order)
        (let* ((pair (gethash key grouped))
               (opts (nreverse (car pair)))
               (cmds (nreverse (cadr pair)))
               (rows (append (mapcar #'clime-help--option-row opts)
                             (mapcar #'clime-help--command-row cmds)))
               (header (cond
                        ((eq key :options) "Options:")
                        ((eq key :commands) "Commands:")
                        (t (format "%s:" key)))))
          (push (concat header "\n" (clime-help--format-table rows)) sections)))
      (when sections
        (string-join (nreverse sections) "\n\n")))))

(defun clime-help--collect-ancestor-options (node)
  "Collect all options from ancestors of NODE.
Walk the parent chain, gathering each ancestor's non-hidden options.
Returns a flat list of `clime-option' structs, deduped by flag set."
  (let ((parent (clime-node-parent node))
        (seen-flags (make-hash-table :test 'equal))
        (result '()))
    ;; Mark current node's flags as seen (so we don't duplicate)
    (dolist (opt (clime-node-options node))
      (dolist (flag (clime-option-flags opt))
        (puthash flag t seen-flags)))
    ;; Walk ancestors
    (while parent
      (dolist (opt (clime-node-options parent))
        (unless (or (clime-option-hidden opt)
                    (cl-some (lambda (flag) (gethash flag seen-flags))
                             (clime-option-flags opt)))
          (dolist (flag (clime-option-flags opt))
            (puthash flag t seen-flags))
          (push opt result)))
      (setq parent (clime-node-parent parent)))
    (nreverse result)))

;;; ─── Public API ────────────────────────────────────────────────────────

(defun clime-format-help (node path)
  "Return formatted help string for NODE at PATH."
  (let ((sections (list (clime-help--format-usage node path)))
        (help-text (clime-node-help node)))
    ;; Description
    (when (and help-text (not (string-empty-p help-text)))
      (push help-text sections))
    ;; Arguments
    (let ((args-section (clime-help--format-arguments (clime-node-args node))))
      (when args-section
        (push args-section sections)))
    ;; Options + Commands (unified sections with interleaved categories)
    (if (clime-group-only-p node)
        (let ((unified (clime-help--format-sections
                        (clime-node-options node)
                        (clime-help--effective-children node))))
          (when unified
            (push unified sections)))
      ;; Leaf commands: options only, no children
      (let ((opts-section (clime-help--format-options (clime-node-options node))))
        (when opts-section
          (push opts-section sections))))
    ;; Global Options (inherited from ancestors)
    (let* ((ancestor-opts (clime-help--collect-ancestor-options node))
           (global-section (when ancestor-opts
                             (let ((rows (mapcar
                                          (lambda (opt)
                                            (cons (clime-help--format-option-flags opt)
                                                  (clime-help--append-choices
                                                   (clime-option-help opt)
                                                   (clime-option-choices opt))))
                                          ancestor-opts)))
                               (concat "Global Options:\n"
                                       (clime-help--format-table rows))))))
      (when global-section
        (push global-section sections)))
    ;; Epilog
    (let ((epilog (clime-node-epilog node)))
      (when (and epilog (not (string-empty-p epilog)))
        (push epilog sections)))
    (concat (string-join (nreverse sections) "\n\n") "\n")))

(defun clime-format-version (app)
  "Return version string for APP."
  (format "%s %s\n" (clime-app-name app) (clime-app-version app)))

(provide 'clime-help)
;;; clime-help.el ends here
