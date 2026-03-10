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
Respects :group labels and :hidden flags."
  (let ((visible (cl-remove-if #'clime-option-hidden options)))
    (when visible
      (let ((grouped (make-hash-table :test 'equal))
            (group-order '()))
        ;; Partition by group label
        (dolist (opt visible)
          (let ((grp (or (clime-option-group opt) "")))
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

(defun clime-help--effective-children (node)
  "Return the effective children alist for NODE.
Default groups are replaced by their children (inlined at parent level)."
  (cl-mapcan (lambda (entry)
               (let ((child (cdr entry)))
                 (if (and (clime-group-p child)
                          (clime-node-inline child))
                     (copy-sequence (clime-group-children child))
                   (list entry))))
             (clime-group-children node)))

(defun clime-help--format-commands (children)
  "Format the Commands section for CHILDREN alist."
  (let* ((visible (cl-remove-if
                   (lambda (entry)
                     (clime-node-hidden (cdr entry)))
                   children)))
    (when visible
      (let ((rows (mapcar
                   (lambda (entry)
                     (cons (car entry)
                           (clime-help--first-line
                            (or (clime-node-help (cdr entry)) ""))))
                   visible)))
        (concat "Commands:\n" (clime-help--format-table rows))))))

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
    ;; Options
    (let ((opts-section (clime-help--format-options (clime-node-options node))))
      (when opts-section
        (push opts-section sections)))
    ;; Commands (for groups), inlining default groups
    (when (clime-group-only-p node)
      (let ((cmds-section (clime-help--format-commands
                           (clime-help--effective-children node))))
        (when cmds-section
          (push cmds-section sections))))
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
