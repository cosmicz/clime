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
(require 'clime-settings)

;;; ─── Utilities ──────────────────────────────────────────────────────────

(defun clime-help--first-line (text)
  "Return the first line of TEXT, or TEXT if single-line."
  (if (and text (string-match "\n" text))
      (substring text 0 (match-beginning 0))
    text))

(defconst clime-help--min-width 40
  "Minimum terminal width for help formatting.")

(defun clime-help--terminal-width ()
  "Return the terminal width for help text wrapping.
Uses `clime-help-width' if set, else COLUMNS env var, else 80.
Result is clamped to a minimum of `clime-help--min-width'."
  (max clime-help--min-width
       (cond
        ((and (integerp clime-help-width) (> clime-help-width 0))
         clime-help-width)
        (t (let ((cols (getenv "COLUMNS")))
             (if (and cols (string-match-p "\\`[0-9]+\\'" cols))
                 (string-to-number cols)
               80))))))

(defun clime-help--wrap-text (text width &optional prefix)
  "Word-wrap TEXT to WIDTH columns.
PREFIX is prepended to continuation lines (not the first line).
The WIDTH budget applies to the content on every line (PREFIX is
extra visual indentation, not counted against WIDTH).
Preserves existing newlines.  Words longer than WIDTH are not broken."
  (let ((prefix (or prefix ""))
        (paragraphs (split-string text "\n")))
    (mapconcat
     (lambda (para)
       (if (<= (length para) width)
           para
         (let ((words (split-string para " " t))
               (lines '())
               (current ""))
           (dolist (word words)
             (let ((tentative (if (string-empty-p current)
                                  word
                                (concat current " " word))))
               (cond
                ;; Fits on current line
                ((<= (length tentative) width)
                 (setq current tentative))
                ;; Current line has content — flush and start new line
                ((not (string-empty-p current))
                 (push current lines)
                 (setq current word))
                ;; Empty line, word alone exceeds width — emit as-is
                (t
                 (push word lines)
                 (setq current "")))))
           (unless (string-empty-p current)
             (push current lines))
           (let ((result (nreverse lines)))
             (concat (car result)
                     (if (cdr result)
                         (concat "\n" (mapconcat
                                       (lambda (l) (concat prefix l))
                                       (cdr result) "\n"))
                       ""))))))
     paragraphs "\n")))

;;; ─── Column Alignment ─────────────────────────────────────────────────

(defconst clime-help--indent "  "
  "Indentation prefix for help entries.")

(defconst clime-help--min-gap 4
  "Minimum gap between left column and help text.")

(defun clime-help--format-table (rows &optional width)
  "Format ROWS as a two-column table.
Each row is (LEFT-STRING . HELP-STRING).  Left column is padded
to the widest entry plus `clime-help--min-gap'.
When WIDTH is non-nil, help text is wrapped to fit within WIDTH columns."
  (if (null rows)
      ""
    (let* ((max-left (cl-reduce #'max rows :key (lambda (r) (length (car r)))))
           (pad (+ max-left clime-help--min-gap))
           (indent-len (length clime-help--indent))
           (help-col (+ indent-len pad))
           (help-width (when width (- width help-col)))
           (cont-prefix (when (and help-width (> help-width 0))
                          (make-string help-col ?\s))))
      (mapconcat
       (lambda (row)
         (let ((left (car row))
               (help (or (cdr row) "")))
           (if (string-empty-p help)
               (concat clime-help--indent left)
             (let ((wrapped (if (and help-width (> help-width 0))
                                (clime-help--wrap-text help help-width cont-prefix)
                              help)))
               (concat clime-help--indent
                       left
                       (make-string (max 1 (- pad (length left))) ?\s)
                       wrapped)))))
       rows "\n"))))

;;; ─── Section Formatters ───────────────────────────────────────────────

(defun clime-help--format-usage (node path)
  "Build the usage line for NODE at PATH."
  (let* ((path-str (string-join path " "))
         (has-options (not (null (clime-node-options node))))
         (has-children (and (clime-branch-p node)
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

(defun clime-help--format-arguments (args &optional width)
  "Format the Arguments section for ARGS list.
When WIDTH is non-nil, help text is wrapped to fit."
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
        (concat "Arguments:\n" (clime-help--format-table rows width))))))

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

(defun clime-help--format-options (options &optional width)
  "Format the Options section for OPTIONS list.
Respects :category labels and :hidden flags.
When WIDTH is non-nil, help text is wrapped to fit."
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
              (push (concat header "\n" (clime-help--format-table rows width)) sections)))
          (string-join (nreverse sections) "\n\n"))))))

(defun clime-help--category-path (node stop-at)
  "Build category path from NODE up to (but not including) STOP-AT.
Walks `clime-node-parent', collecting non-nil `clime-node-category'
values.  Returns a list of category strings, outermost first."
  (let ((segments '())
        (cur node))
    (while (and cur (not (eq cur stop-at)))
      (let ((cat (clime-node-category cur)))
        (when cat (push cat segments)))
      (setq cur (clime-node-parent cur)))
    segments))

(defun clime-help--collect-items (node)
  "Collect help items from NODE using `clime-node-collect'.
Returns flat list of (CATEGORY-PATH TYPE ITEM SCOPE) where:
  CATEGORY-PATH — list of category strings (outermost first), or nil.
  TYPE          — :option, :command, or :group.
  ITEM          — the struct.
  SCOPE         — list of command name strings (scope annotation), or nil.

Options on uncategorized inline groups with no own :category are skipped.
SCOPE is set for options that have a sub-category from an inline group,
listing the group's command children to indicate which commands the
options apply to."
  (let ((raw (clime-node-collect node))
        (items '()))
    (dolist (entry raw)
      (let* ((type (car entry))
             (item (cadr entry)))
        (pcase type
          (:group
           ;; Emit group if it has :help (for section descriptions)
           (let ((path (clime-help--category-path item node)))
             (when (and path (clime-node-help item))
               (push (list path :group item nil) items))))
          (:option
           (let* ((owner (caddr entry))
                  (path (clime-help--category-path owner node))
                  (own-cat (clime-option-category item))
                  (full-path (if own-cat (append path (list own-cat)) path))
                  ;; Scope: when option has sub-category and owner is an
                  ;; inline group (not the root), collect children names
                  (scope (when (and own-cat
                                    (clime-group-p owner)
                                    (clime-node-inline owner)
                                    (not (eq owner node)))
                           (mapcar #'car (clime-group-children owner)))))
             ;; Skip options with empty category path (uncategorized on
             ;; uncategorized inline group).  Top-level options (parent = node)
             ;; are always kept.
             (when (or full-path (eq owner node))
               (push (list full-path :option item scope) items))))
          (:command
           (let* ((path (clime-help--category-path (clime-node-parent item) node))
                  (own-cat (clime-node-category item))
                  (full-path (if own-cat (append path (list own-cat)) path)))
             (push (list full-path :command item nil) items))))))
    (nreverse items)))

(defun clime-help--option-row (opt)
  "Format OPT as a (left . help) table row."
  (cons (clime-help--format-option-flags opt)
        (clime-help--append-choices
         (clime-option-help opt)
         (clime-option-choices opt))))

(defun clime-help--indent-lines (str prefix)
  "Prepend PREFIX to each line of STR."
  (mapconcat (lambda (line) (concat prefix line))
             (split-string str "\n")
             "\n"))

(defun clime-help--command-row (cmd)
  "Format CMD as a (left . help) table row."
  (cons (clime-node-name cmd)
        (clime-help--first-line (or (clime-node-help cmd) ""))))

(defun clime-help--format-sections (items &optional width)
  "Format ITEMS into nested category sections.
ITEMS is a flat list of (CATEGORY-PATH TYPE ITEM SCOPE) from
`clime-help--collect-items'.  Groups by top-level category (first
path element), then by sub-category (remaining path).  Uncategorized
options use \"Options:\", uncategorized commands use \"Commands:\".
Sub-categories render as indented sub-headings with scope annotations
when SCOPE is present.
When WIDTH is non-nil, help text is wrapped to fit."
  (let ((top-groups (make-hash-table :test 'equal))
        (top-order '()))
    ;; ── Partition into two-level groups ──────────────────────────────
    ;; Each top-group is a hash: sub-key → (desc opts cmds scope)
    ;; sub-key nil = root items in that category.
    (dolist (entry items)
      (let* ((path (car entry))
             (type (cadr entry))
             (item (caddr entry))
             (scope (cadddr entry))
             (top-key (cond
                       ((and path (car path)) (car path))
                       ((eq type :option) :options)
                       ((eq type :command) :commands)
                       (t :commands)))
             (sub-key (when (and path (cdr path))
                        (string-join (cdr path) " / "))))
        ;; Ensure top-group exists
        (unless (gethash top-key top-groups)
          (push top-key top-order)
          ;; (sub-order . sub-hash)
          (puthash top-key (cons '() (make-hash-table :test 'equal))
                   top-groups))
        (let* ((tg (gethash top-key top-groups))
               (sub-hash (cdr tg)))
          ;; Ensure sub-group bucket exists
          (unless (gethash sub-key sub-hash)
            ;; nil (root items) always first in sub-order
            (if (null sub-key)
                (setcar tg (cons nil (car tg)))
              (setcar tg (append (car tg) (list sub-key))))
            ;; (desc opts cmds scope)
            (puthash sub-key (list nil nil nil nil) sub-hash))
          (let ((bucket (gethash sub-key sub-hash)))
            (pcase type
              (:group (unless (car bucket)
                        (setcar bucket (clime-help--first-line
                                        (clime-node-help item)))))
              (:option
               (push item (cadr bucket))
               (when (and scope (not (cadddr bucket)))
                 (setcar (cdddr bucket) scope)))
              (:command (push item (caddr bucket))))))))
    (setq top-order (nreverse top-order))
    ;; ── Render ───────────────────────────────────────────────────────
    (let ((sections '()))
      (dolist (top-key top-order)
        (let* ((tg (gethash top-key top-groups))
               (sub-order (car tg))
               (sub-hash (cdr tg))
               (header (cond
                        ((eq top-key :options) "Options:")
                        ((eq top-key :commands) "Commands:")
                        (t (format "%s:" top-key))))
               (parts (list header)))
          (dolist (sub-key sub-order)
            (let* ((bucket (gethash sub-key sub-hash))
                   (desc (car bucket))
                   (opts (nreverse (cadr bucket)))
                   (cmds (nreverse (caddr bucket)))
                   (scope (cadddr bucket))
                   (rows (append (mapcar #'clime-help--option-row opts)
                                 (mapcar #'clime-help--command-row cmds))))
              (if (null sub-key)
                  ;; Root items: standard indent
                  (progn
                    (when desc
                      (push (concat clime-help--indent desc "\n") parts))
                    (push (clime-help--format-table rows width) parts))
                ;; Sub-section: indented heading + double-indented table
                ;; Sub-tables are indented one extra level, reduce width
                (let* ((scope-str (if scope
                                      (format " (%s)" (string-join scope ", "))
                                    ""))
                       (sub-header (format "\n%s%s%s:"
                                           clime-help--indent sub-key scope-str))
                       (sub-width (when width
                                    (- width (length clime-help--indent))))
                       (table (clime-help--indent-lines
                               (clime-help--format-table rows sub-width)
                               clime-help--indent)))
                  (push sub-header parts)
                  (when desc
                    (push (concat clime-help--indent clime-help--indent desc)
                          parts))
                  (push table parts)))))
          (push (string-join (nreverse parts) "\n") sections)))
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
        (help-text (clime-node-help node))
        (width (clime-help--terminal-width)))
    ;; Description (wrapped)
    (when (and help-text (not (string-empty-p help-text)))
      (push (clime-help--wrap-text help-text width) sections))
    ;; Arguments
    (let ((args-section (clime-help--format-arguments (clime-node-args node) width)))
      (when args-section
        (push args-section sections)))
    ;; Options + Commands (unified sections with interleaved categories)
    (if (clime-branch-p node)
        (let ((unified (clime-help--format-sections
                        (clime-help--collect-items node) width)))
          (when unified
            (push unified sections)))
      ;; Leaf commands: options only, no children
      (let ((opts-section (clime-help--format-options
                           (clime-node-options node) width)))
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
                                       (clime-help--format-table rows width))))))
      (when global-section
        (push global-section sections)))
    ;; Epilog (wrapped)
    (let ((epilog (clime-node-epilog node)))
      (when (and epilog (not (string-empty-p epilog)))
        (push (clime-help--wrap-text epilog width) sections)))
    ;; Command help footer
    (when (and (clime-branch-p node) (clime-group-children node))
      (push (format "Run \"%s COMMAND --help\" for more information on a command."
                    (string-join path " "))
            sections))
    (concat (string-join (nreverse sections) "\n\n") "\n")))

(defun clime-format-version (app)
  "Return version string for APP."
  (format "%s %s\n" (clime-app-name app) (clime-app-version app)))

(provide 'clime-help)
;;; clime-help.el ends here
