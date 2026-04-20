;;; clime-config.el --- Config file providers for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Config provider creators for loading option defaults from files.
;; A provider is a function (COMMAND-PATH PARAM-NAME) → VALUE | nil.
;; Provider creators read a file and return a closure that looks up
;; values by path.

;;; Code:

(require 'json)

;;; ─── JSON provider ─────────────────────────────────────────────────────

(defun clime-config-json (file)
  "Create a config provider from JSON FILE.
Returns a function (COMMAND-PATH PARAM-NAME) that walks nested objects
with hierarchical inheritance: tries the most specific path first, then
walks up to the root.

Example: for COMMAND-PATH=(\"server\" \"start\") and PARAM-NAME=\"port\",
tries obj[\"server\"][\"start\"][\"port\"], then obj[\"server\"][\"port\"],
then obj[\"port\"].

Returns nil if FILE does not exist."
  (when (file-exists-p file)
    (let ((data (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (json-read))))
      (lambda (command-path param-name)
        (clime-config--json-lookup data command-path param-name)))))

(defun clime-config--json-lookup (data command-path param-name)
  "Look up PARAM-NAME in DATA walking COMMAND-PATH with inheritance.
Tries most-specific path first, then progressively shorter prefixes."
  (let ((paths (clime-config--inheritance-paths command-path))
        (result nil)
        (found nil))
    (while (and paths (not found))
      (let ((val (clime-config--walk-json data (car paths) param-name)))
        (when val
          (setq result (clime-config--json-to-elisp val)
                found t)))
      (setq paths (cdr paths)))
    result))

(defun clime-config--walk-json (data path param-name)
  "Walk DATA through PATH segments, then look up PARAM-NAME.
Return the raw JSON value or nil if any step fails."
  (let ((obj data))
    (dolist (seg path)
      (if (and obj (listp obj))
          (let ((entry (assoc (intern seg) obj)))
            (setq obj (and entry (cdr entry))))
        (setq obj nil)))
    (when (and obj (listp obj))
      (let ((entry (assoc (intern param-name) obj)))
        (and entry (cdr entry))))))

(defun clime-config--json-to-elisp (val)
  "Convert JSON value VAL to Elisp.
json.el returns :json-false for false, nil for null, t for true,
vectors for arrays.  Numbers and strings pass through unchanged.
Returns nil for both false and null (treated as not-configured)."
  (cond
   ((eq val :json-false) nil)
   ((eq val t) t)
   ((vectorp val) (append val nil))
   (t val)))

;;; ─── Sexp provider ─────────────────────────────────────────────────────

(defun clime-config-sexp (file)
  "Create a config provider from a sexp FILE (nested plist).
Returns a function (COMMAND-PATH PARAM-NAME) that walks nested plists
with hierarchical inheritance, same as the JSON provider.

The file should contain a single plist form:
  (:serve (:port 8080 :host \"localhost\") :port 3000)

Returns nil if FILE does not exist."
  (when (file-exists-p file)
    (let ((data (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (read (current-buffer)))))
      (lambda (command-path param-name)
        (clime-config--sexp-lookup data command-path param-name)))))

(defun clime-config--sexp-lookup (data command-path param-name)
  "Look up PARAM-NAME in plist DATA walking COMMAND-PATH with inheritance."
  (let ((paths (clime-config--inheritance-paths command-path))
        (result nil)
        (found nil))
    (while (and paths (not found))
      (let ((val (clime-config--walk-sexp data (car paths) param-name)))
        (when val
          (setq result (car val)
                found t)))
      (setq paths (cdr paths)))
    result))

(defun clime-config--walk-sexp (data path param-name)
  "Walk plist DATA through PATH segments, then look up PARAM-NAME.
Return (VALUE) as a one-element list if found, nil otherwise.
The wrapping distinguishes nil-value from not-found."
  (let ((obj data))
    (dolist (seg path)
      (if (plistp obj)
          (let ((child (plist-get obj (intern (concat ":" seg)))))
            (setq obj child))
        (setq obj nil)))
    (when (plistp obj)
      (let ((key (intern (concat ":" param-name))))
        (when (plist-member obj key)
          (list (plist-get obj key)))))))

;;; ─── Shared utilities ──────────────────────────────────────────────────

(defun clime-config--inheritance-paths (command-path)
  "Return list of paths to try, most-specific first.
For COMMAND-PATH=(\"server\" \"start\"), returns:
  ((\"server\" \"start\") (\"server\") nil)"
  (let ((paths (list command-path)))
    (let ((path (copy-sequence command-path)))
      (while path
        (setq path (butlast path))
        (push path paths)))
    (nreverse paths)))

(provide 'clime-config)
;;; clime-config.el ends here
