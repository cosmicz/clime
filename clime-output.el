;;; clime-output.el --- Output protocol for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dual text/JSON output protocol.  Handlers call clime-output-* helpers
;; which dispatch to plain text or JSON based on the current mode.
;;
;; JSON encoding conventions:
;;   nil     → JSON null
;;   :null   → JSON null
;;   :json-false → JSON false
;;   vectors → JSON arrays (use [] for empty array, NOT nil)
;;   plists/alists → JSON objects
;;
;; Multiple output calls emit NDJSON (one JSON object per line).
;; For a single clean JSON response, use the callback return protocol.

;;; Code:

(require 'json)
(require 'clime-core)

;;; ─── Mode Variable ────────────────────────────────────────────────────

(defvar clime--json-mode-p nil
  "Non-nil when --json output mode is active.
Bound dynamically by `clime-run' based on pre-parse --json detection.")

;;; ─── JSON Encoding ────────────────────────────────────────────────────

(defun clime-json-encode (data)
  "Encode DATA as a JSON string.
Thin wrapper around `json-encode'.  See commentary for encoding
conventions (nil→null, vectors→arrays, :json-false→false)."
  (json-encode data))

;;; ─── Pre-parse Helpers ────────────────────────────────────────────────

(defun clime--pre-parse-json-p (argv)
  "Return non-nil if ARGV contains \"--json\"."
  (member "--json" argv))

(defun clime--ensure-json-option (app)
  "Add a --json boolean option to APP if :json-mode and not already present.
Modifies APP in place.  Idempotent."
  (when (and (clime-app-json-mode app)
             (not (clime-node-find-option app "--json")))
    (let ((opt (clime-make-option :name 'json
                                  :flags '("--json")
                                  :nargs 0
                                  :help "Output as JSON"
                                  :group "Output")))
      (setf (clime-group-options app)
            (append (clime-group-options app) (list opt))))))

;;; ─── Output Helpers ───────────────────────────────────────────────────

(defun clime-output (data)
  "Output DATA in the current mode.
JSON mode: print JSON-encoded data + newline (NDJSON).
Text mode: print data as string."
  (if clime--json-mode-p
      (princ (concat (clime-json-encode data) "\n"))
    (princ (format "%s" data)))
  data)

(defun clime-output-success (data)
  "Output DATA as a success result.
JSON mode: {\"success\": true, \"data\": DATA} + newline.
Text mode: print data as-is."
  (if clime--json-mode-p
      (princ (concat (clime-json-encode `((success . t) (data . ,data))) "\n"))
    (princ (format "%s" data)))
  data)

(defun clime-output-error (msg)
  "Output MSG as an error.
JSON mode: {\"error\": MSG} + newline to stdout.
Text mode: \"Error: MSG\" to stderr."
  (if clime--json-mode-p
      (princ (concat (clime-json-encode `((error . ,msg))) "\n"))
    (message "Error: %s" msg)))

(defun clime-output-list (items)
  "Output ITEMS as a list result.
JSON mode: {\"success\": true, \"data\": [...]} + newline.
Text mode: print each item on its own line."
  (if clime--json-mode-p
      (let ((arr (if (vectorp items) items (vconcat items))))
        (princ (concat (clime-json-encode `((success . t) (data . ,arr))) "\n")))
    (dolist (item items)
      (princ (format "%s\n" item))))
  items)

(provide 'clime-output)
;;; clime-output.el ends here
