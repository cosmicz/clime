;;; clime-output.el --- Output protocol for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dual text/JSON output protocol.  Handlers call clime-output-* helpers
;; which dispatch to plain text or JSON based on the current mode.
;;
;; JSON encoding conventions:
;;   nil     → JSON null
;;   :json-false → JSON false
;;   vectors → JSON arrays (use [] for empty array, NOT nil)
;;   plists/alists → JSON objects
;;
;; In JSON mode, `clime-run' binds `clime--output-buffer' so that
;; `clime-output' accumulates items.  After the handler returns,
;; `clime--output-flush' emits the result: bare object (1 item),
;; JSON array (2+ items), or nothing (0 items).
;;
;; Without the buffer binding (standalone usage), `clime-output' emits
;; NDJSON immediately for backward compatibility.
;;
;; For explicit NDJSON streaming, use `clime-output-stream' which
;; always bypasses the accumulator.

;;; Code:

(require 'json)
(require 'clime-core)

;;; ─── Mode Variable ────────────────────────────────────────────────────

(defvar clime-output-mode 'text
  "Current output mode symbol.
Bound dynamically by `clime-run' based on pre-parse detection.
Known values: `text', `json'.")

(defun clime-output-mode-json-p ()
  "Return non-nil when output mode is JSON."
  (eq clime-output-mode 'json))

;;; ─── Accumulator ────────────────────────────────────────────────────

(defvar clime--output-buffer nil
  "Accumulator for JSON output items.
When bound to a non-nil value by `clime-run', `clime-output' pushes
items here instead of printing immediately.  Initialize to \\='(:buffer)
as a sentinel; items are pushed onto the front.  `clime--output-flush'
strips the sentinel before emitting.  nil means not buffering.")

(defun clime--output-flush ()
  "Emit accumulated JSON output and clear the buffer.
0 items: no output.  1 item: bare JSON object.  2+ items: JSON array.
Strips the `:buffer' sentinel before processing."
  (when (and clime--output-buffer (clime-output-mode-json-p))
    ;; Strip :buffer sentinel, reverse to emission order
    (let ((items (nreverse (remq :buffer clime--output-buffer))))
      (setq clime--output-buffer nil)
      (when items
        (princ (concat (clime-json-encode
                        (if (cdr items)
                            (vconcat items)
                          (car items)))
                       "\n"))))))

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

;;; ─── Output Helpers ───────────────────────────────────────────────────

(defun clime-output (data)
  "Output DATA in the current mode.
JSON mode with accumulator: push to buffer (flushed later).
JSON mode without accumulator: emit NDJSON immediately.
Text mode: print data as string."
  (if (clime-output-mode-json-p)
      (if clime--output-buffer
          (push data clime--output-buffer)
        (princ (concat (clime-json-encode data) "\n")))
    (princ (format "%s" data)))
  data)

(defun clime-output-success (data)
  "Output DATA as a success result.
JSON mode: {\"success\": true, \"data\": DATA}.
Text mode: print data as-is."
  (let ((envelope `((success . t) (data . ,data))))
    (if (clime-output-mode-json-p)
        (if clime--output-buffer
            (push envelope clime--output-buffer)
          (princ (concat (clime-json-encode envelope) "\n")))
      (princ (format "%s" data))))
  data)

(defun clime-output-error (msg)
  "Output MSG as an error.
JSON mode: {\"error\": MSG} + newline to stdout (always immediate).
Text mode: \"Error: MSG\" to stderr."
  (if (clime-output-mode-json-p)
      (princ (concat (clime-json-encode `((error . ,msg))) "\n"))
    (message "Error: %s" msg)))

(defun clime-output-list (items)
  "Output ITEMS as a list result.
JSON mode: {\"success\": true, \"data\": [...]}.
Text mode: print each item on its own line."
  (let ((arr (if (vectorp items) items (vconcat items))))
    (if (clime-output-mode-json-p)
        (let ((envelope `((success . t) (data . ,arr))))
          (if clime--output-buffer
              (push envelope clime--output-buffer)
            (princ (concat (clime-json-encode envelope) "\n"))))
      (dolist (item items)
        (princ (format "%s\n" item)))))
  items)

(defun clime-output-stream (data)
  "Emit DATA as NDJSON immediately, bypassing the accumulator.
Use this when streaming output is desired (one JSON object per line)."
  (princ (concat (clime-json-encode data) "\n"))
  data)

(provide 'clime-output)
;;; clime-output.el ends here
