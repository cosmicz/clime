;;; clime-output.el --- Output protocol for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Format-driven output protocol.  Every output mode is a
;; `clime-output-format' — including the default text mode.  The active
;; format struct encapsulates all output behavior: encoding, error
;; handling, accumulation, finalization.
;;
;; `clime--active-output-format' is always non-nil (defaults to text).
;; Handlers call `clime-output', `clime-output-error', and
;; `clime-output-stream' which dispatch through the active format.
;;
;; In buffered mode (streaming=nil), items and errors accumulate in
;; `clime--output-items' and `clime--output-errors'.  After the handler
;; returns, `clime--output-flush' passes both to the format's finalize
;; function which controls the output envelope.
;;
;; JSON encoding conventions:
;;   nil     → JSON null
;;   :json-false → JSON false
;;   vectors → JSON arrays (use [] for empty array, NOT nil)
;;   plists/alists → JSON objects

;;; Code:

(require 'json)
(require 'clime-core)

;;; ─── JSON Encoding ────────────────────────────────────────────────────

(defun clime-json-encode (data)
  "Encode DATA as a JSON string.
Thin wrapper around `json-encode'.  See commentary for encoding
conventions (nil→null, vectors→arrays, :json-false→false)."
  (json-encode data))

;;; ─── Default Format ───────────────────────────────────────────────────

(defconst clime-output-default-format
  (clime-make-output-format
   :name 'text
   :flags '("--text")
   :streaming t
   :encoder (lambda (data) (format "%s" data))
   :error-handler (lambda (msg) (message "Error: %s" msg)))
  "Default text output format.  Always-streaming, princ-based.
Not a CLI option — just a behavior container for text mode.")

;;; ─── Dynamic State ────────────────────────────────────────────────────

(defvar clime--active-output-format clime-output-default-format
  "The active output format struct.
Always non-nil.  Bound by `clime-run' to the detected format or
the default text format.")

(defvar clime--output-items nil
  "Accumulated output items (buffered mode only).
Items are appended in emission order.")

(defvar clime--output-errors nil
  "Accumulated error strings (buffered mode only).
Errors are appended in chronological order.")

;;; ─── Predicate ────────────────────────────────────────────────────────

(defun clime-output-mode-json-p ()
  "Return non-nil when the active output format is JSON."
  (eq (clime-output-format-name clime--active-output-format) 'json))

;;; ─── Output Functions ─────────────────────────────────────────────────

(defun clime-output (data)
  "Output DATA via the active format.
Streaming: encode and princ immediately.
Buffered: append to `clime--output-items' (flushed later)."
  (let ((fmt clime--active-output-format))
    (if (clime-output-format-streaming fmt)
        (princ (funcall (clime-output-format-encoder fmt) data))
      (setq clime--output-items (nconc clime--output-items (list data)))))
  data)

(defun clime-output-error (msg)
  "Report error MSG via the active format.
Streaming: dispatches to the format's error-handler (immediate).
Buffered: appends to `clime--output-errors' (handled at finalize)."
  (let ((fmt clime--active-output-format))
    (if (clime-output-format-streaming fmt)
        (funcall (clime-output-format-error-handler fmt) msg)
      (setq clime--output-errors (nconc clime--output-errors (list msg))))))

(defun clime-output-stream (data)
  "Emit DATA immediately, bypassing the accumulator.
Always encodes via the active format's encoder."
  (princ (funcall (clime-output-format-encoder clime--active-output-format) data))
  data)

;;; ─── Finalize ─────────────────────────────────────────────────────────

(defun clime--output-finalize-default (items retval errors)
  "Default finalize: wrap output for emission.
ITEMS is a list of accumulated `clime-output' items (may be nil).
RETVAL is the handler's return value.
ERRORS is a list of error strings (may be nil).
Returns data to encode, or nil for no output.

Priority: errors > items > retval.
- errors non-nil → ((error . first-msg))
- items 2+       → [item1, item2, ...]
- items 1        → bare item
- retval non-nil → ((success . t) (data . retval))
- all nil        → nil"
  (cond
   (errors
    `((error . ,(car errors))))
   (items
    (if (cdr items) (vconcat items) (car items)))
   (retval
    `((success . t) (data . ,retval)))))

(defun clime--output-flush (finalize retval)
  "Flush accumulated output using FINALIZE function.
FINALIZE receives (items retval errors) and returns data to encode.
Defaults to `clime--output-finalize-default'.
RETVAL is the handler's return value (passed through to finalize).
Reads items from `clime--output-items' and errors from `clime--output-errors'."
  (let* ((items clime--output-items)
         (errors clime--output-errors)
         (fn (or finalize #'clime--output-finalize-default))
         (data (funcall fn items retval errors)))
    (setq clime--output-items nil)
    (setq clime--output-errors nil)
    (when data
      (princ (funcall (clime-output-format-encoder clime--active-output-format) data)))))

(provide 'clime-output)
;;; clime-output.el ends here
