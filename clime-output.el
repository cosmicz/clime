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
;; `clime-out--active-format' is always non-nil (defaults to text).
;; Handlers call `clime-out', `clime-out-error', and
;; `clime-out-emit' which dispatch through the active format.
;;
;; In buffered mode (streaming=nil), items and errors accumulate in
;; `clime-out--items' and `clime-out--errors'.  After the handler
;; returns, `clime-out--flush' passes both to the format's finalize
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

;;; ─── Legacy Aliases (must precede defvar of targets) ─────────────────

(defvaralias 'clime--active-output-format 'clime-out--active-format)
(defvaralias 'clime--output-items 'clime-out--items)
(defvaralias 'clime--output-errors 'clime-out--errors)

;;; ─── Dynamic State ────────────────────────────────────────────────────

(defvar clime-out--active-format clime-output-default-format
  "The active output format struct.
Always non-nil.  Bound by `clime-run' to the detected format or
the default text format.")

(defvar clime-out--items nil
  "Accumulated output items (buffered mode only).
Items are appended in emission order.")

(defvar clime-out--errors nil
  "Accumulated error strings (buffered mode only).
Errors are appended in chronological order.")

;;; ─── Active Format Query ──────────────────────────────────────────────

(defun clime-out-format ()
  "Return the name symbol of the active output format.
E.g. `text', `json', `yaml'.  Handlers use this to branch on
output format: (eq (clime-out-format) \\='json)."
  (clime-output-format-name clime-out--active-format))

;;; ─── Output Functions ─────────────────────────────────────────────────

(defun clime-out (data &rest keys)
  "Output DATA via the active format.
KEYS is an optional plist.  Recognized keys:
  :text STRING — in text mode, emit STRING (with newline) instead
                 of encoding DATA.  Ignored by structured formats.
Streaming: encode and princ immediately.
Buffered: append to `clime-out--items' (flushed later)."
  (let ((fmt clime-out--active-format))
    (if (clime-output-format-streaming fmt)
        (let ((text (plist-get keys :text)))
          (if (and text (eq (clime-output-format-name fmt) 'text))
              (princ (concat text "\n"))
            (princ (funcall (clime-output-format-encoder fmt) data))))
      (setq clime-out--items (nconc clime-out--items (list data)))))
  data)

(defun clime-out-text (text)
  "Emit TEXT with newline in text mode; no-op for structured formats.
Use for headers, labels, and decoration with no structured equivalent."
  (when (eq (clime-output-format-name clime-out--active-format) 'text)
    (princ (concat text "\n")))
  nil)

(defun clime-out-error (msg)
  "Report error MSG via the active format.
Streaming: dispatches to the format's error-handler (immediate).
Buffered: appends to `clime-out--errors' (handled at finalize)."
  (let ((fmt clime-out--active-format))
    (if (clime-output-format-streaming fmt)
        (funcall (clime-output-format-error-handler fmt) msg)
      (setq clime-out--errors (nconc clime-out--errors (list msg))))))

(defun clime-out-emit (data)
  "Emit DATA immediately, bypassing the accumulator.
Always encodes via the active format's encoder."
  (princ (funcall (clime-output-format-encoder clime-out--active-format) data))
  data)

;;; ─── Finalize ─────────────────────────────────────────────────────────

(defun clime-out--finalize-default (items retval errors)
  "Default finalize: wrap output for emission.
ITEMS is a list of accumulated `clime-out' items (may be nil).
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

(defun clime-out--flush (finalize retval)
  "Flush accumulated output using FINALIZE function.
FINALIZE receives (items retval errors) and returns data to encode.
Defaults to `clime-out--finalize-default'.
RETVAL is the handler's return value (passed through to finalize).
Reads items from `clime-out--items' and errors from `clime-out--errors'."
  (let* ((items clime-out--items)
         (errors clime-out--errors)
         (fn (or finalize #'clime-out--finalize-default))
         (data (funcall fn items retval errors)))
    (setq clime-out--items nil)
    (setq clime-out--errors nil)
    (when data
      (princ (funcall (clime-output-format-encoder clime-out--active-format) data)))))

;;; ─── Backward Compatibility ───────────────────────────────────────────

(defalias 'clime-output 'clime-out)
(defalias 'clime-output-error 'clime-out-error)
(defalias 'clime-output-stream 'clime-out-emit)
(defalias 'clime-output-text 'clime-out-text)
(defalias 'clime-output-name 'clime-out-format)
(make-obsolete 'clime-output 'clime-out "0.5.0")
(make-obsolete 'clime-output-error 'clime-out-error "0.5.0")
(make-obsolete 'clime-output-stream 'clime-out-emit "0.5.0")
(make-obsolete 'clime-output-text 'clime-out-text "0.5.0")
(make-obsolete 'clime-output-name 'clime-out-format "0.5.0")

(provide 'clime-output)
;;; clime-output.el ends here
