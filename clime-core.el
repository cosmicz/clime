;;; clime-core.el --- Core domain logic for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core data structures and domain logic.

;;; Code:

(require 'cl-lib)

;;; ─── Data Structures ──────────────────────────────────────────────────

(cl-defstruct (clime-thing (:constructor clime-thing--create)
                              (:copier nil))
  "A thing in the clime domain."
  (name nil :type string :read-only t)
  (value nil :read-only t))

;;; ─── Constructors ─────────────────────────────────────────────────────

(defun clime-make-thing (name &optional value)
  "Create a new thing with NAME and optional VALUE."
  (clime-thing--create :name name :value value))

;;; ─── Core Operations ──────────────────────────────────────────────────

(defun clime-process (thing)
  "Process THING and return the result."
  (clime-thing-name thing))

(provide 'clime-core)
;;; clime-core.el ends here
