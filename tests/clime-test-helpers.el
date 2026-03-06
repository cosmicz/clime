;;; clime-test-helpers.el --- Test utilities for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared test utilities, macros, and helpers.

;;; Code:

(require 'cl-lib)

;;; ─── Message Capture ──────────────────────────────────────────────────

(defvar clime-test--message-log 'inactive
  "When a list, captures messages during tests.
Set to `inactive' to disable capturing.")

(defun clime-test--capture-message (fn fmt &rest args)
  "Advice around `message' to capture output during tests.
Calls FN with FMT and ARGS while recording the formatted message."
  (when (and fmt (listp clime-test--message-log))
    (push (apply #'format-message fmt args) clime-test--message-log))
  (apply fn fmt args))

(advice-add 'message :around #'clime-test--capture-message)

;;; ─── Temp Directory ───────────────────────────────────────────────────

(defmacro clime-test-with-temp-dir (&rest body)
  "Execute BODY with a temporary directory as `default-directory'."
  (declare (indent 0) (debug t))
  `(let ((temp-dir (make-temp-file "clime-test-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory temp-dir)))
           ,@body)
       (delete-directory temp-dir t))))

;;; ─── Message Log ──────────────────────────────────────────────────────

(defmacro clime-test-with-messages (&rest body)
  "Execute BODY while capturing messages.
Bind `clime-test--message-log' to a list during execution."
  (declare (indent 0) (debug t))
  `(let ((clime-test--message-log nil))
     ,@body
     (nreverse clime-test--message-log)))

(provide 'clime-test-helpers)
;;; clime-test-helpers.el ends here
