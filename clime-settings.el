;;; clime-settings.el --- Configuration for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; User-facing configuration options for clime.

;;; Code:

(defgroup clime nil
  "Brief description of clime."
  :group 'tools
  :prefix "clime-")

(defcustom clime-log-buffer "*Clime Log*"
  "Buffer name for clime logging output.
Set to nil to disable logging."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "Disabled" nil))
  :group 'clime)

(provide 'clime-settings)
;;; clime-settings.el ends here
