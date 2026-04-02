;;; clime.el --- Declarative CLI framework for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>
;; URL: https://github.com/cosmicz/clime
;; Keywords: tools, processes
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.3.0

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;;; Commentary:

;; Entry point for clime.
;;
;; This file provides autoloads and top-level require for the package.

;;; Code:

(require 'clime-settings)
(require 'clime-core)
(require 'clime-param-type)
(require 'clime-parse)
(require 'clime-dsl)
(require 'clime-help)
(require 'clime-output)
(require 'clime-run)

(defconst clime-version "0.6.0"
  "The clime package version string.")

(defconst clime--modules
  '(clime-settings clime-core clime-param-type clime-parse clime-dsl
                   clime-help clime-output clime-run)
  "Clime modules in dependency order.")

(defconst clime--optional-modules
  '(clime-invoke clime-make)
  "Optional clime modules, reloaded only if already loaded.")

;;;###autoload
(defun clime-reload ()
  "Force-reload all clime modules and invalidate caches.
Use this during development to pick up changes without restarting Emacs.
Reloads modules in dependency order to avoid stale definitions."
  (interactive)
  (let ((dir (file-name-directory (or load-file-name
                                      (locate-library "clime")
                                      buffer-file-name
                                      default-directory))))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))
    (dolist (mod clime--modules)
      (let ((file (locate-library (symbol-name mod))))
        (when file
          (load file nil t t))))
    ;; Reload clime.el itself (picks up changes to reload logic, module lists)
    (let ((self (locate-library "clime")))
      (when self (load self nil t t)))
    ;; Clear face-defface-spec before reload so defface applies new specs
    (dolist (face (apropos-internal "\\`clime-" #'facep))
      (put face 'face-defface-spec nil))
    ;; Reload optional clime modules that were previously loaded
    (dolist (mod clime--optional-modules)
      (when (featurep mod)
        (let ((file (locate-library (symbol-name mod))))
          (when file
            (load file nil t t)))))
    ;; Mark invoke registry as stale so cached apps get refreshed
    (when (boundp 'clime-invoke--stale)
      (setq clime-invoke--stale t)))
  (message "Reloaded clime modules"))

(provide 'clime)
;;; clime.el ends here
