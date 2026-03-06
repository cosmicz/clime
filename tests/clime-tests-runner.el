;;; clime-tests-runner.el --- Test runner for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test runner and utilities for clime tests.

;;; Code:

(require 'ert)
(require 'clime)

;;; ─── Test Environment Setup ───────────────────────────────────────────

(defun clime-setup-test-environment ()
  "Setup the test environment for clime.
Configure project-specific settings for clean test execution."
  (setq max-lisp-eval-depth 1000
        print-level 1000
        print-length 1000)
  (message "clime test environment setup complete."))

;;; ─── Load Test Files ──────────────────────────────────────────────────

(require 'clime-test-helpers)
(require 'clime-core-tests)
(require 'clime-parse-tests)
(require 'clime-dsl-tests)
(require 'clime-run-tests)

;;; ─── Test Runner Functions ────────────────────────────────────────────

(defun clime-run-tests (&optional selector)
  "Run clime tests interactively matching SELECTOR."
  (interactive)
  (clime-setup-test-environment)
  (ert-run-tests-interactively (or selector "^clime-test-")))

(defun clime-run-tests-batch (&optional selector)
  "Run clime tests in batch mode matching SELECTOR."
  (clime-setup-test-environment)
  (let ((selector-regexp (if (or (null selector) (string-empty-p selector))
                             "^clime-test-"
                           selector)))
    (ert-run-tests-batch-and-exit selector-regexp)))

(provide 'clime-tests-runner)
;;; clime-tests-runner.el ends here
