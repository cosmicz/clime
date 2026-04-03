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
(require 'clime-collision-tests)
(require 'clime-help-tests)
(require 'clime-output-tests)
(require 'clime-env-tests)
(require 'clime-stdin-tests)
(require 'clime-alias-for-tests)
(require 'clime-invoke-tests)
(require 'clime-deprecated-tests)
(require 'clime-negatable-tests)
(require 'clime-requires-tests)
(require 'clime-conform-tests)
(require 'clime-values-tests)
(require 'clime-param-type-tests)
(require 'clime-integration-tests)

;;; ─── Test Runner Functions ────────────────────────────────────────────

(defvar clime-test-verbose nil
  "Non-nil lets `message' output through to stderr during tests.
Set via CLIME_TEST_VERBOSE env var or VERBOSE make variable.")

(defun clime-run-tests (&optional selector)
  "Run clime tests interactively matching SELECTOR."
  (interactive)
  (clime-setup-test-environment)
  (ert-run-tests-interactively (or selector "^clime-test-")))

(defun clime-run-tests-batch (&optional selector)
  "Run clime tests in batch mode matching SELECTOR.
When `clime-test-verbose' is non-nil, test messages are printed
to stderr instead of being captured by ERT."
  (clime-setup-test-environment)
  (let ((env (getenv "CLIME_TEST_VERBOSE")))
    (when (and env (not (string-empty-p env)))
      (setq clime-test-verbose t)))
  (when clime-test-verbose
    ;; Redirect `message' to stderr so ERT can't swallow it
    (advice-add 'message :after
                (lambda (fmt &rest args)
                  (let ((text (apply #'format-message fmt args)))
                    (princ (concat text "\n") #'external-debugging-output)))
                '((name . clime-test-verbose))))
  (let ((selector-regexp (if (or (null selector) (string-empty-p selector))
                             "^clime-test-"
                           selector)))
    (ert-run-tests-batch-and-exit selector-regexp)))

(provide 'clime-tests-runner)
;;; clime-tests-runner.el ends here
