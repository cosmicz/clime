;;; clime-core-tests.el --- Tests for clime-core  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for core domain logic.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-test-helpers)

;;; ─── Struct Tests ─────────────────────────────────────────────────────

(ert-deftest clime-test-make-thing ()
  "Test creation of a thing struct."
  (let ((thing (clime-make-thing "test" 42)))
    (should (clime-thing-p thing))
    (should (equal (clime-thing-name thing) "test"))
    (should (= (clime-thing-value thing) 42))))

(ert-deftest clime-test-make-thing-no-value ()
  "Test creation of a thing with no value."
  (let ((thing (clime-make-thing "empty")))
    (should (clime-thing-p thing))
    (should (equal (clime-thing-name thing) "empty"))
    (should (null (clime-thing-value thing)))))

;;; ─── Core Operation Tests ─────────────────────────────────────────────

(ert-deftest clime-test-process ()
  "Test processing a thing."
  (let ((thing (clime-make-thing "hello" 99)))
    (should (equal (clime-process thing) "hello"))))

(provide 'clime-core-tests)
;;; clime-core-tests.el ends here
