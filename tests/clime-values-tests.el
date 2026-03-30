;;; clime-values-tests.el --- Tests for clime values-map API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'clime-core)

;;; ─── Accessors ─────────────────────────────────────────────────────────

(ert-deftest clime-test-values/get-present ()
  "clime-values-get returns (VALUE . SOURCE) for existing entry."
  (let ((values '((port . (8080 . user)))))
    (should (equal '(8080 . user) (clime-values-get values 'port)))))

(ert-deftest clime-test-values/get-absent ()
  "clime-values-get returns nil for absent entry."
  (should-not (clime-values-get '() 'port)))

(ert-deftest clime-test-values/value ()
  "clime-values-value returns the value component."
  (let ((values '((port . (8080 . user)))))
    (should (= 8080 (clime-values-value values 'port)))))

(ert-deftest clime-test-values/source ()
  "clime-values-source returns the source component."
  (let ((values '((port . (8080 . user)))))
    (should (eq 'user (clime-values-source values 'port)))))

(ert-deftest clime-test-values/value-absent ()
  "clime-values-value returns nil for absent entry."
  (should-not (clime-values-value '() 'port)))

;;; ─── Set (unconditional) ───────────────────────────────────────────────

(ert-deftest clime-test-values/set-new ()
  "clime-values-set inserts a new entry."
  (let ((result (clime-values-set '() 'port 8080 'user)))
    (should (= 8080 (clime-values-value result 'port)))
    (should (eq 'user (clime-values-source result 'port)))))

(ert-deftest clime-test-values/set-overwrites ()
  "clime-values-set unconditionally replaces existing entry."
  (let* ((values '((port . (8080 . user))))
         (result (clime-values-set values 'port 3000 'default)))
    (should (= 3000 (clime-values-value result 'port)))
    (should (eq 'default (clime-values-source result 'port)))))

(ert-deftest clime-test-values/set-preserves-others ()
  "clime-values-set does not disturb other entries."
  (let* ((values '((host . ("localhost" . user)) (port . (8080 . user))))
         (result (clime-values-set values 'port 3000 'env)))
    (should (equal "localhost" (clime-values-value result 'host)))
    (should (= 3000 (clime-values-value result 'port)))))

;;; ─── Merge (precedence-aware) ──────────────────────────────────────────

(ert-deftest clime-test-values/merge-insert ()
  "clime-values-merge inserts when name is absent."
  (let ((result (clime-values-merge '() 'port 8080 'user)))
    (should (= 8080 (clime-values-value result 'port)))))

(ert-deftest clime-test-values/merge-higher-wins ()
  "clime-values-merge overwrites when new source has higher precedence."
  (let* ((values '((port . (3000 . default))))
         (result (clime-values-merge values 'port 8080 'user)))
    (should (= 8080 (clime-values-value result 'port)))
    (should (eq 'user (clime-values-source result 'port)))))

(ert-deftest clime-test-values/merge-lower-loses ()
  "clime-values-merge keeps existing when new source has lower precedence."
  (let* ((values '((port . (8080 . user))))
         (result (clime-values-merge values 'port 3000 'default)))
    (should (= 8080 (clime-values-value result 'port)))
    (should (eq 'user (clime-values-source result 'port)))))

(ert-deftest clime-test-values/merge-same-keeps ()
  "clime-values-merge keeps existing when sources are equal."
  (let* ((values '((port . (8080 . user))))
         (result (clime-values-merge values 'port 3000 'user)))
    (should (= 8080 (clime-values-value result 'port)))))

(ert-deftest clime-test-values/merge-precedence-order ()
  "Source precedence: user > app > env > default > conform."
  (let ((values '()))
    ;; Insert with lowest precedence first
    (setq values (clime-values-merge values 'x 1 'conform))
    (should (= 1 (clime-values-value values 'x)))
    (setq values (clime-values-merge values 'x 2 'default))
    (should (= 2 (clime-values-value values 'x)))
    (setq values (clime-values-merge values 'x 3 'env))
    (should (= 3 (clime-values-value values 'x)))
    (setq values (clime-values-merge values 'x 4 'app))
    (should (= 4 (clime-values-value values 'x)))
    (setq values (clime-values-merge values 'x 5 'user))
    (should (= 5 (clime-values-value values 'x)))
    ;; Now nothing can override user
    (setq values (clime-values-merge values 'x 99 'env))
    (should (= 5 (clime-values-value values 'x)))))

;;; ─── Plist Derivation ──────────────────────────────────────────────────

(ert-deftest clime-test-values/plist-basic ()
  "clime-values-plist produces a flat NAME VALUE plist."
  (let* ((values '((port . (8080 . user)) (host . ("localhost" . env))))
         (plist (clime-values-plist values)))
    (should (= 8080 (plist-get plist 'port)))
    (should (equal "localhost" (plist-get plist 'host)))))

(ert-deftest clime-test-values/plist-empty ()
  "clime-values-plist on empty values returns nil."
  (should-not (clime-values-plist '())))

(provide 'clime-values-tests)
;;; clime-values-tests.el ends here
