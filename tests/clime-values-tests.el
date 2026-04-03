;;; clime-values-tests.el --- Tests for clime values-map API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'clime-core)

;;; ─── Accessors ─────────────────────────────────────────────────────────

(ert-deftest clime-test-values/get-present ()
  "clime-values-get returns plist for existing entry."
  (let ((values '((port :value 8080 :source user))))
    (should (equal '(:value 8080 :source user) (clime-values-get values 'port)))))

(ert-deftest clime-test-values/get-absent ()
  "clime-values-get returns nil for absent entry."
  (should-not (clime-values-get '() 'port)))

(ert-deftest clime-test-values/value ()
  "clime-values-value returns the value component."
  (let ((values '((port :value 8080 :source user))))
    (should (= 8080 (clime-values-value values 'port)))))

(ert-deftest clime-test-values/source ()
  "clime-values-source returns the source component."
  (let ((values '((port :value 8080 :source user))))
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
  (let* ((values '((port :value 8080 :source user)))
         (result (clime-values-set values 'port 3000 'default)))
    (should (= 3000 (clime-values-value result 'port)))
    (should (eq 'default (clime-values-source result 'port)))))

(ert-deftest clime-test-values/set-preserves-others ()
  "clime-values-set does not disturb other entries."
  (let* ((values '((host :value "localhost" :source user) (port :value 8080 :source user)))
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
  (let* ((values '((port :value 3000 :source default)))
         (result (clime-values-merge values 'port 8080 'user)))
    (should (= 8080 (clime-values-value result 'port)))
    (should (eq 'user (clime-values-source result 'port)))))

(ert-deftest clime-test-values/merge-lower-loses ()
  "clime-values-merge keeps existing when new source has lower precedence."
  (let* ((values '((port :value 8080 :source user)))
         (result (clime-values-merge values 'port 3000 'default)))
    (should (= 8080 (clime-values-value result 'port)))
    (should (eq 'user (clime-values-source result 'port)))))

(ert-deftest clime-test-values/merge-same-keeps ()
  "clime-values-merge keeps existing when sources are equal."
  (let* ((values '((port :value 8080 :source user)))
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
  (let* ((values '((port :value 8080 :source user) (host :value "localhost" :source env)))
         (plist (clime-values-plist values)))
    (should (= 8080 (plist-get plist 'port)))
    (should (equal "localhost" (plist-get plist 'host)))))

(ert-deftest clime-test-values/plist-empty ()
  "clime-values-plist on empty values returns nil."
  (should-not (clime-values-plist '())))

;;; ─── Plist Entry Shape (spec: clime-33d2) ────────────────────────────
;;
;; These tests define the target entry shape: (NAME . (:value V :source S))
;; with optional :error key.  They will fail until the migration is complete.

(ert-deftest clime-test-values/plist-entry-shape ()
  "Values map entries use plist shape (:value V :source S)."
  (let ((values (clime-values-set '() 'port 8080 'user)))
    (should (equal '(:value 8080 :source user)
                   (clime-values-get values 'port)))))

(ert-deftest clime-test-values/set-error ()
  "clime-values-set-error writes an :error key to a values entry."
  (let* ((values (clime-values-set '() 'port 8080 'user))
         (values (clime-values-set-error values 'port "must be 1-65535")))
    (should (equal "must be 1-65535" (clime-values-error values 'port)))
    ;; Original value and source preserved
    (should (= 8080 (clime-values-value values 'port)))
    (should (eq 'user (clime-values-source values 'port)))))

(ert-deftest clime-test-values/set-error-no-prior-value ()
  "clime-values-set-error on absent entry creates error-only entry."
  (let ((values (clime-values-set-error '() 'port "required")))
    (should (equal "required" (clime-values-error values 'port)))
    (should-not (plist-member (clime-values-get values 'port) :value))))

(ert-deftest clime-test-values/errors-collects-all ()
  "clime-values-errors returns all entries with :error key."
  (let* ((values (clime-values-set '() 'port 8080 'user))
         (values (clime-values-set values 'host "localhost" 'user))
         (values (clime-values-set-error values 'port "must be positive"))
         (values (clime-values-set-error values 'host "cannot be localhost"))
         (errors (clime-values-errors values)))
    (should (= 2 (length errors)))
    (should (assq 'port errors))
    (should (assq 'host errors))))

(ert-deftest clime-test-values/plist-skips-error-only ()
  "clime-values-plist excludes entries that have :error but no :value."
  (let* ((values (clime-values-set '() 'port 8080 'user))
         (values (clime-values-set-error '() 'bad "no value for this"))
         (values (clime-values-set values 'port 8080 'user))
         (plist (clime-values-plist values)))
    (should (= 8080 (plist-get plist 'port)))
    (should-not (plist-member plist 'bad))))

(ert-deftest clime-test-values/plist-includes-value-with-error ()
  "clime-values-plist includes entries that have both :value and :error."
  (let* ((values (clime-values-set '() 'port 8080 'user))
         (values (clime-values-set-error values 'port "warning: non-standard"))
         (plist (clime-values-plist values)))
    (should (= 8080 (plist-get plist 'port)))))

;;; ─── Error Accumulation (spec: clime-33d2) ──────────────────────────

(ert-deftest clime-test-values/conform-errors-accumulate ()
  "Multiple per-param conformer errors accumulate in values map."
  (let* ((opt-port (clime-make-option :name 'port :flags '("--port")
                                       :type 'number
                                       :conform (lambda (v _p)
                                                  (when (< v 0)
                                                    (error "must be positive"))
                                                  v)))
         (opt-count (clime-make-option :name 'count :flags '("--count")
                                        :type 'number
                                        :conform (lambda (v _p)
                                                   (when (< v 1)
                                                     (error "must be >= 1"))
                                                   v)))
         (cmd (clime-make-command :name "test" :handler #'ignore
                                   :options (list opt-port opt-count)))
         (values (clime-values-set '() 'port -5 'user))
         (values (clime-values-set values 'count 0 'user))
         (result (clime--run-conformers (list cmd) values)))
    ;; Both errors accumulated, not just the first one
    (should (clime-values-error result 'port))
    (should (clime-values-error result 'count))))

(ert-deftest clime-test-values/conform-errors-signal-compound ()
  "Parse signals compound error when multiple conformers fail."
  (let* ((app (clime-make-app
               :name "test"
               :handler #'ignore
               :options (list (clime-make-option :name 'port :flags '("--port")
                                                 :type 'number
                                                 :conform (lambda (v _p)
                                                            (when (< v 0)
                                                              (error "must be positive"))
                                                            v))
                              (clime-make-option :name 'count :flags '("--count")
                                                 :type 'number
                                                 :conform (lambda (v _p)
                                                            (when (< v 1)
                                                              (error "must be >= 1"))
                                                            v))))))
    ;; Signal contains both error messages
    (condition-case err
        (progn (clime-parse app '("--port" "-5" "--count" "0"))
               (ert-fail "Expected clime-usage-error"))
      (clime-usage-error
       (let ((msg (cadr err)))
         (should (string-match-p "must be positive" msg))
         (should (string-match-p "must be >= 1" msg)))))))

(ert-deftest clime-test-values/node-conform-error-in-map ()
  "Node conformer error with :params attribution writes to values map."
  (let* ((cmd (clime-make-command
               :name "test" :handler #'ignore
               :options (list (clime-make-option :name 'min :flags '("--min") :type 'number)
                              (clime-make-option :name 'max :flags '("--max") :type 'number))
               :conform (lambda (values _node)
                          (when (and (clime-values-value values 'min)
                                     (clime-values-value values 'max)
                                     (> (clime-values-value values 'min)
                                        (clime-values-value values 'max)))
                            (signal 'clime-usage-error
                                    '("min must be <= max" :params (min max))))
                          values)))
         (values (clime-values-set '() 'min 10 'user))
         (values (clime-values-set values 'max 5 'user))
         (result (clime--apply-node-conform cmd values)))
    (should (clime-values-error result 'min))
    (should (clime-values-error result 'max))))

(provide 'clime-values-tests)
;;; clime-values-tests.el ends here
