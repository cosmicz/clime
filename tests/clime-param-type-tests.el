;;; clime-param-type-tests.el --- Tests for type registry + resolution -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for clime-54z3.p7wo: type registry, clime-resolve-type, clime-deftype.

;;; Code:

(require 'ert)
(require 'clime)

;;; ─── Registry ──────────────────────────────────────────────────────────

(ert-deftest clime-test-types/registry-exists ()
  "Registry is a hash-table with eq test."
  (should (hash-table-p clime-type-registry))
  (should (eq 'eq (hash-table-test clime-type-registry))))

(ert-deftest clime-test-types/register-type-stores-constructor ()
  "clime-register-type stores a function retrievable by name."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'test-ty (lambda () (list :parse #'identity :describe "test")))
    (should (functionp (gethash 'test-ty clime-type-registry)))))

(ert-deftest clime-test-types/register-type-overwrites ()
  "Re-registering the same name silently overwrites."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'test-ty (lambda () (list :parse #'identity :describe "v1")))
    (clime-register-type 'test-ty (lambda () (list :parse #'identity :describe "v2")))
    (let ((plist (funcall (gethash 'test-ty clime-type-registry))))
      (should (equal "v2" (plist-get plist :describe))))))

(ert-deftest clime-test-types/register-type-rejects-bad-name ()
  "clime-register-type signals error for non-symbol name."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (should-error (clime-register-type "not-a-symbol" #'ignore))))

(ert-deftest clime-test-types/register-type-rejects-bad-constructor ()
  "clime-register-type signals error for non-function constructor."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (should-error (clime-register-type 'test-ty "not-a-function"))))

;;; ─── Resolution ────────────────────────────────────────────────────────

(ert-deftest clime-test-types/resolve-nil ()
  "nil resolves to string passthrough."
  (let ((plist (clime-resolve-type nil)))
    (should (equal "string" (plist-get plist :describe)))
    (should (equal "hello" (funcall (plist-get plist :parse) "hello")))))

(ert-deftest clime-test-types/resolve-symbol ()
  "Bare symbol looks up in registry and calls constructor with no args."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'my-type
                         (lambda () (list :parse #'string-to-number :describe "my-type")))
    (let ((plist (clime-resolve-type 'my-type)))
      (should (equal "my-type" (plist-get plist :describe)))
      (should (= 42 (funcall (plist-get plist :parse) "42"))))))

(ert-deftest clime-test-types/resolve-list ()
  "List spec calls constructor with cdr as args."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'bounded
                         (lambda (min max)
                           (list :parse (lambda (s)
                                          (let ((n (string-to-number s)))
                                            (when (or (< n min) (> n max))
                                              (error "Out of range"))
                                            n))
                                 :describe (format "integer (%d–%d)" min max))))
    (let ((plist (clime-resolve-type '(bounded 1 100))))
      (should (equal "integer (1–100)" (plist-get plist :describe)))
      (should (= 50 (funcall (plist-get plist :parse) "50")))
      (should-error (funcall (plist-get plist :parse) "200")))))

(ert-deftest clime-test-types/resolve-unknown-symbol ()
  "Unknown symbol signals clime-type-error."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (should-error (clime-resolve-type 'nonexistent)
                  :type 'clime-type-error)))

(ert-deftest clime-test-types/resolve-unknown-list-head ()
  "Unknown list head signals clime-type-error."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (should-error (clime-resolve-type '(nonexistent 1 2))
                  :type 'clime-type-error)))

(ert-deftest clime-test-types/resolve-invalid-spec ()
  "Non-nil/non-symbol/non-list signals clime-type-error."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (should-error (clime-resolve-type 42)
                  :type 'clime-type-error)))

;;; ─── clime-deftype ─────────────────────────────────────────────────────

(ert-deftest clime-test-types/deftype-zero-args ()
  "clime-deftype with no args registers a bare-symbol type."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (eval '(clime-deftype test-zero ()
             "A test type."
             (list :parse #'identity :describe "test-zero"))
          t)
    (let ((plist (clime-resolve-type 'test-zero)))
      (should (equal "test-zero" (plist-get plist :describe))))))

(ert-deftest clime-test-types/deftype-with-args ()
  "clime-deftype with args registers a parameterized type."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (eval '(clime-deftype test-param (lo hi)
             "A bounded type."
             (list :parse (lambda (s)
                            (let ((n (string-to-number s)))
                              (when (or (< n lo) (> n hi))
                                (error "Out of range"))
                              n))
                   :describe (format "(%d–%d)" lo hi)))
          t)
    (let ((plist (clime-resolve-type '(test-param 10 20))))
      (should (equal "(10–20)" (plist-get plist :describe)))
      (should (= 15 (funcall (plist-get plist :parse) "15"))))))

(ert-deftest clime-test-types/deftype-creates-named-function ()
  "clime-deftype creates clime-type--NAME function."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (eval '(clime-deftype test-named ()
             "Named fn test."
             (list :parse #'identity :describe "named"))
          t)
    (should (fboundp 'clime-type--test-named))))

(ert-deftest clime-test-types/deftype-composable ()
  "clime-deftype body can call clime-resolve-type for delegation."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    ;; Register a base type
    (clime-register-type 'base-ty
                         (lambda () (list :parse #'string-to-number :describe "base")))
    ;; Define a type that delegates
    (eval '(clime-deftype delegating ()
             "Delegates to base."
             (let ((base (clime-resolve-type 'base-ty)))
               (list :parse (plist-get base :parse)
                     :describe (concat "delegating:" (plist-get base :describe)))))
          t)
    (let ((plist (clime-resolve-type 'delegating)))
      (should (equal "delegating:base" (plist-get plist :describe)))
      (should (= 7 (funcall (plist-get plist :parse) "7"))))))

;;; ─── Validation ────────────────────────────────────────────────────────

(ert-deftest clime-test-types/plist-valid ()
  "clime-type-plist-p returns non-nil for valid plist."
  (should (clime-type-plist-p (list :parse #'identity :describe "ok"))))

(ert-deftest clime-test-types/plist-valid-with-choices ()
  "clime-type-plist-p accepts optional :choices."
  (should (clime-type-plist-p (list :parse #'identity :describe "ok" :choices '("a" "b")))))

(ert-deftest clime-test-types/plist-missing-parse ()
  "clime-type-plist-p rejects missing :parse."
  (should-not (clime-type-plist-p (list :describe "no parse"))))

(ert-deftest clime-test-types/plist-missing-describe ()
  "clime-type-plist-p rejects missing :describe."
  (should-not (clime-type-plist-p (list :parse #'identity))))

(ert-deftest clime-test-types/plist-non-function-parse ()
  "clime-type-plist-p rejects non-function :parse."
  (should-not (clime-type-plist-p (list :parse "nope" :describe "bad"))))

(ert-deftest clime-test-types/plist-non-string-describe ()
  "clime-type-plist-p rejects non-string :describe."
  (should-not (clime-type-plist-p (list :parse #'identity :describe 42))))

;;; ─── Edge Cases ────────────────────────────────────────────────────────

(ert-deftest clime-test-types/resolve-list-empty-args ()
  "List spec with no args calls constructor with no arguments."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'bare
                         (lambda () (list :parse #'identity :describe "bare")))
    (let ((plist (clime-resolve-type '(bare))))
      (should (equal "bare" (plist-get plist :describe))))))

(ert-deftest clime-test-types/resolve-list-many-args ()
  "List spec passes all cdr elements as arguments."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'multi
                         (lambda (a b c)
                           (list :parse #'identity
                                 :describe (format "%s-%s-%s" a b c))))
    (let ((plist (clime-resolve-type '(multi x y z))))
      (should (equal "x-y-z" (plist-get plist :describe))))))

(ert-deftest clime-test-types/error-condition-type ()
  "clime-type-error is a defined error condition."
  (should (get 'clime-type-error 'error-conditions))
  (should (memq 'clime-type-error (get 'clime-type-error 'error-conditions))))

(ert-deftest clime-test-types/resolve-does-not-mutate-registry ()
  "Resolving a type does not alter the registry."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (clime-register-type 'immut
                         (lambda () (list :parse #'identity :describe "immut")))
    (clime-resolve-type 'immut)
    (clime-resolve-type 'immut)
    (should (= 1 (hash-table-count clime-type-registry)))))

(ert-deftest clime-test-types/deftype-optional-args ()
  "clime-deftype with &optional args works for bare and parameterized."
  (let ((clime-type-registry (make-hash-table :test 'eq)))
    (eval '(clime-deftype test-opt (&optional min max)
             "Optional bounds."
             (list :parse #'identity
                   :describe (format "opt(%s,%s)" min max)))
          t)
    ;; Bare symbol — no args
    (should (equal "opt(nil,nil)"
                   (plist-get (clime-resolve-type 'test-opt) :describe)))
    ;; Parameterized — with args
    (should (equal "opt(1,10)"
                   (plist-get (clime-resolve-type '(test-opt 1 10)) :describe)))))

(ert-deftest clime-test-types/plist-nil-not-valid ()
  "clime-type-plist-p rejects nil."
  (should-not (clime-type-plist-p nil)))

(ert-deftest clime-test-types/plist-empty-list-not-valid ()
  "clime-type-plist-p rejects empty list."
  (should-not (clime-type-plist-p '())))

;;; ─── Built-in Type Registrations ───────────────────────────────────────

(ert-deftest clime-test-types/builtin-string-registered ()
  "string type is registered and resolves."
  (let ((plist (clime-resolve-type 'string)))
    (should (equal "string" (plist-get plist :describe)))
    (should (equal "hello" (funcall (plist-get plist :parse) "hello")))))

(ert-deftest clime-test-types/builtin-integer-parses ()
  "integer type parses valid integers."
  (let ((parse (plist-get (clime-resolve-type 'integer) :parse)))
    (should (= 42 (funcall parse "42")))
    (should (= -7 (funcall parse "-7")))
    (should (= 0 (funcall parse "0")))))

(ert-deftest clime-test-types/builtin-integer-rejects ()
  "integer type rejects non-integers."
  (let ((parse (plist-get (clime-resolve-type 'integer) :parse)))
    (should-error (funcall parse "abc"))
    (should-error (funcall parse "3.14"))
    (should-error (funcall parse ""))))

(ert-deftest clime-test-types/builtin-number-parses ()
  "number type parses integers and floats."
  (let ((parse (plist-get (clime-resolve-type 'number) :parse)))
    (should (= 42 (funcall parse "42")))
    (should (= 3.14 (funcall parse "3.14")))
    (should (= -2.5 (funcall parse "-2.5")))
    (should (= 0 (funcall parse "0")))))

(ert-deftest clime-test-types/builtin-number-rejects ()
  "number type rejects non-numbers."
  (let ((parse (plist-get (clime-resolve-type 'number) :parse)))
    (should-error (funcall parse "abc"))
    (should-error (funcall parse ""))))

(ert-deftest clime-test-types/builtin-boolean-parses ()
  "boolean type parses truthy/falsy strings."
  (let ((parse (plist-get (clime-resolve-type 'boolean) :parse)))
    (should (eq t (funcall parse "true")))
    (should (eq t (funcall parse "1")))
    (should (eq t (funcall parse "yes")))
    (should (eq t (funcall parse "on")))
    (should (eq t (funcall parse "TRUE")))
    (should (null (funcall parse "false")))
    (should (null (funcall parse "0")))
    (should (null (funcall parse "no")))
    (should (null (funcall parse "off")))
    (should (null (funcall parse "")))))

(ert-deftest clime-test-types/builtin-boolean-rejects ()
  "boolean type rejects unrecognized strings."
  (let ((parse (plist-get (clime-resolve-type 'boolean) :parse)))
    (should-error (funcall parse "maybe"))
    (should-error (funcall parse "2"))))

;;; ─── clime--coerce-value Delegation ────────────────────────────────────

(ert-deftest clime-test-types/coerce-nil-delegates ()
  "clime--coerce-value with nil type returns string as-is."
  (should (equal "hello" (clime--coerce-value "hello" nil "--flag"))))

(ert-deftest clime-test-types/coerce-string-delegates ()
  "clime--coerce-value with 'string delegates to registry."
  (should (equal "hello" (clime--coerce-value "hello" 'string "--flag"))))

(ert-deftest clime-test-types/coerce-integer-delegates ()
  "clime--coerce-value with 'integer delegates to registry."
  (should (= 42 (clime--coerce-value "42" 'integer "--port"))))

(ert-deftest clime-test-types/coerce-integer-error-wraps ()
  "clime--coerce-value wraps integer parse error as clime-usage-error."
  (should-error (clime--coerce-value "abc" 'integer "--port")
                :type 'clime-usage-error))

(ert-deftest clime-test-types/coerce-number-delegates ()
  "clime--coerce-value with 'number delegates to registry."
  (should (= 3.14 (clime--coerce-value "3.14" 'number "--rate"))))

(ert-deftest clime-test-types/coerce-function-still-works ()
  "clime--coerce-value still accepts function types (deprecated)."
  (should (= 42 (clime--coerce-value "42" (lambda (s) (string-to-number s)) "--x"))))

(ert-deftest clime-test-types/coerce-unknown-symbol-errors ()
  "clime--coerce-value signals clime-usage-error for unknown type symbol."
  (should-error (clime--coerce-value "x" 'nonexistent "--flag")
                :type 'clime-usage-error))

;;; ─── Parameterized Integer ──────────────────────────────────────────────

(ert-deftest clime-test-types/integer-bounded ()
  "(integer :min 1 :max 100) accepts in-range, rejects out-of-range."
  (let ((plist (clime-resolve-type '(integer :min 1 :max 100))))
    (should (equal "integer (1–100)" (plist-get plist :describe)))
    (should (= 1 (funcall (plist-get plist :parse) "1")))
    (should (= 50 (funcall (plist-get plist :parse) "50")))
    (should (= 100 (funcall (plist-get plist :parse) "100")))
    (should-error (funcall (plist-get plist :parse) "0"))
    (should-error (funcall (plist-get plist :parse) "101"))))

(ert-deftest clime-test-types/integer-min-only ()
  "(integer :min 1) lower-bounded."
  (let ((plist (clime-resolve-type '(integer :min 1))))
    (should (equal "integer (≥1)" (plist-get plist :describe)))
    (should (= 999 (funcall (plist-get plist :parse) "999")))
    (should-error (funcall (plist-get plist :parse) "0"))))

(ert-deftest clime-test-types/integer-max-only ()
  "(integer :max 100) upper-bounded."
  (let ((plist (clime-resolve-type '(integer :max 100))))
    (should (equal "integer (≤100)" (plist-get plist :describe)))
    (should (= -5 (funcall (plist-get plist :parse) "-5")))
    (should-error (funcall (plist-get plist :parse) "101"))))

(ert-deftest clime-test-types/integer-no-bounds ()
  "(integer) with no bounds same as bare 'integer."
  (let ((plist (clime-resolve-type '(integer))))
    (should (equal "integer" (plist-get plist :describe)))
    (should (= 42 (funcall (plist-get plist :parse) "42")))))

;;; ─── Parameterized Number ──────────────────────────────────────────────

(ert-deftest clime-test-types/number-bounded ()
  "(number :min -1.5 :max 1.5) accepts in-range floats."
  (let ((plist (clime-resolve-type '(number :min -1.5 :max 1.5))))
    (should (equal "number (-1.5–1.5)" (plist-get plist :describe)))
    (should (= 0.5 (funcall (plist-get plist :parse) "0.5")))
    (should (= -1.5 (funcall (plist-get plist :parse) "-1.5")))
    (should-error (funcall (plist-get plist :parse) "2.0"))))

(ert-deftest clime-test-types/number-min-only ()
  "(number :min 0) lower-bounded."
  (let ((plist (clime-resolve-type '(number :min 0))))
    (should (equal "number (≥0)" (plist-get plist :describe)))
    (should (= 3.14 (funcall (plist-get plist :parse) "3.14")))
    (should-error (funcall (plist-get plist :parse) "-0.1"))))

;;; ─── Parameterized String ──────────────────────────────────────────────

(ert-deftest clime-test-types/string-match ()
  "(string :match REGEXP) validates pattern."
  (let ((plist (clime-resolve-type '(string :match "^[a-z]+$"))))
    (should (string-match-p "matching" (plist-get plist :describe)))
    (should (equal "hello" (funcall (plist-get plist :parse) "hello")))
    (should-error (funcall (plist-get plist :parse) "Hello123"))))

(ert-deftest clime-test-types/string-no-match ()
  "(string) with no :match same as bare 'string."
  (let ((plist (clime-resolve-type '(string))))
    (should (equal "string" (plist-get plist :describe)))
    (should (equal "anything" (funcall (plist-get plist :parse) "anything")))))

;;; ─── Parameterized via clime--coerce-value ─────────────────────────────

(ert-deftest clime-test-types/coerce-bounded-integer ()
  "clime--coerce-value with bounded integer spec."
  (should (= 8080 (clime--coerce-value "8080" '(integer :min 1 :max 65535) "--port")))
  (should-error (clime--coerce-value "0" '(integer :min 1 :max 65535) "--port")
                :type 'clime-usage-error))

(provide 'clime-param-type-tests)
;;; clime-param-type-tests.el ends here
