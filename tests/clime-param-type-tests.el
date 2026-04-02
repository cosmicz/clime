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
                                 :describe (format "integer %d–%d" min max))))
    (let ((plist (clime-resolve-type '(bounded 1 100))))
      (should (equal "integer 1–100" (plist-get plist :describe)))
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

(ert-deftest clime-test-types/coerce-function-signals-error ()
  "clime--coerce-value rejects function :type with clime-usage-error."
  (should-error (clime--coerce-value "42" (lambda (s) (string-to-number s)) "--x")
                :type 'clime-usage-error))

(ert-deftest clime-test-types/coerce-unknown-symbol-errors ()
  "clime--coerce-value signals clime-usage-error for unknown type symbol."
  (should-error (clime--coerce-value "x" 'nonexistent "--flag")
                :type 'clime-usage-error))

;;; ─── Parameterized Integer ──────────────────────────────────────────────

(ert-deftest clime-test-types/integer-bounded ()
  "(integer :min 1 :max 100) accepts in-range, rejects out-of-range."
  (let ((plist (clime-resolve-type '(integer :min 1 :max 100))))
    (should (equal "integer 1–100" (plist-get plist :describe)))
    (should (= 1 (funcall (plist-get plist :parse) "1")))
    (should (= 50 (funcall (plist-get plist :parse) "50")))
    (should (= 100 (funcall (plist-get plist :parse) "100")))
    (should-error (funcall (plist-get plist :parse) "0"))
    (should-error (funcall (plist-get plist :parse) "101"))))

(ert-deftest clime-test-types/integer-min-only ()
  "(integer :min 1) lower-bounded."
  (let ((plist (clime-resolve-type '(integer :min 1))))
    (should (equal "integer ≥1" (plist-get plist :describe)))
    (should (= 999 (funcall (plist-get plist :parse) "999")))
    (should-error (funcall (plist-get plist :parse) "0"))))

(ert-deftest clime-test-types/integer-max-only ()
  "(integer :max 100) upper-bounded."
  (let ((plist (clime-resolve-type '(integer :max 100))))
    (should (equal "integer ≤100" (plist-get plist :describe)))
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
    (should (equal "number -1.5–1.5" (plist-get plist :describe)))
    (should (= 0.5 (funcall (plist-get plist :parse) "0.5")))
    (should (= -1.5 (funcall (plist-get plist :parse) "-1.5")))
    (should-error (funcall (plist-get plist :parse) "2.0"))))

(ert-deftest clime-test-types/number-min-only ()
  "(number :min 0) lower-bounded."
  (let ((plist (clime-resolve-type '(number :min 0))))
    (should (equal "number ≥0" (plist-get plist :describe)))
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

;;; ─── member Type ──────────────────────────────────────────────────────

(ert-deftest clime-test-types/member-accepts-valid ()
  "(member ...) accepts listed values."
  (let ((parse (plist-get (clime-resolve-type '(member "json" "csv" "table")) :parse)))
    (should (equal "json" (funcall parse "json")))
    (should (equal "csv" (funcall parse "csv")))
    (should (equal "table" (funcall parse "table")))))

(ert-deftest clime-test-types/member-rejects-invalid ()
  "(member ...) rejects unlisted values."
  (let ((parse (plist-get (clime-resolve-type '(member "json" "csv" "table")) :parse)))
    (should-error (funcall parse "xml"))
    (should-error (funcall parse ""))
    (should-error (funcall parse "JSON"))))

(ert-deftest clime-test-types/member-describe ()
  "(member ...) :describe joins values with \"|\"."
  (should (equal "json|csv|table"
                 (plist-get (clime-resolve-type '(member "json" "csv" "table")) :describe))))

(ert-deftest clime-test-types/member-choices ()
  "(member ...) :choices returns the value list."
  (should (equal '("json" "csv" "table")
                 (plist-get (clime-resolve-type '(member "json" "csv" "table")) :choices))))

(ert-deftest clime-test-types/member-single-value ()
  "(member \"x\") accepts only that value."
  (let ((plist (clime-resolve-type '(member "x"))))
    (should (equal "x" (funcall (plist-get plist :parse) "x")))
    (should-error (funcall (plist-get plist :parse) "y"))
    (should (equal '("x") (plist-get plist :choices)))))

;;; ─── const Type ──────────────────────────────────────────────────────

(ert-deftest clime-test-types/const-accepts-match ()
  "(const \"off\") accepts exact match."
  (let ((plist (clime-resolve-type '(const "off"))))
    (should (equal "off" (funcall (plist-get plist :parse) "off")))))

(ert-deftest clime-test-types/const-rejects-mismatch ()
  "(const \"off\") rejects non-matching input."
  (let ((parse (plist-get (clime-resolve-type '(const "off")) :parse)))
    (should-error (funcall parse "on"))
    (should-error (funcall parse "OFF"))
    (should-error (funcall parse ""))))

(ert-deftest clime-test-types/const-returns-original-value ()
  "(const 42) returns the original numeric value, not the string."
  (should (eq 42 (funcall (plist-get (clime-resolve-type '(const 42)) :parse) "42"))))

(ert-deftest clime-test-types/const-describe ()
  "(const \"off\") :describe is quoted value."
  (should (equal "\"off\"" (plist-get (clime-resolve-type '(const "off")) :describe))))

(ert-deftest clime-test-types/const-choices ()
  "(const \"off\") :choices is single-element list."
  (should (equal '("off") (plist-get (clime-resolve-type '(const "off")) :choices))))

(ert-deftest clime-test-types/const-non-string-value ()
  "(const 42) compares against stringified form."
  (let ((plist (clime-resolve-type '(const 42))))
    (should (equal "\"42\"" (plist-get plist :describe)))
    (should (equal '("42") (plist-get plist :choices)))
    (should-error (funcall (plist-get plist :parse) "43"))))

;;; ─── choice Type ─────────────────────────────────────────────────────

(ert-deftest clime-test-types/choice-first-match-wins ()
  "(choice integer (const \"off\")) parses integer first."
  (let ((parse (plist-get (clime-resolve-type '(choice integer (const "off"))) :parse)))
    (should (= 42 (funcall parse "42")))
    (should (equal "off" (funcall parse "off")))))

(ert-deftest clime-test-types/choice-bounded-integer ()
  "(choice (integer :min 1) (const \"off\")) with bounded integer."
  (let ((parse (plist-get (clime-resolve-type '(choice (integer :min 1) (const "off"))) :parse)))
    (should (= 30 (funcall parse "30")))
    (should (equal "off" (funcall parse "off")))
    (should-error (funcall parse "foo"))))

(ert-deftest clime-test-types/choice-rejects-all-fail ()
  "(choice ...) rejects when all alternatives fail."
  (let ((parse (plist-get (clime-resolve-type '(choice integer boolean)) :parse)))
    (should-error (funcall parse "not-a-thing"))))

(ert-deftest clime-test-types/choice-describe ()
  "(choice ...) :describe joins with \"|\"."
  (should (equal "integer|\"off\""
                 (plist-get (clime-resolve-type '(choice integer (const "off"))) :describe))))

(ert-deftest clime-test-types/choice-choices-unions ()
  "(choice ...) :choices unions sub-type choices."
  (let ((plist (clime-resolve-type '(choice (member "a" "b") (const "c")))))
    (should (equal '("a" "b" "c") (plist-get plist :choices)))))

(ert-deftest clime-test-types/choice-choices-skips-no-choices ()
  "(choice integer (const \"off\")) only collects choices from types that have them."
  (let ((plist (clime-resolve-type '(choice integer (const "off")))))
    (should (equal '("off") (plist-get plist :choices)))))

(ert-deftest clime-test-types/choice-nested ()
  "Nested choice works."
  (let ((parse (plist-get (clime-resolve-type
                           '(choice (choice integer boolean) (const "auto")))
                          :parse)))
    (should (= 5 (funcall parse "5")))
    (should (eq t (funcall parse "true")))
    (should (equal "auto" (funcall parse "auto")))
    (should-error (funcall parse "nope"))))

;;; ─── Composite via clime--coerce-value ───────────────────────────────

(ert-deftest clime-test-types/coerce-member ()
  "clime--coerce-value with member type."
  (should (equal "json" (clime--coerce-value "json" '(member "json" "csv") "--format")))
  (should-error (clime--coerce-value "xml" '(member "json" "csv") "--format")
                :type 'clime-usage-error))

(ert-deftest clime-test-types/coerce-choice ()
  "clime--coerce-value with choice type."
  (should (= 8080 (clime--coerce-value "8080" '(choice (integer :min 1) (const "off")) "--port")))
  (should (equal "off" (clime--coerce-value "off" '(choice (integer :min 1) (const "off")) "--port")))
  (should-error (clime--coerce-value "bad" '(choice (integer :min 1) (const "off")) "--port")
                :type 'clime-usage-error))

;;; ─── Composite Edge Cases ─────────────────────────────────────────────

(ert-deftest clime-test-types/member-valid-plist ()
  "(member ...) resolves to a valid type plist."
  (should (clime-type-plist-p (clime-resolve-type '(member "a" "b")))))

(ert-deftest clime-test-types/const-valid-plist ()
  "(const ...) resolves to a valid type plist."
  (should (clime-type-plist-p (clime-resolve-type '(const "x")))))

(ert-deftest clime-test-types/choice-valid-plist ()
  "(choice ...) resolves to a valid type plist."
  (should (clime-type-plist-p (clime-resolve-type '(choice integer string)))))

(ert-deftest clime-test-types/member-error-message-lists-values ()
  "(member ...) error message includes available values."
  (condition-case err
      (funcall (plist-get (clime-resolve-type '(member "a" "b")) :parse) "c")
    (error
     (should (string-match-p "\"a\"" (error-message-string err)))
     (should (string-match-p "\"b\"" (error-message-string err))))))

(ert-deftest clime-test-types/choice-error-message-includes-sub-errors ()
  "(choice ...) error message includes each sub-type's error."
  (condition-case err
      (funcall (plist-get (clime-resolve-type '(choice integer (const "off"))) :parse) "bad")
    (error
     (should (string-match-p "integer" (error-message-string err)))
     (should (string-match-p "\"off\"" (error-message-string err))))))

(ert-deftest clime-test-types/choice-single-alternative ()
  "(choice integer) with single alternative works like bare integer."
  (let ((parse (plist-get (clime-resolve-type '(choice integer)) :parse)))
    (should (= 5 (funcall parse "5")))
    (should-error (funcall parse "abc"))))

(ert-deftest clime-test-types/choice-order-matters ()
  "First matching alternative wins — (choice string integer) always returns string."
  (let ((parse (plist-get (clime-resolve-type '(choice string integer)) :parse)))
    ;; string :parse is identity, so "42" stays a string
    (should (equal "42" (funcall parse "42")))))

(ert-deftest clime-test-types/const-empty-string ()
  "(const \"\") matches empty string."
  (let ((plist (clime-resolve-type '(const ""))))
    (should (equal "" (funcall (plist-get plist :parse) "")))
    (should-error (funcall (plist-get plist :parse) "x"))))

(ert-deftest clime-test-types/choice-bounded-integer-rejects-out-of-range ()
  "(choice (integer :min 1 :max 10) (const \"off\")) rejects 0 (out of range, not \"off\")."
  (let ((parse (plist-get (clime-resolve-type '(choice (integer :min 1 :max 10) (const "off"))) :parse)))
    (should-error (funcall parse "0"))))

(ert-deftest clime-test-types/coerce-const ()
  "clime--coerce-value with const type."
  (should (equal "off" (clime--coerce-value "off" '(const "off") "--mode")))
  (should-error (clime--coerce-value "on" '(const "off") "--mode")
                :type 'clime-usage-error))

(ert-deftest clime-test-types/choice-nested-choices-union ()
  "Nested choice unions :choices from all levels."
  (let ((plist (clime-resolve-type '(choice (member "a") (choice (const "b") (member "c" "d"))))))
    (should (equal '("a" "b" "c" "d") (plist-get plist :choices)))))

;;; ─── clime--type-describe ─────────────────────────────────────────────

(ert-deftest clime-test-types/type-describe-nil ()
  "clime--type-describe returns nil for nil type."
  (should-not (clime--type-describe nil)))

(ert-deftest clime-test-types/type-describe-string ()
  "clime--type-describe returns nil for plain string type."
  (should-not (clime--type-describe 'string)))

(ert-deftest clime-test-types/type-describe-integer ()
  "clime--type-describe returns \"integer\" for integer type."
  (should (equal "integer" (clime--type-describe 'integer))))

(ert-deftest clime-test-types/type-describe-bounded-integer ()
  "clime--type-describe returns bounded description."
  (should (equal "integer 1–100" (clime--type-describe '(integer :min 1 :max 100)))))

(ert-deftest clime-test-types/type-describe-member ()
  "clime--type-describe returns member description."
  (should (equal "json|csv" (clime--type-describe '(member "json" "csv")))))

(ert-deftest clime-test-types/type-describe-choice ()
  "clime--type-describe returns choice description."
  (should (equal "integer|\"off\""
                 (clime--type-describe '(choice integer (const "off"))))))

(ert-deftest clime-test-types/type-describe-unknown-returns-nil ()
  "clime--type-describe returns nil for unknown type (no error)."
  (should-not (clime--type-describe 'nonexistent-type-xyz)))

;;; ─── clime--effective-choices ────────────────────────────────────────

(ert-deftest clime-test-types/effective-choices-explicit-wins ()
  "Explicit :choices overrides type-provided choices."
  (let ((opt (clime-make-option :name 'fmt :flags '("-f")
                           :type '(member "json" "csv")
                           :choices '("xml" "yaml"))))
    (should (equal '("xml" "yaml") (clime--effective-choices opt)))))

(ert-deftest clime-test-types/effective-choices-falls-back-to-type ()
  "Falls back to type :choices when explicit is nil."
  (let ((opt (clime-make-option :name 'fmt :flags '("-f")
                           :type '(member "json" "csv"))))
    (should (equal '("json" "csv") (clime--effective-choices opt)))))

(ert-deftest clime-test-types/effective-choices-nil-when-no-choices ()
  "Returns nil when neither explicit nor type provides choices."
  (let ((opt (clime-make-option :name 'port :flags '("-p") :type 'integer)))
    (should-not (clime--effective-choices opt))))

(ert-deftest clime-test-types/effective-choices-arg ()
  "Works for args too."
  (let ((arg (clime-make-arg :name 'format :type '(member "json" "csv"))))
    (should (equal '("json" "csv") (clime--effective-choices arg)))))

(ert-deftest clime-test-types/effective-choices-unknown-type-returns-nil ()
  "Returns nil for unknown type (no error)."
  (let ((opt (clime-make-option :name 'x :flags '("--x") :type 'nonexistent-xyz)))
    (should-not (clime--effective-choices opt))))

;;; ─── Short Aliases ───────────────────────────────────────────────────

(ert-deftest clime-test-types/short-int ()
  "'int resolves to integer."
  (let ((plist (clime-resolve-type 'int)))
    (should (equal "integer" (plist-get plist :describe)))
    (should (= 42 (funcall (plist-get plist :parse) "42")))))

(ert-deftest clime-test-types/short-int-parameterized ()
  "(int :min 1) works like (integer :min 1)."
  (let ((plist (clime-resolve-type '(int :min 1))))
    (should (equal "integer ≥1" (plist-get plist :describe)))
    (should-error (funcall (plist-get plist :parse) "0"))))

(ert-deftest clime-test-types/short-num ()
  "'num resolves to number."
  (should (= 3.14 (funcall (plist-get (clime-resolve-type 'num) :parse) "3.14"))))

(ert-deftest clime-test-types/short-str ()
  "'str resolves to string."
  (should (equal "hello" (funcall (plist-get (clime-resolve-type 'str) :parse) "hello"))))

(ert-deftest clime-test-types/short-bool ()
  "'bool resolves to boolean."
  (should (eq t (funcall (plist-get (clime-resolve-type 'bool) :parse) "true"))))

;;; ─── Redundancy Check ───────────────────────────────────────────────

(ert-deftest clime-test-types/redundant-member ()
  "Member type is redundant with its own choices."
  (should (clime--type-describe-redundant-p
           '(member "a" "b") '("a" "b"))))

(ert-deftest clime-test-types/not-redundant-choice ()
  "Choice type is NOT redundant with its choices."
  (should-not (clime--type-describe-redundant-p
               '(choice integer (const "off")) '("off"))))

(ert-deftest clime-test-types/not-redundant-integer ()
  "Integer type has no choices, not redundant."
  (should-not (clime--type-describe-redundant-p 'integer nil)))

(ert-deftest clime-test-types/not-redundant-member-with-explicit-choices ()
  "Member type is not redundant when effective choices differ."
  (should-not (clime--type-describe-redundant-p
               '(member "a" "b") '("x" "y"))))

;;; ─── Compact describe ───────────────────────────────────────────────

(ert-deftest clime-test-type/compact-describe-choice-member-integer ()
  "Compact describe collapses member to 'choice', keeps integer."
  (should (equal "choice|integer ≥1"
                 (clime--type-describe-compact
                  '(choice (member "auto" "serial" "cpus") (integer :min 1))))))

(ert-deftest clime-test-type/compact-describe-choice-number-const ()
  "Compact describe collapses const to 'choice', keeps number."
  (should (equal "choice|number ≥0"
                 (clime--type-describe-compact
                  '(choice (number :min 0) (const "off"))))))

(ert-deftest clime-test-type/compact-describe-pure-member ()
  "Pure member type returns nil (fully redundant with choices)."
  (should-not (clime--type-describe-compact '(member "a" "b" "c"))))

(ert-deftest clime-test-type/compact-describe-pure-choice-member-const ()
  "Choice with only member/const branches returns nil (redundant)."
  (should-not (clime--type-describe-compact
               '(choice (member "a" "b") (const "off")))))

(ert-deftest clime-test-type/compact-describe-plain-integer ()
  "Non-choice types fall back to standard describe."
  (should (equal "integer 1–100"
                 (clime--type-describe-compact '(integer :min 1 :max 100)))))

(ert-deftest clime-test-type/compact-describe-plain-symbol ()
  "Plain symbol type falls back to standard describe."
  (should (equal "integer"
                 (clime--type-describe-compact 'integer))))

(ert-deftest clime-test-type/compact-describe-nil ()
  "Nil type returns nil."
  (should-not (clime--type-describe-compact nil)))

(ert-deftest clime-test-type/compact-describe-string ()
  "String type returns nil (suppressed)."
  (should-not (clime--type-describe-compact 'string)))

(ert-deftest clime-test-type/compact-describe-choice-with-string ()
  "Choice with string branch: string suppressed, member collapsed → nil."
  (should-not (clime--type-describe-compact
               '(choice (member "a" "b") (string)))))

;;; ─── File/Directory/Path Types ──────────────────────────────────────

(ert-deftest clime-test-type/file-bare ()
  "Bare file type resolves with :describe \"file\"."
  (let ((plist (clime-resolve-type 'file)))
    (should (clime-type-plist-p plist))
    (should (equal "file" (plist-get plist :describe)))))

(ert-deftest clime-test-type/file-parse-expands ()
  "File parser expands the path."
  (let* ((plist (clime-resolve-type 'file))
         (parse (plist-get plist :parse))
         (result (funcall parse "~/foo.txt")))
    (should (string-prefix-p "/" result))
    (should (string-suffix-p "/foo.txt" result))))

(ert-deftest clime-test-type/file-must-exist-rejects-missing ()
  "File :must-exist rejects non-existent path."
  (let* ((plist (clime-resolve-type '(file :must-exist t)))
         (parse (plist-get plist :parse)))
    (should (equal "file (existing)" (plist-get plist :describe)))
    (should-error (funcall parse "/tmp/clime-test-nonexistent-file-xyz"))))

(ert-deftest clime-test-type/file-must-exist-accepts-regular-file ()
  "File :must-exist accepts an existing regular file."
  (let* ((tmp (make-temp-file "clime-test"))
         (plist (clime-resolve-type '(file :must-exist t)))
         (parse (plist-get plist :parse)))
    (unwind-protect
        (should (stringp (funcall parse tmp)))
      (delete-file tmp))))

(ert-deftest clime-test-type/file-must-exist-rejects-directory ()
  "File :must-exist rejects a directory (not a regular file)."
  (let* ((tmp (make-temp-file "clime-test" t))
         (plist (clime-resolve-type '(file :must-exist t)))
         (parse (plist-get plist :parse)))
    (unwind-protect
        (should-error (funcall parse tmp))
      (delete-directory tmp))))

(ert-deftest clime-test-type/directory-bare ()
  "Bare directory type resolves with :describe \"directory\"."
  (let ((plist (clime-resolve-type 'directory)))
    (should (clime-type-plist-p plist))
    (should (equal "directory" (plist-get plist :describe)))))

(ert-deftest clime-test-type/directory-must-exist-accepts-dir ()
  "Directory :must-exist accepts an existing directory."
  (let* ((tmp (make-temp-file "clime-test" t))
         (plist (clime-resolve-type '(directory :must-exist t)))
         (parse (plist-get plist :parse)))
    (unwind-protect
        (should (stringp (funcall parse tmp)))
      (delete-directory tmp))))

(ert-deftest clime-test-type/directory-must-exist-rejects-file ()
  "Directory :must-exist rejects a regular file."
  (let* ((tmp (make-temp-file "clime-test"))
         (plist (clime-resolve-type '(directory :must-exist t)))
         (parse (plist-get plist :parse)))
    (unwind-protect
        (should-error (funcall parse tmp))
      (delete-file tmp))))

(ert-deftest clime-test-type/directory-must-exist-rejects-missing ()
  "Directory :must-exist rejects non-existent path."
  (let* ((plist (clime-resolve-type '(directory :must-exist t)))
         (parse (plist-get plist :parse)))
    (should (equal "directory (existing)" (plist-get plist :describe)))
    (should-error (funcall parse "/tmp/clime-test-nonexistent-dir-xyz"))))

(ert-deftest clime-test-type/path-bare ()
  "Bare path type resolves with :describe \"path\"."
  (let ((plist (clime-resolve-type 'path)))
    (should (clime-type-plist-p plist))
    (should (equal "path" (plist-get plist :describe)))))

(ert-deftest clime-test-type/path-must-exist-accepts-file ()
  "Path :must-exist accepts an existing regular file."
  (let* ((tmp (make-temp-file "clime-test"))
         (plist (clime-resolve-type '(path :must-exist t)))
         (parse (plist-get plist :parse)))
    (unwind-protect
        (should (stringp (funcall parse tmp)))
      (delete-file tmp))))

(ert-deftest clime-test-type/path-must-exist-accepts-dir ()
  "Path :must-exist accepts an existing directory."
  (let* ((tmp (make-temp-file "clime-test" t))
         (plist (clime-resolve-type '(path :must-exist t)))
         (parse (plist-get plist :parse)))
    (unwind-protect
        (should (stringp (funcall parse tmp)))
      (delete-directory tmp))))

(ert-deftest clime-test-type/path-must-exist-rejects-missing ()
  "Path :must-exist rejects non-existent path."
  (let* ((plist (clime-resolve-type '(path :must-exist t)))
         (parse (plist-get plist :parse)))
    (should (equal "path (existing)" (plist-get plist :describe)))
    (should-error (funcall parse "/tmp/clime-test-nonexistent-path-xyz"))))

(ert-deftest clime-test-type/file-in-choice ()
  "File type works inside a choice with const."
  (let* ((plist (clime-resolve-type '(choice (file :must-exist t) (const "-"))))
         (parse (plist-get plist :parse)))
    ;; const "-" parses
    (should (equal "-" (funcall parse "-")))
    ;; existing file parses
    (let ((tmp (make-temp-file "clime-test")))
      (unwind-protect
          (should (stringp (funcall parse tmp)))
        (delete-file tmp)))))

(ert-deftest clime-test-type/dir-alias ()
  "Dir is a short alias for directory."
  (let ((plist (clime-resolve-type 'dir)))
    (should (clime-type-plist-p plist))
    (should (equal "directory" (plist-get plist :describe)))))

(ert-deftest clime-test-type/file-bare-accepts-nonexistent ()
  "Bare file type does not check existence."
  (let* ((plist (clime-resolve-type 'file))
         (parse (plist-get plist :parse)))
    (should (stringp (funcall parse "/tmp/clime-test-nonexistent-xyz")))))

(provide 'clime-param-type-tests)
;;; clime-param-type-tests.el ends here
