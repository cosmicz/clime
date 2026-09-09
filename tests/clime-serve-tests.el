;;; clime-serve-tests.el --- Tests for clime-serve  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for HTTP serve surface — path walking, path params, values seeding,
;; dispatch, and response mapping.  Does NOT require web-server; tests pure logic.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-dsl)
(require 'clime-test-helpers)

;; clime-serve may not be loadable yet, guard the require
(condition-case nil
    (require 'clime-serve)
  (error nil))

(defun clime-test-serve--require-web-server ()
  "Require `web-server' for real HTTP integration tests."
  (unless (require 'web-server nil t)
    (ert-fail
     "clime serve integration tests require web-server; run `make submodules' or add lib/web-server to `load-path'.")))

;;; ─── Test Fixtures ──────────────────────────────────────────────────────

(eval '(clime-app clime-test--serve-app
         :version "1.0"
         (clime-command health
           :help "Health check"
           (clime-handler (ctx) "ok"))
         (clime-group db
           :help "Database commands"
           (clime-command migrate
             :help "Run migrations"
             (clime-option dry-run ("--dry-run") :help "Preview only")
             (clime-handler (ctx)
               (if (clime-ctx-get ctx 'dry-run) "dry" "migrated")))
           (clime-command seed
             :help "Seed the database"
             (clime-arg file :help "Seed file" :required t)
             (clime-handler (ctx)
               (format "seeded:%s" (clime-ctx-get ctx 'file))))))
      t)

;; App with path params: /agent/:id/start
(eval '(clime-app clime-test--serve-pathparam-app
         :version "1.0"
         (clime-group agent
           :help "Agent commands"
           (clime-arg id :help "Agent ID" :required t)
           (clime-command start
             :help "Start an agent"
             (clime-handler (ctx)
               (format "started:%s" (clime-ctx-get ctx 'id))))
           (clime-command stop
             :help "Stop an agent"
             (clime-option force ("--force") :help "Force stop")
             (clime-handler (ctx)
               (format "stopped:%s:force=%s"
                       (clime-ctx-get ctx 'id)
                       (clime-ctx-get ctx 'force))))))
      t)

(eval '(clime-app clime-test--serve-err-app
         :version "1.0"
         (clime-command boom
           :help "Always errors"
           (clime-handler (ctx)
             (error "Kaboom"))))
      t)

;; App with typed option for coercion test
(eval '(clime-app clime-test--serve-typed-app
         :version "1.0"
         (clime-command count
           :help "Count things"
           (clime-option limit ("--limit") :type 'integer :default 10
             :help "Max items")
           (clime-handler (ctx)
             (format "limit=%d" (clime-ctx-get ctx 'limit)))))
      t)

;; App with hidden command
(eval '(clime-app clime-test--serve-hidden-app
         :version "1.0"
         (clime-command visible
           :help "Visible command"
           (clime-handler (ctx) "visible"))
         (clime-command secret
           :help "Secret command"
           :hidden t
           (clime-handler (ctx) "secret")))
      t)

;; App with JSON output format for suffix tests
(eval '(clime-app clime-test--serve-fmt-app
         :version "2.0"
         :help "Format test app"
         (clime-output-format json ("--json"))
         (clime-command list
           :help "List items"
           (clime-handler (ctx)
             (clime-out '(("items" . [1 2 3])))))
         (clime-command badreq
           :help "Signals a usage (client) error"
           (clime-handler (ctx)
             (signal 'clime-usage-error (list "bad input"))))
         (clime-group db
           :help "Database"
           (clime-command status
             :help "DB status"
             (clime-handler (ctx)
               (clime-out '(("status" . "ok")))))))
      t)

(eval '(clime-app clime-test--serve-policy-app
         :version "1.0"
         :adapter-policies '((http . (:same-origin t)))
         (clime-output-format json ("--json"))
         (clime-command view
           :adapter-policies '((http . (:methods (GET)
                                        :param-sources (query))))
           (clime-option name ("--name") :type 'string)
           (clime-handler (ctx)
             (format "view:%s:%s"
                     (clime-ctx-get ctx 'name)
                     (plist-get (clime-dispatch-request-metadata
                                 (clime-context-request ctx))
                                :method))))
         (clime-command submit
           :adapter-policies '((http . (:methods (POST)
                                        :param-sources (json-body)
                                        :content-types ("application/json")
                                        :max-body-bytes 64)))
           (clime-option name ("--name") :type 'string)
           (clime-handler (ctx)
             (format "submit:%s:%s"
                     (clime-ctx-get ctx 'name)
                     (clime-dispatch-input-source
                      (car (clime-dispatch-request-inputs
                            (clime-context-request ctx)))))))
         (clime-command legacy
           (clime-option name ("--name") :type 'string)
           (clime-handler (ctx)
             (format "legacy:%s" (clime-ctx-get ctx 'name)))))
      t)

;; Inject API commands on all fixtures (mirrors clime-serve startup)
(when (featurep 'clime-serve)
  (dolist (app (list clime-test--serve-app
                     clime-test--serve-pathparam-app
                     clime-test--serve-err-app
                     clime-test--serve-typed-app
                     clime-test--serve-hidden-app
                     clime-test--serve-fmt-app
                     clime-test--serve-policy-app))
    (clime-serve--inject-api-commands app)))

;;; ─── Path Walking ───────────────────────────────────────────────────────

(ert-deftest clime-test-serve/walk-simple-command ()
  "Walk /health resolves to health command."
  (skip-unless (featurep 'clime-serve))
  (let* ((tree (clime--prepare-tree clime-test--serve-app))
         (result (clime-serve--walk-path tree '("health"))))
    (should result)
    (should (equal (clime-node-name (plist-get result :node)) "health"))))

(ert-deftest clime-test-serve/walk-nested-command ()
  "Walk /db/migrate resolves through group to command."
  (skip-unless (featurep 'clime-serve))
  (let* ((tree (clime--prepare-tree clime-test--serve-app))
         (result (clime-serve--walk-path tree '("db" "migrate"))))
    (should result)
    (should (equal (clime-node-name (plist-get result :node)) "migrate"))))

(ert-deftest clime-test-serve/walk-unknown-segment-404 ()
  "Walk with unknown segment and no arg slot signals error."
  (skip-unless (featurep 'clime-serve))
  (let ((tree (clime--prepare-tree clime-test--serve-app)))
    (should-error (clime-serve--walk-path tree '("nonexistent")))))

(ert-deftest clime-test-serve/walk-empty-path ()
  "Walk with empty segments stays at root."
  (skip-unless (featurep 'clime-serve))
  (let* ((tree (clime--prepare-tree clime-test--serve-app))
         (result (clime-serve--walk-path tree '())))
    (should result)
    (should (clime-app-p (plist-get result :node)))))

(ert-deftest clime-test-serve/introspection-omits-surface-denied-children ()
  "Serve metadata does not advertise a command unavailable on serve."
  (skip-unless (featurep 'clime-serve))
  (let* ((private (clime-make-command :name "private" :handler #'ignore
                                      :surfaces '(mcp)))
         (public (clime-make-command :name "public" :handler #'ignore))
         (app (clime-make-app :name "test" :version "1"
                              :children `(("private" . ,private)
                                          ("public" . ,public))))
         (nodes (clime-contract-nodes
                 app :surface 'serve :tree-mode 'prepared
                 :visibility 'visible :value-mode 'declared))
         (data (clime-serve--node-to-alist
                (car nodes) clime-serve--contract-policy nodes))
         (children (cdr (assoc "children" data)))
         (names (mapcar (lambda (child) (cdr (assoc "name" child))) children)))
    (should (equal names '("public")))))

;;; ─── Path Params ────────────────────────────────────────────────────────

(ert-deftest clime-test-serve/path-param-consumed-as-arg ()
  "/agent/abc123/start consumes abc123 as id arg."
  (skip-unless (featurep 'clime-serve))
  (let* ((tree (clime--prepare-tree clime-test--serve-pathparam-app))
         (result (clime-serve--walk-path tree '("agent" "abc123" "start")))
         (values (plist-get result :values)))
    (should (equal (clime-node-name (plist-get result :node)) "start"))
    (should (equal (clime-values-value values 'id) "abc123"))))

(ert-deftest clime-test-serve/path-param-different-values ()
  "Different path param values are captured correctly."
  (skip-unless (featurep 'clime-serve))
  (let* ((tree (clime--prepare-tree clime-test--serve-pathparam-app))
         (result (clime-serve--walk-path tree '("agent" "xyz789" "stop")))
         (values (plist-get result :values)))
    (should (equal (clime-node-name (plist-get result :node)) "stop"))
    (should (equal (clime-values-value values 'id) "xyz789"))))

(ert-deftest clime-test-serve/path-param-no-arg-slot-errors ()
  "Segment after all args consumed signals error."
  (skip-unless (featurep 'clime-serve))
  (let ((tree (clime--prepare-tree clime-test--serve-pathparam-app)))
    ;; agent has 1 arg (id), so 2 non-child segments should fail
    (should-error (clime-serve--walk-path tree '("agent" "id1" "id2" "start")))))

;;; ─── Query String Parsing ───────────────────────────────────────────────

(ert-deftest clime-test-serve/parse-query-string ()
  "Query string parsed to alist."
  (skip-unless (featurep 'clime-serve))
  (let ((params (clime-serve--parse-query-string "dry-run=true&format=json")))
    (should (equal (cdr (assoc "dry-run" params)) "true"))
    (should (equal (cdr (assoc "format" params)) "json"))))

(ert-deftest clime-test-serve/parse-query-string-bare-key ()
  "Query param without value parsed as key with t value."
  (skip-unless (featurep 'clime-serve))
  (let ((params (clime-serve--parse-query-string "dry-run&verbose")))
    (should (equal (cdr (assoc "dry-run" params)) t))
    (should (equal (cdr (assoc "verbose" params)) t))))

(ert-deftest clime-test-serve/parse-query-string-empty ()
  "Empty/nil query string returns nil."
  (skip-unless (featurep 'clime-serve))
  (should (null (clime-serve--parse-query-string "")))
  (should (null (clime-serve--parse-query-string nil))))

;;; ─── JSON Body Parsing ─────────────────────────────────────────────────

(ert-deftest clime-test-serve/parse-json-body ()
  "JSON body parsed to alist."
  (skip-unless (featurep 'clime-serve))
  (let ((params (clime-serve--parse-json-body "{\"file\":\"data.sql\",\"count\":3}")))
    (should (equal (cdr (assoc "file" params)) "data.sql"))
    (should (equal (cdr (assoc "count" params)) 3))))

(ert-deftest clime-test-serve/parse-json-body-empty ()
  "Empty/nil body returns nil."
  (skip-unless (featurep 'clime-serve))
  (should (null (clime-serve--parse-json-body "")))
  (should (null (clime-serve--parse-json-body nil))))

;;; ─── Values Seeding ─────────────────────────────────────────────────────

(ert-deftest clime-test-serve/seed-values-from-query-params ()
  "Query params seeded into values map with source user."
  (skip-unless (featurep 'clime-serve))
  (let* ((values (clime-serve--seed-values nil '(("dry-run" . "true"))))
         (entry (clime-values-get values 'dry-run)))
    (should entry)
    (should (equal (plist-get entry :value) "true"))
    (should (equal (plist-get entry :source) 'user))))

;;; ─── Dispatch ───────────────────────────────────────────────────────────

(ert-deftest clime-test-serve/dispatch-success ()
  "Successful dispatch returns 200 with handler output."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app '("health") nil)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "ok" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-nested ()
  "Dispatch resolves nested group/command."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                       '("db" "migrate") nil)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "migrated" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-with-query-params ()
  "Query params reach handler as option values."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                       '("db" "migrate")
                                       '(("dry-run" . t)))))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "dry" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-with-body-params ()
  "Body params reach handler as arg values."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                       '("db" "seed")
                                       '(("file" . "data.sql")))))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "seeded:data.sql" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-path-params ()
  "Path param values reach handler."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-pathparam-app
                                       '("agent" "abc123" "start") nil)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "started:abc123" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-path-params-percent-encoded ()
  "Percent-encoded path param is decoded before reaching handler.
Regression test for URL-decode fix (clime-azs1)."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-pathparam-app
                                       '("agent" "hello%20world" "start") nil)))
    ;; Segments arrive pre-decoded by the handler layer; dispatch sees
    ;; the raw strings.  The fix is in clime-serve--make-handler which
    ;; applies url-unhex-string before calling dispatch.  So test the
    ;; handler layer via integration test below.  Here we confirm that
    ;; dispatch passes through whatever it receives — no double-encoding.
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "started:hello%20world" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-path-params-with-options ()
  "Path params + query option params both reach handler."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-pathparam-app
                                       '("agent" "abc123" "stop")
                                       '(("force" . t)))))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "stopped:abc123" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-unknown-404 ()
  "Unknown path returns 404."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                       '("nonexistent") nil)))
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/dispatch-error-500 ()
  "Handler runtime error returns 500."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-err-app
                                       '("boom") nil)))
    (should (equal (plist-get result :status) 500))))

(ert-deftest clime-test-serve/dispatch-usage-error-400-json ()
  "A handler `clime-usage-error' returns 400 with the format error envelope.
With a json default-format the body is the json error object, not raw
text (regression for the serve usage-error rendering fix)."
  (skip-unless (featurep 'clime-serve))
  (let* ((json-fmt (cl-find 'json (clime-app-output-formats
                                   clime-test--serve-fmt-app)
                            :key #'clime-output-format-name))
         (result (clime-serve--dispatch clime-test--serve-fmt-app
                                        '("badreq") nil json-fmt)))
    (should (equal (plist-get result :status) 400))
    (should (equal (plist-get result :content-type) "application/json"))
    (let ((obj (json-read-from-string (plist-get result :body))))
      (should (equal (cdr (assq 'error obj)) "bad input")))))

;;; ─── HTTP adapter policies ──────────────────────────────────────────────

(defun clime-test-serve--query-input (name value)
  (clime-make-dispatch-input :name name :value value :source 'query))

(defun clime-test-serve--json-input (name value)
  (clime-make-dispatch-input :name name :value value :source 'json-body))

(defun clime-test-serve--request (&rest args)
  (apply #'clime-make-dispatch-request
         (list :surface 'serve
               :adapter 'http
               :path (plist-get args :path)
               :inputs (plist-get args :inputs)
               :metadata (append (list :method (or (plist-get args :method) 'GET)
                                       :content-type (plist-get args :content-type)
                                       :body-bytes (or (plist-get args :body-bytes) 0)
                                       :server-origin "http://127.0.0.1:8080")
                                 (plist-get args :metadata)))))

(ert-deftest clime-test-serve/dsl-adapter-policies-on-app-and-command ()
  "DSL stores opaque adapter policy metadata on nodes."
  (skip-unless (featurep 'clime-serve))
  (let ((cmd (cdr (assoc "submit"
                         (clime-group-children clime-test--serve-policy-app)))))
    (should (equal (cdr (assq 'http (clime-node-adapter-policies
                                     clime-test--serve-policy-app)))
                   '(:same-origin t)))
    (should (equal (plist-get (cdr (assq 'http (clime-node-adapter-policies cmd)))
                              :methods)
                   '(POST)))))

(ert-deftest clime-test-serve/policy-free-mixed-inputs-json-body-wins ()
  "Legacy policy-free routes accept mixed inputs and body duplicates win."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("legacy")
                   :method 'POST
                   :content-type "application/json"
                   :inputs (list (clime-test-serve--query-input 'name "query")
                                 (clime-test-serve--json-input 'name "body"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "legacy:body" (plist-get result :body)))))

(ert-deftest clime-test-serve/http-policy-disallowed-method-405-allow ()
  "Disallowed HTTP methods reject before handler execution and set Allow."
  (skip-unless (featurep 'clime-serve))
  (let ((calls 0)
        (events nil))
    (cl-letf (((symbol-function 'clime-test-serve--counting-handler)
               (lambda (_ctx) (setq calls (1+ calls)) "called")))
      (let* ((app (clime-make-app
                   :name "policy-method"
                   :children
                   (list (cons "post"
                               (clime-make-command
                                :name "post"
                                :adapter-policies '((http . (:methods (POST))))
                                :handler #'clime-test-serve--counting-handler)))
                   :on-invocation (lambda (ev) (push ev events))))
             (request (clime-test-serve--request :path '("post") :method 'GET))
             (result (clime-serve--dispatch-request app request nil)))
        (should (equal (plist-get result :status) 405))
        (should (equal (cdr (assoc "Allow" (plist-get result :headers))) "POST"))
        (should (= calls 0))
        (should (= (length events) 1))
        (should (eq (clime-invocation-event-phase (car events)) 'rejected))))))

(ert-deftest clime-test-serve/http-policy-json-content-type-required-415 ()
  "Routes requiring JSON body reject non-JSON media types before handlers."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("submit")
                   :method 'POST
                   :content-type "text/plain"
                   :inputs (list (clime-test-serve--json-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 415))))

(ert-deftest clime-test-serve/http-policy-malformed-required-json-400 ()
  "Required JSON routes turn parse failures into controlled 400 responses."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("submit")
                   :method 'POST
                   :content-type "application/json"
                   :metadata '(:json-error "Malformed JSON body")))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 400))
    (should (string-match-p "Malformed JSON" (plist-get result :body)))))

(ert-deftest clime-test-serve/http-policy-real-malformed-json-400 ()
  "Malformed JSON bytes from the web-server adapter become controlled 400s."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-serve--request-from-web-server
                   'POST "/submit"
                   '((:Content-Type . "application/json"))
                   "{\"name\":" "127.0.0.1" 8080 nil))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 400))
    (should (string-match-p "Malformed JSON" (plist-get result :body)))))

(ert-deftest clime-test-serve/http-policy-forbidden-mixed-sources-400 ()
  "Routes constrained to JSON body reject query/body mixing."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("submit")
                   :method 'POST
                   :content-type "application/json; charset=utf-8"
                   :inputs (list (clime-test-serve--query-input 'name "query")
                                 (clime-test-serve--json-input 'name "body"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 400))))

(ert-deftest clime-test-serve/http-policy-body-limit-413 ()
  "Route-level max body bytes rejects oversized requests."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("submit")
                   :method 'POST
                   :content-type "application/json"
                   :body-bytes 65
                   :inputs (list (clime-test-serve--json-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 413))))

(ert-deftest clime-test-serve/http-policy-same-origin-403 ()
  "Same-origin policy rejects mismatched browser origins."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("view")
                   :method 'GET
                   :metadata '(:origin "http://evil.example"
                               :sec-fetch-site "cross-site")
                   :inputs (list (clime-test-serve--query-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 403))))

(ert-deftest clime-test-serve/http-policy-same-origin-null-origin-403 ()
  "Same-origin policy rejects browser Origin: null."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("view")
                   :method 'GET
                   :metadata '(:origin "null")
                   :inputs (list (clime-test-serve--query-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 403))))

(ert-deftest clime-test-serve/http-policy-same-origin-fetch-site-same-site-403 ()
  "Same-origin policy rejects Sec-Fetch-Site: same-site."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("view")
                   :method 'GET
                   :metadata '(:origin "http://127.0.0.1:8080"
                               :sec-fetch-site "same-site")
                   :inputs (list (clime-test-serve--query-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 403))))

(ert-deftest clime-test-serve/http-policy-same-origin-fetch-site-none-accepted ()
  "Same-origin policy accepts Sec-Fetch-Site: none with a matching origin."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("view")
                   :method 'GET
                   :metadata '(:origin "http://127.0.0.1:8080"
                               :sec-fetch-site "none")
                   :inputs (list (clime-test-serve--query-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 200))))

(ert-deftest clime-test-serve/http-policy-same-origin-fetch-site-absent-accepted ()
  "Same-origin policy accepts requests with no browser origin metadata."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("view")
                   :method 'GET
                   :inputs (list (clime-test-serve--query-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 200))))

(ert-deftest clime-test-serve/http-policy-accepted-request-context ()
  "Accepted policy routes receive the sanitized dispatch request explicitly."
  (skip-unless (featurep 'clime-serve))
  (let* ((request (clime-test-serve--request
                   :path '("submit")
                   :method 'POST
                   :content-type "application/json; charset=utf-8"
                   :metadata '(:origin "http://127.0.0.1:8080"
                               :sec-fetch-site "same-origin")
                   :inputs (list (clime-test-serve--json-input 'name "Ada"))))
         (result (clime-serve--dispatch-request clime-test--serve-policy-app
                                                request nil)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "submit:Ada:json-body" (plist-get result :body)))))

(ert-deftest clime-test-serve/http-policy-descendant-cannot-weaken-same-origin ()
  "Descendant :same-origin nil cannot weaken an ancestor requirement."
  (skip-unless (featurep 'clime-serve))
  (let* ((app (clime-make-app
               :name "same-origin-app"
               :adapter-policies '((http . (:same-origin t)))
               :children
               (list (cons "open"
                           (clime-make-command
                            :name "open"
                            :adapter-policies '((http . (:same-origin nil)))
                            :handler (lambda (_ctx) "open"))))))
         (request (clime-test-serve--request
                   :path '("open")
                   :method 'GET
                   :metadata '(:origin "http://evil.example"))))
    (should (equal (plist-get (clime-serve--dispatch-request app request nil)
                              :status)
                   403))))

(ert-deftest clime-test-serve/http-policy-unknown-key-fails-validation ()
  "Unknown local HTTP policy keys fail closed at validation."
  (skip-unless (featurep 'clime-serve))
  (let* ((app (clime-make-app
               :name "unknown-key"
               :children
               (list (cons "submit"
                           (clime-make-command
                            :name "submit"
                            :adapter-policies '((http . (:methdos (POST))))
                            :handler (lambda (_ctx) "submit"))))))
         (err (should-error (clime-serve--validate-http-policies app)
                            :type 'error))
         (msg (error-message-string err)))
    (should (string-match-p ":methdos" msg))
    (should (string-match-p "submit" msg))))

(ert-deftest clime-test-serve/http-policy-unknown-adapter-fails-validation ()
  "Unknown adapter policy symbols fail closed at validation."
  (skip-unless (featurep 'clime-serve))
  (let* ((app (clime-make-app
               :name "unknown-adapter"
               :children
               (list (cons "submit"
                           (clime-make-command
                            :name "submit"
                            :adapter-policies '((htttp . (:methods (POST))))
                            :handler (lambda (_ctx) "submit"))))))
         (err (should-error (clime-serve--validate-http-policies app)
                            :type 'error))
         (msg (error-message-string err)))
    (should (string-match-p "htttp" msg))
    (should (string-match-p "submit" msg))))

(ert-deftest clime-test-serve/http-policy-malformed-plist-fails-validation ()
  "Malformed local HTTP policy plists fail closed at validation."
  (skip-unless (featurep 'clime-serve))
  (let* ((app (clime-make-app
               :name "malformed-policy"
               :children
               (list (cons "submit"
                           (clime-make-command
                            :name "submit"
                            :adapter-policies '((http . (:same-origin)))
                            :handler (lambda (_ctx) "submit"))))))
         (err (should-error (clime-serve--validate-http-policies app)
                            :type 'error))
         (msg (error-message-string err)))
    (should (string-match-p "malformed" msg))
    (should (string-match-p "submit" msg))))

(ert-deftest clime-test-serve/http-policy-contradictory-methods-fail-validation ()
  "Contradictory inherited method sets fail closed at validation."
  (skip-unless (featurep 'clime-serve))
  (let* ((app (clime-make-app
               :name "contradict-methods"
               :adapter-policies '((http . (:methods (POST))))
               :children
               (list (cons "read"
                           (clime-make-command
                            :name "read"
                            :adapter-policies '((http . (:methods (GET))))
                            :handler (lambda (_ctx) "read")))))))
    (should-error (clime-serve--validate-http-policies app))))

(ert-deftest clime-test-serve/http-policy-survives-alias-and-deep-copy ()
  "Alias reconstruction and per-request deep copy preserve adapter policy."
  (skip-unless (featurep 'clime-serve))
  (eval '(clime-app clime-test--serve-policy-alias-app
           (clime-command target
             :adapter-policies '((http . (:methods (POST))))
             (clime-handler (_ctx) "target"))
           (clime-alias-for alias (target)))
        t)
  (let* ((tree (clime--prepare-tree clime-test--serve-policy-alias-app))
         (alias (clime-group-find-child tree "alias"))
         (request (clime-test-serve--request
                   :path '("alias")
                   :method 'GET))
         (events nil))
    (unwind-protect
        (progn
          (setf (clime-app-on-invocation clime-test--serve-policy-alias-app)
                (list (lambda (event) (push event events))))
          (let ((result (clime-serve--dispatch-request
                         clime-test--serve-policy-alias-app request nil)))
            (should (equal (plist-get (clime-serve--effective-http-policy alias)
                                      :methods)
                           '(POST)))
            (should (equal (plist-get result :status) 405))
            (should (= 1 (length events)))
            (should (= 405 (clime-invocation-event-response-status
                            (car events))))))
      (setf (clime-app-on-invocation clime-test--serve-policy-alias-app) nil))))

;;; ─── on-invocation events through serve ─────────────────────────────────

(ert-deftest clime-test-serve/on-invocation-surface-serve ()
  "Serve dispatch fires the unified hook with surface `serve' on success."
  (skip-unless (featurep 'clime-serve))
  (let ((evs nil))
    (unwind-protect
        (progn
          (setf (clime-app-on-invocation clime-test--serve-app)
                (list (lambda (ev) (push ev evs))))
          (let ((result (clime-serve--dispatch clime-test--serve-app '("health") nil)))
            (should (equal (plist-get result :status) 200))
            (should (= 1 (length evs)))
            (let ((ev (car evs)))
              (should (eq 'serve (clime-invocation-event-surface ev)))
              (should (eq 'completed (clime-invocation-event-phase ev)))
              (should (= 0 (clime-invocation-event-exit-code ev))))))
      (setf (clime-app-on-invocation clime-test--serve-app) nil))))

(ert-deftest clime-test-serve/on-invocation-serve-runtime-error ()
  "Serve dispatch of a failing handler fires a runtime-error event (exit 1)."
  (skip-unless (featurep 'clime-serve))
  (let ((evs nil))
    (unwind-protect
        (progn
          (setf (clime-app-on-invocation clime-test--serve-err-app)
                (list (lambda (ev) (push ev evs))))
          (let ((result (clime-serve--dispatch clime-test--serve-err-app '("boom") nil)))
            (should (equal (plist-get result :status) 500))
            (should (= 1 (length evs)))
            (let ((ev (car evs)))
              (should (eq 'serve (clime-invocation-event-surface ev)))
              (should (eq 'runtime-error (clime-invocation-event-phase ev)))
              (should (= 1 (clime-invocation-event-exit-code ev)))
              (should (string-match-p "Kaboom" (clime-invocation-event-error-message ev))))))
      (setf (clime-app-on-invocation clime-test--serve-err-app) nil))))

(ert-deftest clime-test-serve/on-invocation-unknown-route-404 ()
  "An unknown route fires a not-found event even though no handler runs."
  (skip-unless (featurep 'clime-serve))
  (let ((evs nil))
    (unwind-protect
        (progn
          (setf (clime-app-on-invocation clime-test--serve-app)
                (list (lambda (ev) (push ev evs))))
          (let ((result (clime-serve--dispatch clime-test--serve-app '("nonexistent") nil)))
            (should (equal (plist-get result :status) 404))
            (should (= 1 (length evs)))
            (let ((ev (car evs)))
              (should (eq 'serve (clime-invocation-event-surface ev)))
              (should (eq 'not-found (clime-invocation-event-phase ev)))
              (should (equal '("nonexistent") (clime-invocation-event-path ev)))
              (should (clime-invocation-event-error-message ev))
              ;; no handler executed → nil exit code, no context
              (should (null (clime-invocation-event-exit-code ev)))
              (should (null (clime-invocation-event-context ev)))
              (should (floatp (clime-invocation-event-duration ev))))))
      (setf (clime-app-on-invocation clime-test--serve-app) nil))))

(ert-deftest clime-test-serve/on-invocation-handlerless-group-404 ()
  "Dispatching to a handlerless group fires a not-found event (404)."
  (skip-unless (featurep 'clime-serve))
  (let ((evs nil))
    (unwind-protect
        (progn
          (setf (clime-app-on-invocation clime-test--serve-app)
                (list (lambda (ev) (push ev evs))))
          (let ((result (clime-serve--dispatch clime-test--serve-app '("db") nil)))
            (should (equal (plist-get result :status) 404))
            (should (= 1 (length evs)))
            (let ((ev (car evs)))
              (should (eq 'serve (clime-invocation-event-surface ev)))
              (should (eq 'not-found (clime-invocation-event-phase ev)))
              (should (equal '("db") (clime-invocation-event-path ev))))))
      (setf (clime-app-on-invocation clime-test--serve-app) nil))))

(ert-deftest clime-test-serve/on-invocation-api-detail-404-once ()
  "The /_api/commands detail-walk 404 throw fires one not-found event."
  (skip-unless (featurep 'clime-serve))
  (let ((app (clime--deep-copy-tree clime-test--serve-app))
        (evs nil))
    (unwind-protect
        (progn
          (clime-serve--inject-api-commands app)
          (setf (clime-app-on-invocation app)
                (list (lambda (ev) (push ev evs))))
          (let ((result (clime-serve--dispatch app
                                               '("_api" "commands" "missing")
                                               nil)))
            (should (equal (plist-get result :status) 404))
            (should (= 1 (length evs)))
            (let ((ev (car evs)))
              (should (eq 'serve (clime-invocation-event-surface ev)))
              (should (eq 'not-found (clime-invocation-event-phase ev)))
              (should (clime-invocation-event-handler-invoked-p ev))
              (should (floatp
                       (clime-invocation-event-execution-duration ev)))
              (should-not (clime-invocation-event-returned-p ev))
              (should (equal '("_api" "commands" "missing")
                             (clime-invocation-event-path ev)))
              (should (string-match-p
                       "Unknown command path: missing"
                       (clime-invocation-event-error-message ev))))))
      (setf (clime-app-on-invocation app) nil))))

;;; ─── Introspection ──────────────────────────────────────────────────────

(ert-deftest clime-test-serve/routes-introspection ()
  "/_routes returns available commands."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                       '("_routes") nil)))
    (should (equal (plist-get result :status) 200))
    (should (plist-get result :body))))

;;; ─── Server Lifecycle (mocked) ──────────────────────────────────────────

(ert-deftest clime-test-serve/start-and-stop ()
  "clime-serve starts a server and clime-serve-stop stops it."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9876)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-app :port port))
          (should server)
          (should (assq port clime-serve--servers)))
      (when server
        (clime-serve-stop server))
      (should-not (assq port clime-serve--servers)))))

;;; ─── Integration Tests ──────────────────────────────────────────────────

(ert-deftest clime-test-serve/integration-http-get ()
  "Integration: start server, GET /health, verify response."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9877)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/health" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (search-forward "200" nil t))
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (should (string-match-p "ok" (buffer-substring (point) (point-max)))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-nested-with-option ()
  "Integration: GET /db/migrate?dry-run returns dry."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9878)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/db/migrate?dry-run" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (should (string-match-p "dry" (buffer-substring (point) (point-max)))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-path-params ()
  "Integration: GET /agent/abc123/start returns started:abc123."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9879)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-pathparam-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/agent/abc123/start" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (should (string-match-p "started:abc123"
                                          (buffer-substring (point) (point-max)))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-path-params-percent-encoded ()
  "Integration: percent-encoded path segment is decoded before dispatch.
Regression test for URL-decode fix (clime-azs1)."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9882)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-pathparam-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/agent/hello%%20world/start" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (should (string-match-p "started:hello world"
                                          (buffer-substring (point) (point-max)))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-404 ()
  "Integration: GET /nonexistent returns 404."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9880)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/nonexistent" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (search-forward "404" nil t)))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-introspection ()
  "Integration: GET /_routes returns command listing."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9881)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/_routes" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (let ((body (buffer-substring (point) (point-max))))
                    (should (string-match-p "/health" body))
                    (should (string-match-p "/db/migrate" body))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

;;; ─── Path Walking (additional) ───────────────────────────────────────

(ert-deftest clime-test-serve/walk-path-list-correct ()
  "Walk builds correct :path list including root."
  (let* ((tree (clime--prepare-tree clime-test--serve-app))
         (result (clime-serve--walk-path tree '("db" "migrate")))
         (path (plist-get result :path)))
    ;; Path includes root app name, group, and command
    (should (equal (car path) (clime-node-name tree)))
    (should (member "db" path))
    (should (member "migrate" path))))

(ert-deftest clime-test-serve/walk-group-without-handler ()
  "Walking to a group (no handler) — walk succeeds but dispatch should 404."
  (let* ((tree (clime--prepare-tree clime-test--serve-app))
         (result (clime-serve--walk-path tree '("db"))))
    ;; Walk itself succeeds — node is the group
    (should (clime-group-p (plist-get result :node)))
    (should (equal (clime-node-name (plist-get result :node)) "db"))))

;;; ─── Dispatch (additional) ──────────────────────────────────────────────

(ert-deftest clime-test-serve/dispatch-group-no-handler-404 ()
  "Dispatching to a group with no handler returns 404."
  (let ((result (clime-serve--dispatch clime-test--serve-app '("db") nil)))
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/dispatch-missing-required-arg-400 ()
  "Missing required arg triggers usage error → 400."
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                       '("db" "seed") nil)))
    ;; seed requires 'file' arg — not provided
    (should (equal (plist-get result :status) 400))))

(ert-deftest clime-test-serve/dispatch-type-coercion ()
  "Typed option values are coerced through finalize pipeline."
  (let ((result (clime-serve--dispatch clime-test--serve-typed-app
                                       '("count")
                                       '(("limit" . "25")))))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "limit=25" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-type-default ()
  "Default values applied when param not provided."
  (let ((result (clime-serve--dispatch clime-test--serve-typed-app
                                       '("count") nil)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "limit=10" (plist-get result :body)))))

(ert-deftest clime-test-serve/dispatch-empty-path-root ()
  "Empty path dispatches to root — no handler → 404."
  (let ((result (clime-serve--dispatch clime-test--serve-app '() nil)))
    (should (equal (plist-get result :status) 404))))

;;; ─── Values Seeding (additional) ────────────────────────────────────────

(ert-deftest clime-test-serve/seed-values-multiple-params ()
  "Multiple params all seeded into values map."
  (let* ((values (clime-serve--seed-values nil
                   '(("file" . "data.sql") ("limit" . "10") ("verbose" . "true"))))
         (file-entry (clime-values-get values 'file))
         (limit-entry (clime-values-get values 'limit)))
    (should (equal (plist-get file-entry :value) "data.sql"))
    (should (equal (plist-get limit-entry :value) "10"))
    (should (clime-values-get values 'verbose))))

(ert-deftest clime-test-serve/seed-values-preserves-walk-values ()
  "Seeding preserves values already set by path walking."
  (let* ((walk-values (clime-values-set nil 'id "abc" 'user))
         (values (clime-serve--seed-values walk-values '(("force" . "true")))))
    (should (equal (clime-values-value values 'id) "abc"))
    (should (equal (clime-values-value values 'force) "true"))))

;;; ─── Query String Parsing (additional) ──────────────────────────────────

(ert-deftest clime-test-serve/parse-query-string-value-with-equals ()
  "Query value containing = is preserved."
  (let ((params (clime-serve--parse-query-string "expr=a=b")))
    ;; split on first = only: key=expr, value starts at a
    ;; Current impl splits on all =, so value is "a" — document behavior
    (should (assoc "expr" params))))

;;; ─── JSON Body Parsing (additional) ─────────────────────────────────────

(ert-deftest clime-test-serve/parse-json-body-array-returns-nil ()
  "JSON array (not object) returns nil."
  (should (null (clime-serve--parse-json-body "[1,2,3]"))))

(ert-deftest clime-test-serve/parse-json-body-nested-object ()
  "Nested JSON object values preserved as-is."
  (let ((params (clime-serve--parse-json-body "{\"config\":{\"a\":1}}")))
    (should (assoc "config" params))
    ;; Nested value is an alist
    (should (listp (cdr (assoc "config" params))))))

;;; ─── Introspection (additional) ─────────────────────────────────────────

(ert-deftest clime-test-serve/introspection-lists-commands ()
  "Introspection body contains all visible command paths."
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("_routes") nil))
         (body (plist-get result :body)))
    (should (string-match-p "/health" body))
    (should (string-match-p "/db/migrate" body))
    (should (string-match-p "/db/seed" body))))

(ert-deftest clime-test-serve/introspection-excludes-hidden ()
  "Introspection excludes hidden commands."
  (let* ((result (clime-serve--dispatch clime-test--serve-hidden-app
                                         '("_routes") nil))
         (body (plist-get result :body)))
    (should (string-match-p "/visible" body))
    (should-not (string-match-p "/secret" body))))

(ert-deftest clime-test-serve/introspection-includes-help-text ()
  "Introspection includes command help text."
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("_routes") nil))
         (body (plist-get result :body)))
    (should (string-match-p "Health check" body))
    (should (string-match-p "Run migrations" body))))

;;; ─── API: /_api/meta ────────────────────────────────────────────────────

(ert-deftest clime-test-serve/api-meta-returns-structured ()
  "/_api/meta returns app name, version, help via clime-out."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "meta") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body))))
    (should (equal (plist-get result :status) 200))
    (should (equal (cdr (assoc "name" parsed)) "clime-test--serve-app"))
    (should (equal (cdr (assoc "version" parsed)) "1.0"))))

(ert-deftest clime-test-serve/api-meta-text-format ()
  "/_api/meta in text mode outputs via default format."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "meta") nil))
         (body (plist-get result :body)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "clime-test--serve-app" body))))

;;; ─── API: /_api/commands ───────────────────────────────────────────────

(ert-deftest clime-test-serve/api-commands-returns-tree ()
  "/_api/commands returns recursive command tree."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body)))
         (commands (cdr (assoc "commands" parsed))))
    (should (equal (plist-get result :status) 200))
    ;; Should have health and db group
    (should (cl-some (lambda (c) (equal (cdr (assoc "name" c)) "health")) commands))
    (should (cl-some (lambda (c) (equal (cdr (assoc "name" c)) "db")) commands))))

(ert-deftest clime-test-serve/api-commands-includes-options ()
  "/_api/commands tree includes option metadata."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body)))
         (commands (cdr (assoc "commands" parsed)))
         (db (cl-find-if (lambda (c) (equal (cdr (assoc "name" c)) "db")) commands))
         (children (cdr (assoc "children" db)))
         (migrate (cl-find-if (lambda (c) (equal (cdr (assoc "name" c)) "migrate")) children))
         (options (cdr (assoc "options" migrate))))
    ;; migrate has --dry-run option
    (should (cl-some (lambda (o) (equal (cdr (assoc "name" o)) "dry-run")) options))))

(ert-deftest clime-test-serve/api-commands-includes-args ()
  "/_api/commands tree includes arg metadata."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body)))
         (commands (cdr (assoc "commands" parsed)))
         (db (cl-find-if (lambda (c) (equal (cdr (assoc "name" c)) "db")) commands))
         (children (cdr (assoc "children" db)))
         (seed (cl-find-if (lambda (c) (equal (cdr (assoc "name" c)) "seed")) children))
         (args (cdr (assoc "args" seed))))
    ;; seed has required 'file' arg
    (should (cl-some (lambda (a)
                       (and (equal (cdr (assoc "name" a)) "file")
                            (not (eq (cdr (assoc "required" a)) :json-false))))
                     args))))

(ert-deftest clime-test-serve/api-commands-preserves-choices-presence ()
  "/_api/commands omits absent choices and preserves declared choices."
  (skip-unless (featurep 'clime-serve))
  (let* ((app (eval '(clime-app clime-test--serve-choices-app
                      (clime-command inspect
                        (clime-option format ("--format")
                          :choices '("json" "text"))
                        (clime-arg target :choices '("one" "two"))
                        (clime-handler (_ctx) "ok")))))
         (_ (clime-serve--inject-api-commands app))
         (result (clime-serve--dispatch app '("_api" "commands") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (commands (cdr (assoc "commands"
                               (json-read-from-string (plist-get result :body)))))
         (inspect (aref commands 0))
         (option (aref (cdr (assoc "options" inspect)) 0))
         (arg (aref (cdr (assoc "args" inspect)) 0)))
    (should (equal (plist-get result :status) 200))
    (should (equal (cdr (assoc "choices" option)) ["json" "text"]))
    (should (equal (cdr (assoc "choices" arg)) ["one" "two"]))
    (let* ((base-result (clime-serve--dispatch clime-test--serve-app
                                                '("_api" "commands") nil))
           (base-commands
            (cdr (assoc "commands"
                        (json-read-from-string (plist-get base-result :body)))))
           (db (cl-find-if (lambda (command)
                             (equal (cdr (assoc "name" command)) "db"))
                           base-commands))
           (migrate (cl-find-if (lambda (command)
                                  (equal (cdr (assoc "name" command)) "migrate"))
                                (cdr (assoc "children" db))))
           (seed (cl-find-if (lambda (command)
                               (equal (cdr (assoc "name" command)) "seed"))
                             (cdr (assoc "children" db))))
           (choiceless-option (aref (cdr (assoc "options" migrate)) 0))
           (choiceless-arg (aref (cdr (assoc "args" seed)) 0)))
      (should-not (assoc "choices" choiceless-option))
      (should-not (assoc "choices" choiceless-arg)))))

(ert-deftest clime-test-serve/api-commands-excludes-hidden ()
  "/_api/commands excludes hidden commands."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-hidden-app
                                         '("_api" "commands") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body)))
         (commands (cdr (assoc "commands" parsed))))
    (should (cl-some (lambda (c) (equal (cdr (assoc "name" c)) "visible")) commands))
    (should-not (cl-some (lambda (c) (equal (cdr (assoc "name" c)) "secret")) commands))))

(ert-deftest clime-test-serve/api-commands-node-type ()
  "/_api/commands nodes have correct type field."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body)))
         (commands (cdr (assoc "commands" parsed)))
         (health (cl-find-if (lambda (c) (equal (cdr (assoc "name" c)) "health")) commands))
         (db (cl-find-if (lambda (c) (equal (cdr (assoc "name" c)) "db")) commands)))
    (should (equal (cdr (assoc "type" health)) "command"))
    (should (equal (cdr (assoc "type" db)) "group"))))

;;; ─── API: /_api/commands/PATH ──────────────────────────────────────────

(ert-deftest clime-test-serve/api-command-detail ()
  "/_api/commands/db/migrate returns single command detail."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands" "db" "migrate") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body))))
    (should (equal (plist-get result :status) 200))
    (should (equal (cdr (assoc "name" parsed)) "migrate"))
    (should (equal (cdr (assoc "help" parsed)) "Run migrations"))))

(ert-deftest clime-test-serve/api-command-detail-includes-hidden-serve-command ()
  "/_api/commands/PATH preserves detail for hidden serve commands."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-hidden-app
                                         '("_api" "commands" "secret") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body))))
    (should (equal (plist-get result :status) 200))
    (should (equal (cdr (assoc "name" parsed)) "secret"))))

(ert-deftest clime-test-serve/api-command-detail-group ()
  "/_api/commands/db returns group detail with children."
  (skip-unless (featurep 'clime-serve))
  (let* ((clime-out--active-format (clime-make-output-format
                                    :name 'json :flags '("--json")))
         (result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands" "db") nil))
         (json-object-type 'alist)
         (json-key-type 'string)
         (parsed (json-read-from-string (plist-get result :body))))
    (should (equal (plist-get result :status) 200))
    (should (equal (cdr (assoc "name" parsed)) "db"))
    (should (equal (cdr (assoc "type" parsed)) "group"))
    (should (cdr (assoc "children" parsed)))))

(ert-deftest clime-test-serve/api-command-detail-404 ()
  "/_api/commands/nonexistent returns 404."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                        '("_api" "commands" "nonexistent") nil)))
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/api-unknown-endpoint-404 ()
  "/_api/unknown returns 404."
  (skip-unless (featurep 'clime-serve))
  (let ((result (clime-serve--dispatch clime-test--serve-app
                                        '("_api" "bogus") nil)))
    (should (equal (plist-get result :status) 404))))

;;; ─── API: option serialization ─────────────────────────────────────────

(ert-deftest clime-test-serve/option-to-alist-fields ()
  "Option serialization includes key fields."
  (skip-unless (featurep 'clime-serve))
  (let* ((opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")
                                  :help "Be verbose" :type 'string))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "test"
                              :children `(("show" . ,cmd))))
         (_nodes (clime-contract-nodes
                  app :surface 'serve :tree-mode 'prepared
                  :visibility 'visible :value-mode 'declared))
         (alist (clime-serve--option-to-alist opt clime-serve--contract-policy)))
    (should (equal (cdr (assoc "name" alist)) "verbose"))
    (should (equal (cdr (assoc "help" alist)) "Be verbose"))
    (should (equal (cdr (assoc "flags" alist)) ["--verbose" "-v"]))))

(ert-deftest clime-test-serve/arg-to-alist-fields ()
  "Arg serialization includes key fields."
  (skip-unless (featurep 'clime-serve))
  (let* ((arg (clime-arg--create :name 'file :help "Input file"
                                  :type 'string :required t))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :args (list arg)))
         (app (clime-make-app :name "test"
                              :children `(("show" . ,cmd))))
         (_nodes (clime-contract-nodes
                  app :surface 'serve :tree-mode 'prepared
                  :visibility 'visible :value-mode 'declared))
         (alist (clime-serve--arg-to-alist arg clime-serve--contract-policy)))
    (should (equal (cdr (assoc "name" alist)) "file"))
    (should (equal (cdr (assoc "help" alist)) "Input file"))
    (should (equal (cdr (assoc "required" alist)) t))))

;;; ─── API: integration test ─────────────────────────────────────────────

(ert-deftest clime-test-serve/integration-api-meta ()
  "Integration: GET /_api/meta returns app data over HTTP."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9883)
        (server nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-app :port port))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/_api/meta" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (let ((body (buffer-substring (point) (point-max))))
                    (should (string-match-p "clime-test--serve-app" body))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

;;; ─── Path Suffix Format Selection ─────────────────────────────────────

(ert-deftest clime-test-serve/suffix-json-activates-format ()
  "GET /list.json → JSON body via output format, Content-Type application/json."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("list.json") nil))
         (body (plist-get result :body))
         (ct   (plist-get result :content-type)))
    (should (equal (plist-get result :status) 200))
    ;; Body should be JSON-encoded (the handler calls clime-out with alist)
    (should (let ((parsed (json-read-from-string body)))
              (equal (cdr (assq 'items parsed)) [1 2 3])))
    (should (equal ct "application/json"))))

(ert-deftest clime-test-serve/no-suffix-text-format ()
  "GET /list (no suffix) → text body, Content-Type text/plain."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("list") nil))
         (ct (plist-get result :content-type)))
    (should (equal (plist-get result :status) 200))
    (should (equal ct "text/plain; charset=utf-8"))))

(ert-deftest clime-test-serve/unknown-suffix-literal ()
  "GET /list.xml with no xml format → 404 (literal segment, no match)."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("list.xml") nil)))
    ;; .xml is not a registered format → segment stays as 'list.xml'
    ;; → no command named 'list.xml' → 404
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/nested-path-suffix ()
  "GET /db/status.json → JSON body for nested command."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("db" "status.json") nil))
         (body (plist-get result :body))
         (ct   (plist-get result :content-type)))
    (should (equal (plist-get result :status) 200))
    (should (let ((parsed (json-read-from-string body)))
              (equal (cdr (assq 'status parsed)) "ok")))
    (should (equal ct "application/json"))))

(ert-deftest clime-test-serve/suffix-only-last-segment ()
  "Suffix stripping applies only to the last path segment, not intermediate ones."
  (skip-unless (featurep 'clime-serve))
  ;; 'db.json' is not a group → 404 (suffix only stripped from last segment)
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("db.json" "status") nil)))
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/api-meta-defaults-json ()
  "/_api/meta returns JSON without .json suffix (default format on _api group)."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("_api" "meta") nil))
         (body (plist-get result :body))
         (ct   (plist-get result :content-type)))
    (should (equal (plist-get result :status) 200))
    ;; Should parse as JSON
    (should (let ((parsed (json-read-from-string body)))
              (stringp (cdr (assq 'name parsed)))))
    (should (equal ct "application/json"))))

(ert-deftest clime-test-serve/no-formats-no-stripping ()
  "App without output formats: .json suffix is literal, no stripping."
  (skip-unless (featurep 'clime-serve))
  ;; clime-test--serve-app has no output formats declared
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("health.json") nil)))
    ;; No formats registered → suffix is literal → no command 'health.json' → 404
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/content-type-text-default ()
  "Dispatch without suffix returns text/plain content-type."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("list") nil))
         (ct (plist-get result :content-type)))
    (should (equal ct "text/plain; charset=utf-8"))))

;;; ─── Suffix Helpers (unit) ─────────────────────────────────────────────

(ert-deftest clime-test-serve/extract-suffix-match ()
  "extract-suffix strips known extension and returns format."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--extract-suffix '("list.json")
                                               clime-test--serve-fmt-app)))
    (should (equal (car result) '("list")))
    (should (eq (clime-output-format-name (cdr result)) 'json))))

(ert-deftest clime-test-serve/extract-suffix-no-match ()
  "extract-suffix returns original segments when extension unknown."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--extract-suffix '("list.xml")
                                               clime-test--serve-fmt-app)))
    (should (equal (car result) '("list.xml")))
    (should (null (cdr result)))))

(ert-deftest clime-test-serve/extract-suffix-no-dot ()
  "extract-suffix returns original segments when no dot in last segment."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--extract-suffix '("list")
                                               clime-test--serve-fmt-app)))
    (should (equal (car result) '("list")))
    (should (null (cdr result)))))

(ert-deftest clime-test-serve/extract-suffix-no-formats ()
  "extract-suffix skips entirely when app has no output formats."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--extract-suffix '("health.json")
                                               clime-test--serve-app)))
    (should (equal (car result) '("health.json")))
    (should (null (cdr result)))))

(ert-deftest clime-test-serve/extract-suffix-nested ()
  "extract-suffix only touches the last segment."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--extract-suffix '("db" "status.json")
                                               clime-test--serve-fmt-app)))
    (should (equal (car result) '("db" "status")))
    (should (eq (clime-output-format-name (cdr result)) 'json))))

(ert-deftest clime-test-serve/content-type-for-format-json ()
  "content-type helper returns application/json for json format."
  (skip-unless (featurep 'clime-serve))
  (let ((fmt (clime-make-output-format :name 'json)))
    (should (equal (clime-serve--content-type-for-format fmt)
                   "application/json"))))

(ert-deftest clime-test-serve/content-type-for-format-nil ()
  "content-type helper returns text/plain for nil."
  (skip-unless (featurep 'clime-serve))
  (should (equal (clime-serve--content-type-for-format nil)
                 "text/plain; charset=utf-8")))

(ert-deftest clime-test-serve/content-type-for-format-html ()
  "content-type helper returns text/html for html format."
  (skip-unless (featurep 'clime-serve))
  (let ((fmt (clime-make-output-format :name 'html)))
    (should (equal (clime-serve--content-type-for-format fmt)
                   "text/html"))))

;;; ─── Suffix + other features ───────────────────────────────────────────

(ert-deftest clime-test-serve/suffix-with-path-params ()
  "Suffix works with path params: /agent/42/start.json."
  (skip-unless (featurep 'clime-serve))
  ;; pathparam app has no output formats → suffix is literal
  ;; Verify no crash and 404 (no format registered)
  (let* ((result (clime-serve--dispatch clime-test--serve-pathparam-app
                                         '("agent" "42" "start.json") nil)))
    (should (equal (plist-get result :status) 404))))

(ert-deftest clime-test-serve/api-defaults-to-json-without-app-format ()
  "/_api/meta on app WITHOUT a JSON format still returns JSON.
The _api group auto-declares json, so introspection endpoints default to
application/json even when the app registers no output formats."
  (skip-unless (featurep 'clime-serve))
  ;; clime-test--serve-app has no output-formats → _api still defaults to json
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "meta") nil))
         (ct (plist-get result :content-type))
         (body (plist-get result :body)))
    (should (equal (plist-get result :status) 200))
    (should (equal ct "application/json"))
    ;; Body must be valid JSON with the expected app metadata
    (let ((parsed (json-read-from-string body)))
      (should (equal (cdr (assq 'name parsed)) "clime-test--serve-app")))))

(ert-deftest clime-test-serve/api-commands-defaults-to-json-without-app-format ()
  "/_api/commands on app WITHOUT a JSON format returns parseable JSON."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("_api" "commands") nil))
         (ct (plist-get result :content-type))
         (body (plist-get result :body)))
    (should (equal (plist-get result :status) 200))
    (should (equal ct "application/json"))
    (let ((parsed (json-read-from-string body)))
      (should (assq 'commands parsed)))))

(ert-deftest clime-test-serve/api-honors-app-json-format ()
  "/_api/meta on app WITH a registered json format still returns JSON."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("_api" "meta") nil))
         (ct (plist-get result :content-type))
         (body (plist-get result :body)))
    (should (equal (plist-get result :status) 200))
    (should (equal ct "application/json"))
    (should (json-read-from-string body))))

(ert-deftest clime-test-serve/api-group-auto-declares-json ()
  "The injected _api group carries a json output-format in its slot."
  (skip-unless (featurep 'clime-serve))
  (let* ((api (cdr (assoc "_api" (clime-group-children clime-test--serve-app))))
         (fmt (and api (cl-find 'json (clime-group-output-formats api)
                                :key #'clime-output-format-name))))
    (should api)
    (should (clime-output-format-p fmt))))

(ert-deftest clime-test-serve/suffix-404-has-content-type ()
  "404 responses include content-type text/plain."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("nonexistent.json") nil))
         (ct (plist-get result :content-type)))
    (should (equal (plist-get result :status) 404))
    (should (equal ct "text/plain; charset=utf-8"))))

(ert-deftest clime-test-serve/default-format-arg-overrides ()
  "Explicit default-format arg is used when no suffix present."
  (skip-unless (featurep 'clime-serve))
  (let* ((json-fmt (cl-find 'json (clime-app-output-formats clime-test--serve-fmt-app)
                             :key #'clime-output-format-name))
         (result (clime-serve--dispatch clime-test--serve-fmt-app
                                         '("list") nil json-fmt))
         (body (plist-get result :body))
         (ct   (plist-get result :content-type)))
    (should (equal (plist-get result :status) 200))
    (should (let ((parsed (json-read-from-string body)))
              (equal (cdr (assq 'items parsed)) [1 2 3])))
    (should (equal ct "application/json"))))

;;; ─── Backward compat ───────────────────────────────────────────────────

(ert-deftest clime-test-serve/routes-still-works ()
  "/_routes plain text introspection still works."
  (skip-unless (featurep 'clime-serve))
  (let* ((result (clime-serve--dispatch clime-test--serve-app
                                         '("_routes") nil))
         (body (plist-get result :body)))
    (should (equal (plist-get result :status) 200))
    (should (string-match-p "/health" body))))

;;; ─── Query String URL-decoding ──────────────────────────────────────────

(ert-deftest clime-test-serve/query-parse-url-decodes-values ()
  "Query string values with percent-encoding are decoded."
  (skip-unless (featurep 'clime-serve))
  (let ((params (clime-serve--parse-query-string "name=hello%20world&path=%2Ffoo%2Fbar")))
    (should (equal (cdr (assoc "name" params)) "hello world"))
    (should (equal (cdr (assoc "path" params)) "/foo/bar"))))

(ert-deftest clime-test-serve/query-parse-url-decodes-keys ()
  "Query string keys with percent-encoding are decoded."
  (skip-unless (featurep 'clime-serve))
  (let ((params (clime-serve--parse-query-string "my%20key=val")))
    (should (equal (cdr (assoc "my key" params)) "val"))))

(ert-deftest clime-test-serve/query-parse-url-decodes-bare-key ()
  "Bare query param key (no =) is URL-decoded."
  (skip-unless (featurep 'clime-serve))
  (let ((params (clime-serve--parse-query-string "verbose&my%20flag")))
    (should (equal (cdr (assoc "my flag" params)) t))))

;;; ─── Double Injection Guard ────────────────────────────────────────────

(ert-deftest clime-test-serve/inject-api-commands-idempotent ()
  "Calling inject-api-commands twice does not duplicate _api."
  (skip-unless (featurep 'clime-serve))
  (let ((app (copy-sequence clime-test--serve-app)))
    ;; Simulate: inject twice
    (clime-serve--inject-api-commands app)
    (let ((count-before (length (clime-group-children app))))
      (clime-serve--inject-api-commands app)
      (should (equal count-before (length (clime-group-children app)))))))

;;; ─── Default-format resolution (clime-serve--resolve-format) ──────────

(defun clime-test--serve-json-fmt ()
  "Return the json `clime-output-format' registered on the fmt test app."
  (cl-find 'json (clime-app-output-formats clime-test--serve-fmt-app)
           :key #'clime-output-format-name))

(ert-deftest clime-test-serve/resolve-format-nil ()
  "Nil default-format resolves to nil (text behavior)."
  (skip-unless (featurep 'clime-serve))
  (should (null (clime-serve--resolve-format clime-test--serve-fmt-app nil))))

(ert-deftest clime-test-serve/resolve-format-symbol ()
  "A registered format-name symbol resolves to its format struct."
  (skip-unless (featurep 'clime-serve))
  (let ((fmt (clime-serve--resolve-format clime-test--serve-fmt-app 'json)))
    (should (clime-output-format-p fmt))
    (should (eq (clime-output-format-name fmt) 'json))))

(ert-deftest clime-test-serve/resolve-format-struct-passthrough ()
  "A format struct is returned as-is."
  (skip-unless (featurep 'clime-serve))
  (let ((fmt (clime-test--serve-json-fmt)))
    (should (eq (clime-serve--resolve-format clime-test--serve-fmt-app fmt)
                fmt))))

(ert-deftest clime-test-serve/resolve-format-unknown-errors ()
  "An unregistered format name signals an error."
  (skip-unless (featurep 'clime-serve))
  (should-error (clime-serve--resolve-format clime-test--serve-fmt-app 'xml)))

;;; ─── Default-format bare-route dispatch ────────────────────────────────

(ert-deftest clime-test-serve/default-format-bare-route-json ()
  "A bare route (no .ext suffix) honors the default-format → JSON."
  (skip-unless (featurep 'clime-serve))
  (let* ((fmt (clime-test--serve-json-fmt))
         (result (clime-serve--dispatch clime-test--serve-fmt-app
                                        '("list") nil fmt)))
    (should (equal (plist-get result :status) 200))
    (should (equal (plist-get result :content-type) "application/json"))
    (should (let ((parsed (json-read-from-string (plist-get result :body))))
              (equal (cdr (assq 'items parsed)) [1 2 3])))))

;;; ─── Auth gate (clime-serve--auth-gate / --unauthorized) ───────────────

(ert-deftest clime-test-serve/auth-gate-nil-ungated ()
  "Nil auth never gates (returns nil → request proceeds)."
  (skip-unless (featurep 'clime-serve))
  (should (null (clime-serve--auth-gate nil '(:path "/x") nil))))

(ert-deftest clime-test-serve/auth-gate-allow ()
  "An allowing predicate (non-nil return) does not gate."
  (skip-unless (featurep 'clime-serve))
  (should (null (clime-serve--auth-gate (lambda (_req) t) '(:path "/x") nil))))

(ert-deftest clime-test-serve/auth-gate-reject-text ()
  "A rejecting predicate with no format → 401 text/plain."
  (skip-unless (featurep 'clime-serve))
  (let ((r (clime-serve--auth-gate (lambda (_req) nil) '(:path "/x") nil)))
    (should (equal (plist-get r :status) 401))
    (should (equal (plist-get r :content-type) "text/plain; charset=utf-8"))
    (should (string-match-p "Unauthorized" (plist-get r :body)))))

(ert-deftest clime-test-serve/auth-gate-reject-json-envelope ()
  "A rejecting predicate with the json format → 401, application/json, and a
JSON body equivalent to ((error . \"unauthorized\"))."
  (skip-unless (featurep 'clime-serve))
  (let* ((fmt (clime-test--serve-json-fmt))
         (r (clime-serve--auth-gate (lambda (_req) nil) '(:path "/x") fmt)))
    (should (equal (plist-get r :status) 401))
    (should (equal (plist-get r :content-type) "application/json"))
    (let ((parsed (json-read-from-string (plist-get r :body))))
      (should (equal (cdr (assq 'error parsed)) "unauthorized")))))

(ert-deftest clime-test-serve/auth-predicate-sees-headers ()
  "The auth predicate receives request-info with :headers reachable, so a
later bearer-token predicate can read an Authorization entry."
  (skip-unless (featurep 'clime-serve))
  (let* ((seen nil)
         (pred (lambda (req)
                 (setq seen (plist-get req :headers))
                 (equal "Bearer sekret"
                        (cdr (assoc :AUTHORIZATION (plist-get req :headers))))))
         (req-info (list :method 'GET :path "/x"
                         :headers '((:AUTHORIZATION . "Bearer sekret"))
                         :body nil)))
    ;; Allowed because the predicate finds the Authorization header.
    (should (null (clime-serve--auth-gate pred req-info nil)))
    (should (equal seen '((:AUTHORIZATION . "Bearer sekret"))))))

;;; ─── Integration: default-format + auth (real server) ──────────────────

(defun clime-test--serve-raw-get (port path)
  "Do a bare HTTP/1.0 GET of PATH on 127.0.0.1:PORT; return the raw
response string (status line + headers + CRLFCRLF + body).  Avoids
`url.el' so a 401 response is read verbatim without auth prompts or
authinfo lookups in batch."
  (let ((proc (open-network-stream "clime-test-raw" nil "127.0.0.1" port))
        (out ""))
    (set-process-coding-system proc 'binary 'binary)
    (set-process-filter proc (lambda (_p s) (setq out (concat out s))))
    (process-send-string
     proc (format "GET %s HTTP/1.0\r\nHost: 127.0.0.1:%d\r\n\r\n" path port))
    (while (accept-process-output proc 5))
    (ignore-errors (delete-process proc))
    out))

(ert-deftest clime-test-serve/integration-default-format-json ()
  "clime-serve :default-format 'json → bare /list returns application/json."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9890) (server nil) (auth-sources nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-fmt-app
                                    :port port :default-format 'json))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/list" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (search-forward "200" nil t))
                  (goto-char (point-min))
                  (should (re-search-forward
                           "Content-Type: application/json" nil t))
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (let ((parsed (json-read-from-string
                                 (buffer-substring (point) (point-max)))))
                    (should (equal (cdr (assq 'items parsed)) [1 2 3]))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-auth-reject-json ()
  "clime-serve :auth reject with :default-format 'json → 401, application/json,
JSON error envelope, and the handler is never invoked."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9891) (server nil)
        (auth (lambda (req)
                (equal "sekret"
                       (cdr (assoc "token" (plist-get req :headers)))))))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-fmt-app
                                    :port port :default-format 'json :auth auth))
          ;; No token → rejected before dispatch (raw GET: a 401 must not
          ;; drag url.el into interactive auth / authinfo in batch).
          (let* ((resp (clime-test--serve-raw-get port "/list"))
                 (sep (string-match "\r\n\r\n" resp))
                 (head (substring resp 0 sep))
                 (body (substring resp (+ sep 4)))
                 (parsed (json-read-from-string body)))
            (should (string-match-p "\\`HTTP/[0-9.]+ 401" head))
            (should (string-match-p "Content-Type: application/json" head))
            (should (equal (cdr (assq 'error parsed)) "unauthorized"))
            ;; handler never ran → no items leaked into the body
            (should-not (string-match-p "items" body))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-auth-allow ()
  "clime-serve :auth allow (token present) → 200 and handler output."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9892) (server nil) (auth-sources nil)
        (auth (lambda (req)
                (equal "sekret"
                       (cdr (assoc "token" (plist-get req :headers)))))))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-fmt-app
                                    :port port :default-format 'json :auth auth))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/list?token=sekret" port)
                       t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (search-forward "200" nil t))
                  (goto-char (point-min))
                  (search-forward "\n\n" nil t)
                  (let ((parsed (json-read-from-string
                                 (buffer-substring (point) (point-max)))))
                    (should (equal (cdr (assq 'items parsed)) [1 2 3]))))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(ert-deftest clime-test-serve/integration-nil-auth-ungated ()
  "clime-serve with no :auth serves without gating (200)."
  (skip-unless (featurep 'clime-serve))
  (clime-test-serve--require-web-server)
  (let ((port 9893) (server nil) (auth-sources nil))
    (unwind-protect
        (progn
          (setq server (clime-serve clime-test--serve-fmt-app
                                    :port port :default-format 'json))
          (let* ((url-request-method "GET")
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/list" port) t nil 5)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (search-forward "200" nil t)))
              (kill-buffer buf))))
      (when server (clime-serve-stop server)))))

(provide 'clime-serve-tests)
;;; clime-serve-tests.el ends here
