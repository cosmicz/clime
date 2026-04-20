;;; clime-config-tests.el --- Tests for config file providers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for config providers: JSON and sexp provider creators,
;; config factory (app, result) → provider (command-path, param-name → value),
;; precedence (CLI > env > config > default), hierarchical inheritance,
;; path-based lookup with commands and groups, type coercion, edge cases.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-config)
(require 'clime-parse)

;;; ─── Helper ────────────────────────────────────────────────────────────

(defmacro clime-test-with-config-file (filename content &rest body)
  "Create a temp config file with FILENAME suffix and CONTENT, execute BODY.
Binds `config-file' to the absolute path.  File is deleted after BODY."
  (declare (indent 2))
  `(let ((config-file (make-temp-file "clime-test-config-" nil ,filename)))
     (unwind-protect
         (progn
           (with-temp-file config-file
             (insert ,content))
           ,@body)
       (delete-file config-file))))

;;; ─── JSON provider — nested walk ───────────────────────────────────────

(ert-deftest clime-test-config/json-basic ()
  "JSON config provides option values via nested path walk."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 8080}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

(ert-deftest clime-test-config/json-string-coerced ()
  "JSON string values are type-coerced through the option's type."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": \"3000\"}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     3000)))))

(ert-deftest clime-test-config/json-multiple-keys ()
  "JSON config with multiple keys sets multiple options."
  (clime-test-with-config-file ".json"
      "{\"serve\": {\"host\": \"0.0.0.0\", \"port\": 9090}}"
    (let* ((file config-file)
           (opt-host (clime-make-option :name 'host :flags '("--host")))
           (opt-port (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt-host opt-port)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'host)
                     "0.0.0.0"))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     9090)))))

;;; ─── JSON — hierarchical inheritance ───────────────────────────────────

(ert-deftest clime-test-config/json-root-level-inherited ()
  "Root-level config value is inherited by commands."
  (clime-test-with-config-file ".json" "{\"port\": 8080}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

(ert-deftest clime-test-config/json-command-overrides-root ()
  "Command-level config overrides root-level for same param."
  (clime-test-with-config-file ".json"
      "{\"port\": 8080, \"serve\": {\"port\": 9090}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     9090)))))

(ert-deftest clime-test-config/json-disambiguates-same-param ()
  "Same param name on different commands gets different config values."
  (clime-test-with-config-file ".json"
      "{\"port\": 3000, \"serve\": {\"port\": 8080}, \"deploy\": {\"port\": 9090}}"
    (let* ((file config-file)
           (opt1 (clime-make-option :name 'port :flags '("--port")
                                    :type 'integer))
           (opt2 (clime-make-option :name 'port :flags '("--port")
                                    :type 'integer))
           (cmd-serve (clime-make-command :name "serve" :handler #'ignore
                                          :options (list opt1)))
           (cmd-deploy (clime-make-command :name "deploy" :handler #'ignore
                                           :options (list opt2)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd-serve)
                                                (cons "deploy" cmd-deploy))))
           (result-serve (clime-parse app '("serve")))
           (result-deploy (clime-parse app '("deploy"))))
      (should (equal (plist-get (clime-parse-result-params result-serve) 'port)
                     8080))
      (should (equal (plist-get (clime-parse-result-params result-deploy) 'port)
                     9090)))))

;;; ─── JSON — nested commands ────────────────────────────────────────────

(ert-deftest clime-test-config/json-nested-commands ()
  "JSON provider walks nested objects for subcommand paths."
  (clime-test-with-config-file ".json"
      "{\"server\": {\"start\": {\"port\": 8080}, \"stop\": {\"force\": true}}}"
    (let* ((file config-file)
           (opt-port (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
           (opt-force (clime-make-option :name 'force :flags '("--force")
                                         :nargs 0))
           (cmd-start (clime-make-command :name "start" :handler #'ignore
                                          :options (list opt-port)))
           (cmd-stop (clime-make-command :name "stop" :handler #'ignore
                                         :options (list opt-force)))
           (grp (clime-make-group :name "server"
                                  :children (list (cons "start" cmd-start)
                                                  (cons "stop" cmd-stop))))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "server" grp))))
           (result (clime-parse app '("server" "start"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

;;; ─── JSON — groups in path ─────────────────────────────────────────────

(ert-deftest clime-test-config/json-group-in-path ()
  "Group names appear in the config path."
  (clime-test-with-config-file ".json"
      "{\"serve\": {\"database\": {\"host\": \"db.local\", \"name\": \"mydb\"}}}"
    (let* ((file config-file)
           (opt-host (clime-make-option :name 'host :flags '("--db-host")))
           (opt-name (clime-make-option :name 'name :flags '("--db-name")))
           (db-group (clime-make-group :name "database" :inline t
                                       :options (list opt-host opt-name)))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :children (list (cons "database" db-group))))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve" "--db-host" "override"))))
      ;; CLI value for host, config value for name
      (should (equal (plist-get (clime-parse-result-params result) 'host)
                     "override"))
      (should (equal (plist-get (clime-parse-result-params result) 'name)
                     "mydb")))))

;;; ─── Sexp provider ─────────────────────────────────────────────────────

(ert-deftest clime-test-config/sexp-plist ()
  "Sexp config file (nested plist) provides option values."
  (clime-test-with-config-file ".eld"
      "(:serve (:port 8080 :host \"localhost\"))"
    (let* ((file config-file)
           (opt-host (clime-make-option :name 'host :flags '("--host")))
           (opt-port (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt-host opt-port)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-sexp file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'host)
                     "localhost"))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

;;; ─── Precedence ────────────────────────────────────────────────────────

(ert-deftest clime-test-config/cli-overrides-config ()
  "CLI value takes precedence over config."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 8080}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve" "--port" "3000"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     3000)))))

(ert-deftest clime-test-config/env-overrides-config ()
  "Env var takes precedence over config."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 8080}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer :env "TEST_PORT"))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (saved (getenv "TEST_PORT")))
      (unwind-protect
          (progn
            (setenv "TEST_PORT" "5000")
            (let ((result (clime-parse app '("serve"))))
              (should (equal (plist-get (clime-parse-result-params result) 'port)
                             5000))))
        (setenv "TEST_PORT" saved)))))

(ert-deftest clime-test-config/config-overrides-default ()
  "Config takes precedence over declared default."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 8080}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer :default 3000))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

;;; ─── Config from CLI option ────────────────────────────────────────────

(ert-deftest clime-test-config/from-cli-option ()
  "Config factory reads --config path from pass-1 result."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 4040}}"
    (let* ((file config-file)
           (opt-config (clime-make-option :name 'config :flags '("--config")))
           (opt-port (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt-config opt-port)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app result)
                                          (when-let ((path (plist-get
                                                           (clime-parse-result-params result)
                                                           'config)))
                                            (clime-config-json path)))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app (list "serve" "--config" file))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     4040)))))

;;; ─── Setup runs before config ──────────────────────────────────────────

(ert-deftest clime-test-config/setup-before-config ()
  "Setup hook runs before config factory."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 6060}}"
    (let* ((file config-file)
           (setup-ran nil)
           (config-saw-setup nil)
           (captured-port nil)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve"
                                    :handler (lambda (ctx)
                                               (setq captured-port
                                                     (clime-ctx-get ctx 'port)))
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :setup (lambda (_app _result)
                                         (setq setup-ran t))
                                :config (lambda (_app _result)
                                          (setq config-saw-setup setup-ran)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd)))))
      (clime-run app '("serve"))
      (should setup-ran)
      (should config-saw-setup)
      (should (equal captured-port 6060)))))

;;; ─── Custom provider ───────────────────────────────────────────────────

(ert-deftest clime-test-config/custom-provider ()
  "Custom provider receives command-path and param-name."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                 :type 'integer))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :config (lambda (_app _result)
                                        (lambda (cmd-path param-name)
                                          (when (and (equal cmd-path '("serve"))
                                                     (equal param-name "port"))
                                            9090)))
                              :children (list (cons "serve" cmd))))
         (result (clime-parse app '("serve"))))
    (should (equal (plist-get (clime-parse-result-params result) 'port)
                   9090))))

;;; ─── Edge cases ────────────────────────────────────────────────────────

(ert-deftest clime-test-config/factory-returns-nil ()
  "Config factory returning nil means no config applied."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                 :type 'integer :default 3000))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :config (lambda (_app _result) nil)
                              :children (list (cons "serve" cmd))))
         (result (clime-parse app '("serve"))))
    (should (equal (plist-get (clime-parse-result-params result) 'port)
                   3000))))

(ert-deftest clime-test-config/nil-config-no-two-pass ()
  "Default nil :config means no config loading."
  (let* ((opt (clime-make-option :name 'port :flags '("--port")
                                 :type 'integer :default 3000))
         (cmd (clime-make-command :name "serve" :handler #'ignore
                                  :options (list opt)))
         (app (clime-make-app :name "t" :version "1"
                              :children (list (cons "serve" cmd))))
         (result (clime-parse app '("serve"))))
    (should (equal (plist-get (clime-parse-result-params result) 'port)
                   3000))))

(ert-deftest clime-test-config/empty-json-no-values ()
  "Empty JSON object applies no values."
  (clime-test-with-config-file ".json" "{}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer :default 3000))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     3000)))))

(ert-deftest clime-test-config/unknown-keys-ignored ()
  "Config keys not matching any option are silently ignored."
  (clime-test-with-config-file ".json"
      "{\"serve\": {\"port\": 8080, \"unknown_key\": \"val\"}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

(ert-deftest clime-test-config/boolean-from-json ()
  "JSON boolean true/false are handled correctly."
  (clime-test-with-config-file ".json" "{\"cmd\": {\"verbose\": true}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'verbose :flags '("--verbose")
                                   :nargs 0))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (eq (plist-get (clime-parse-result-params result) 'verbose)
                  t)))))

(ert-deftest clime-test-config/json-array-for-multiple ()
  "JSON array is used for multiple-value options."
  (clime-test-with-config-file ".json"
      "{\"cmd\": {\"tags\": [\"a\", \"b\", \"c\"]}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'tags :flags '("--tag")
                                   :multiple t))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'tags)
                     '("a" "b" "c"))))))

;;; ─── json-to-elisp unit tests ───────────────────────────────────────────

(ert-deftest clime-test-config/json-to-elisp-false ()
  "json-false is converted to nil."
  (should (eq (clime-config--json-to-elisp :json-false) nil)))

(ert-deftest clime-test-config/json-to-elisp-true ()
  "JSON true is converted to t."
  (should (eq (clime-config--json-to-elisp t) t)))

(ert-deftest clime-test-config/json-to-elisp-vector ()
  "JSON array (vector) is converted to list."
  (should (equal (clime-config--json-to-elisp [1 2 3]) '(1 2 3))))

(ert-deftest clime-test-config/json-to-elisp-number ()
  "JSON number passes through unchanged."
  (should (equal (clime-config--json-to-elisp 42) 42))
  (should (equal (clime-config--json-to-elisp 3.14) 3.14)))

(ert-deftest clime-test-config/json-to-elisp-string ()
  "JSON string passes through unchanged."
  (should (equal (clime-config--json-to-elisp "hello") "hello")))

;;; ─── Sexp inheritance ───────────────────────────────────────────────────

(ert-deftest clime-test-config/sexp-root-inherited ()
  "Sexp root-level value is inherited by commands."
  (clime-test-with-config-file ".eld" "(:port 8080)"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-sexp file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080)))))

(ert-deftest clime-test-config/sexp-command-overrides-root ()
  "Sexp command-level value overrides root-level."
  (clime-test-with-config-file ".eld" "(:port 3000 :serve (:port 9090))"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-sexp file))
                                :children (list (cons "serve" cmd))))
           (result (clime-parse app '("serve"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     9090)))))

;;; ─── JSON false/null ───────────────────────────────────────────────────

(ert-deftest clime-test-config/json-false-treated-as-unset ()
  "JSON false is treated as not-configured (consistent with env var falsy)."
  (clime-test-with-config-file ".json" "{\"cmd\": {\"verbose\": false}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'verbose :flags '("--verbose")
                                   :nargs 0 :default nil))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (eq (plist-get (clime-parse-result-params result) 'verbose)
                  nil)))))

(ert-deftest clime-test-config/json-null-treated-as-unset ()
  "JSON null is treated as not-configured."
  (clime-test-with-config-file ".json" "{\"cmd\": {\"port\": null}}"
    (let* ((file config-file)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer :default 3000))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     3000)))))

;;; ─── Full precedence chain ─────────────────────────────────────────────

(ert-deftest clime-test-config/full-precedence-chain ()
  "CLI > env > config > default across multiple options."
  (clime-test-with-config-file ".json"
      "{\"cmd\": {\"a\": \"from-config\", \"b\": \"from-config\", \"c\": \"from-config\"}}"
    (let* ((file config-file)
           ;; a: set via CLI, config, and default
           (opt-a (clime-make-option :name 'a :flags '("--a") :default "from-default"))
           ;; b: set via env, config, and default
           (opt-b (clime-make-option :name 'b :flags '("--b") :env "TEST_B"
                                     :default "from-default"))
           ;; c: set via config and default only
           (opt-c (clime-make-option :name 'c :flags '("--c") :default "from-default"))
           ;; d: default only (not in config)
           (opt-d (clime-make-option :name 'd :flags '("--d") :default "from-default"))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt-a opt-b opt-c opt-d)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (saved (getenv "TEST_B")))
      (unwind-protect
          (progn
            (setenv "TEST_B" "from-env")
            (let* ((result (clime-parse app '("cmd" "--a" "from-cli")))
                   (params (clime-parse-result-params result)))
              (should (equal (plist-get params 'a) "from-cli"))
              (should (equal (plist-get params 'b) "from-env"))
              (should (equal (plist-get params 'c) "from-config"))
              (should (equal (plist-get params 'd) "from-default"))))
        (setenv "TEST_B" saved)))))

;;; ─── Config with positional args ───────────────────────────────────────

(ert-deftest clime-test-config/positional-arg-from-config ()
  "Config can provide values for positional args."
  (clime-test-with-config-file ".json" "{\"cmd\": {\"name\": \"configured\"}}"
    (let* ((file config-file)
           (arg (clime-make-arg :name 'name))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :args (list arg)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd"))))
      (should (equal (plist-get (clime-parse-result-params result) 'name)
                     "configured")))))

(ert-deftest clime-test-config/cli-arg-overrides-config-arg ()
  "CLI positional arg overrides config value."
  (clime-test-with-config-file ".json" "{\"cmd\": {\"name\": \"configured\"}}"
    (let* ((file config-file)
           (arg (clime-make-arg :name 'name))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :args (list arg)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "from-cli"))))
      (should (equal (plist-get (clime-parse-result-params result) 'name)
                     "from-cli")))))

;;; ─── clime-run integration ─────────────────────────────────────────────

(ert-deftest clime-test-config/run-integration ()
  "Config works end-to-end through clime-run."
  (clime-test-with-config-file ".json" "{\"serve\": {\"port\": 7777}}"
    (let* ((file config-file)
           (captured-port nil)
           (opt (clime-make-option :name 'port :flags '("--port")
                                   :type 'integer))
           (cmd (clime-make-command :name "serve"
                                    :handler (lambda (ctx)
                                               (setq captured-port
                                                     (clime-ctx-get ctx 'port)))
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "serve" cmd)))))
      (clime-run app '("serve"))
      (should (equal captured-port 7777)))))

;;; ─── Deep nesting inheritance ──────────────────────────────────────────

(ert-deftest clime-test-config/deep-nesting-walk-up ()
  "Deeply nested command inherits from intermediate and root levels."
  (clime-test-with-config-file ".json"
      "{\"timeout\": 10, \"server\": {\"port\": 8080, \"start\": {\"daemon\": true}}}"
    (let* ((file config-file)
           (opt-port (clime-make-option :name 'port :flags '("--port")
                                        :type 'integer))
           (opt-daemon (clime-make-option :name 'daemon :flags '("--daemon")
                                          :nargs 0))
           (opt-timeout (clime-make-option :name 'timeout :flags '("--timeout")
                                           :type 'integer))
           (cmd-start (clime-make-command :name "start" :handler #'ignore
                                          :options (list opt-port opt-daemon
                                                        opt-timeout)))
           (grp (clime-make-group :name "server"
                                  :children (list (cons "start" cmd-start))))
           (app (clime-make-app :name "t" :version "1"
                                :config (lambda (_app _result)
                                          (clime-config-json file))
                                :children (list (cons "server" grp))))
           (result (clime-parse app '("server" "start"))))
      ;; port from server level, daemon from start level, timeout from root
      (should (equal (plist-get (clime-parse-result-params result) 'port)
                     8080))
      (should (eq (plist-get (clime-parse-result-params result) 'daemon)
                  t))
      (should (equal (plist-get (clime-parse-result-params result) 'timeout)
                     10)))))

(provide 'clime-config-tests)
;;; clime-config-tests.el ends here
