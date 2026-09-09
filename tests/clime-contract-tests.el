;;; clime-contract-tests.el --- Tests for command metadata projection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the transport-neutral, surface-aware command contract shared by
;; adapters.  Fixtures deliberately disagree by surface and visibility so the
;; filtering assertions fail if the projection call is removed.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-contract)
(require 'clime-serve)
(require 'clime-skill)

(defvar clime-contract-test--default-called nil)
(defvar clime-contract-test--choices-called nil)
(defvar clime-contract-test--type-called nil)

(defun clime-contract-test--dynamic-default ()
  "Record an unsafe lazy default call."
  (setq clime-contract-test--default-called t)
  "secret-default")

(defun clime-contract-test--dynamic-choices ()
  "Record an unsafe dynamic choices call."
  (setq clime-contract-test--choices-called t)
  '("secret-choice"))

(defun clime-contract-test--build-app ()
  "Return a command tree covering projection policy and metadata."
  (let* ((global
          (clime-make-option
           :name 'verbose :flags '("--verbose" "-v") :count t
           :help "Increase verbosity"))
         (locked
          (clime-make-option
           :name 'token :flags '("--token") :hidden nil :locked t
           :default "literal-token"))
         (hidden-option
          (clime-make-option
           :name 'internal :flags '("--internal") :hidden t))
         (dynamic
          (clime-make-option
           :name 'mode :flags '("--mode" "-m") :required t :nargs 2
           :multiple t :separator "," :negatable nil
           :type (lambda (value)
                   (setq clime-contract-test--type-called t)
                   value)
           :choices #'clime-contract-test--dynamic-choices
           :deprecated "Use --profile" :help "Execution mode"))
         (lazy
          (clime-make-option
           :name 'lazy :flags '("--lazy")
           :default #'clime-contract-test--dynamic-default
           :help "Lazy value"))
         (file
          (clime-arg--create
           :name 'file :required nil :nargs :rest :type 'string
           :choices '("a" "b") :default "a" :deprecated t
           :help "Input files"))
         (inline
          (clime-make-group
           :name "mode-options" :inline t
           :options (list (clime-make-option
                           :name 'inline :flags '("--inline")))))
         (run
          (clime-make-command
           :name "run" :aliases '("execute") :help "Run work"
           :surfaces '(cli serve mcp) :handler #'ignore
           :options (list dynamic lazy hidden-option) :args (list file)
           :children `(("mode-options" . ,inline))))
         (serve-only
          (clime-make-command
           :name "serve-only" :help "Serve only" :surfaces '(serve)
           :handler #'ignore))
         (mcp-only
          (clime-make-command
           :name "mcp-only" :help "MCP only" :surfaces '(mcp)
           :handler #'ignore))
         (hidden
          (clime-make-command
           :name "hidden" :help "Hidden" :hidden t :handler #'ignore))
         (ops
          (clime-make-group
           :name "ops" :help "Operations" :options (list locked)
           :children `(("run" . ,run)
                       ("serve-only" . ,serve-only)
                       ("mcp-only" . ,mcp-only)
                       ("hidden" . ,hidden))))
         (shortcut
          (clime-alias--create
           :name "quick" :target '("ops" "run")
           :defaults '((mode . "fast")) :vals '((mode . "fast"))))
         (app
          (clime-make-app
           :name "demo" :version "1.0" :env-prefix "DEMO"
           :help "Contract fixture" :options (list global)
           :children `(("ops" . ,ops) ("quick" . ,shortcut)))))
    app))

(defun clime-contract-test--nodes (app &rest overrides)
  "Return APP's contract nodes with standard policy plus OVERRIDES."
  (let ((policy (list :surface 'serve :tree-mode 'declaration
                      :visibility 'all :value-mode 'static-safe)))
    (cl-loop for (key value) on overrides by #'cddr
             do (setq policy (plist-put policy key value)))
    (apply #'clime-contract-nodes app policy)))

(defun clime-contract-test--find (nodes path)
  "Return the node pair for PATH in NODES."
  (cl-find path nodes :key #'cdr :test #'equal))

(defun clime-contract-test--option (options name)
  "Return option named NAME from OPTIONS."
  (cl-find name options :key (lambda (option) (symbol-name
                                              (clime-option-name option)))
           :test #'equal))

(ert-deftest clime-test-contract/functional-api-is-policy-guarded ()
  "Every public functional entry rejects an incomplete policy."
  (let* ((app (clime-contract-test--build-app))
         (command (cdr (assoc "run"
                              (clime-group-children
                               (cdr (assoc "ops" (clime-group-children app)))))))
         (option (car (clime-node-options command))))
    (dolist (call `((clime-contract-nodes ,app)
                    (clime-contract-find ,app '("ops" "run"))
                    (clime-contract-effective-options ,command ,app)
                    (clime-contract-options ,command)
                    (clime-contract-safe-default ,option)
                    (clime-contract-safe-choices ,option)
                    (clime-contract-describe-type 'string)))
      (should-error (eval call)))
    (let ((policy '(:surface serve :tree-mode declaration
                    :visibility all :value-mode static-safe)))
      (should (apply #'clime-contract-nodes app policy))
      (should (apply #'clime-contract-find app '("ops" "run") policy))
      (should (apply #'clime-contract-effective-options command app policy))
      (should (apply #'clime-contract-options command policy))
      (should (null (apply #'clime-contract-safe-default option policy)))
      (should (eq (apply #'clime-contract-safe-choices option policy) 'dynamic))
      (should (equal (apply #'clime-contract-describe-type 'string policy)
                     "string")))))

(ert-deftest clime-test-contract/policy-is-mandatory-and-closed ()
  "Every policy input is explicit and unknown values fail closed."
  (let ((app (clime-contract-test--build-app)))
    (dolist (policy
             '(()
               (:surface serve :tree-mode declaration :visibility all)
               (:surface serve :tree-mode declaration :visibility all
                :value-mode static-safe :unknown t)
               (:surface serve :surface cli :tree-mode declaration
                :visibility all :value-mode static-safe)
               (:surface web :tree-mode declaration :visibility all
                :value-mode static-safe)
               (:surface serve :tree-mode mutable :visibility all
                :value-mode static-safe)
               (:surface serve :tree-mode declaration :visibility secret
                :value-mode static-safe)
               (:surface serve :tree-mode declaration :visibility all
                :value-mode resolved)))
      (should-error (apply #'clime-contract-nodes app policy)))))

(ert-deftest clime-test-contract/surface-and-visibility-transform-tree ()
  "Surface and visibility policy correct a deliberately divergent fixture."
  (let* ((app (clime-contract-test--build-app))
         (mcp (clime-contract-test--nodes app :surface 'mcp))
         (declared (clime-contract-test--nodes app))
         (visible (clime-contract-test--nodes
                   app :visibility 'visible :surface 'serve))
         (run (car (clime-contract-test--find declared '("ops" "run"))))
         (visible-run (car (clime-contract-test--find visible '("ops" "run"))))
         (policy '(:surface serve :tree-mode declaration
                   :visibility visible :value-mode static-safe)))
    (should (clime-contract-test--find mcp '("ops" "mcp-only")))
    (should-not (clime-contract-test--find mcp '("ops" "serve-only")))
    (should-not (clime-contract-test--find declared '("ops" "mcp-only")))
    (should-not (clime-contract-test--find visible '("ops" "mcp-only")))
    (should (clime-contract-test--find declared '("ops" "hidden")))
    (should-not (clime-contract-test--find visible '("ops" "hidden")))
    (should (clime-contract-test--option
             (apply #'clime-contract-options run
                    '(:surface serve :tree-mode declaration :visibility all
                      :value-mode static-safe)) "internal"))
    (should-not (clime-contract-test--option
                 (apply #'clime-contract-options visible-run policy) "internal"))
    (should (clime-contract-test--find visible '("ops" "serve-only")))))

(ert-deftest clime-test-contract/projects-full-static-metadata-without-calls ()
  "Static-safe metadata is complete and dynamic declarations never execute."
  (let* ((clime-contract-test--default-called nil)
         (clime-contract-test--choices-called nil)
         (clime-contract-test--type-called nil)
         (app (clime-contract-test--build-app))
         (nodes (clime-contract-test--nodes app))
         (entry (clime-contract-test--find nodes '("ops" "run")))
         (run (car entry))
         (mode (clime-contract-test--option (clime-node-options run) "mode"))
         (lazy (clime-contract-test--option (clime-node-options run) "lazy"))
         (file (car (clime-node-args run)))
         (policy '(:surface serve :tree-mode declaration
                   :visibility all :value-mode static-safe)))
    (should (equal (clime-app-version app) "1.0"))
    (should (equal (cdr entry) '("ops" "run")))
    (should (equal (clime-node-aliases run) '("execute")))
    (should (clime-node-handler run))
    (should (equal (clime-option-flags mode) '("--mode" "-m")))
    (should (clime-option-required mode))
    (should (= (clime-option-nargs mode) 2))
    (should (clime-option-multiple mode))
    (should (equal (clime-option-separator mode) ","))
    (should (equal (clime-option-deprecated mode) "Use --profile"))
    (should (eq (apply #'clime-contract-safe-choices mode policy) 'dynamic))
    (should-not (apply #'clime-contract-safe-default lazy policy))
    (should-not (apply #'clime-contract-describe-type
                       (clime-option-type mode) policy))
    (should (equal (symbol-name (clime-arg-name file)) "file"))
    (should-not (clime-arg-required file))
    (should (eq (clime-arg-nargs file) :rest))
    (should (equal (apply #'clime-contract-safe-choices file policy) '("a" "b")))
    (should (equal (apply #'clime-contract-safe-default file policy) "a"))
    (should-not clime-contract-test--default-called)
    (should-not clime-contract-test--choices-called)
    (should-not clime-contract-test--type-called)))

(ert-deftest clime-test-contract/alias-modes-and-locked-policy ()
  "Declaration aliases stay explicit; prepared aliases expose locked state."
  (let* ((app (clime-contract-test--build-app))
         (declaration (clime-contract-test--nodes app))
         (alias (car (clime-contract-test--find declaration '("quick"))))
         (prepared (clime-contract-test--nodes
                    app :tree-mode 'prepared :visibility 'all))
         (resolved (car (clime-contract-test--find prepared '("quick"))))
         (public (clime-contract-test--nodes
                  app :tree-mode 'prepared :visibility 'public))
         (public-alias (car (clime-contract-test--find public '("quick"))))
         (all-policy '(:surface serve :tree-mode prepared
                       :visibility all :value-mode static-safe))
         (public-policy '(:surface serve :tree-mode prepared
                          :visibility public :value-mode static-safe)))
    ;; Apps prepare aliases at construction; the alias entry retains its
    ;; invocation path and target help path in both contract modes.
    (should (clime-command-p alias))
    (should (equal (clime-node-help-path alias) '("ops" "run")))
    (should (clime-command-p resolved))
    (should (clime-node-help-path resolved))
    (should (clime-option-locked
             (clime-contract-test--option
              (apply #'clime-contract-options resolved all-policy) "mode")))
    (should-not (clime-contract-test--option
                 (apply #'clime-contract-options public-alias public-policy)
                 "mode"))))

(ert-deftest clime-test-contract/effective-options-record-owner-paths ()
  "Commands receive inherited options with canonical owner paths."
  (let* ((app (clime-contract-test--build-app))
         (run (car (clime-contract-find
                    app '("ops" "run") :surface 'serve :tree-mode 'declaration
                    :visibility 'all :value-mode 'static-safe)))
         (effective (clime-contract-effective-options
                     run app :surface 'serve :tree-mode 'declaration
                     :visibility 'all :value-mode 'static-safe))
         (global (clime-contract-test--option (mapcar #'car effective) "verbose"))
         (group (clime-contract-test--option (mapcar #'car effective) "token"))
         (local (clime-contract-test--option (mapcar #'car effective) "mode"))
         (inline (clime-contract-test--option (mapcar #'car effective) "inline")))
    (should (equal (mapcar (lambda (option) (symbol-name (clime-option-name option)))
                           (mapcar #'car effective))
                   '("verbose" "token" "mode" "lazy" "internal" "inline")))
    (should (equal (cdr (assq global effective)) nil))
    (should (equal (cdr (assq group effective)) '("ops")))
    (should (equal (cdr (assq local effective)) '("ops" "run")))
    (should (equal (cdr (assq inline effective)) '("ops" "run")))))

(ert-deftest clime-test-contract/prepared-nodes-do-not-mutate-the-app ()
  "Prepared alias discovery is read-only with respect to the registered APP."
  (let* ((app (clime-contract-test--build-app))
         (alias (cdr (assoc "quick" (clime-group-children app))))
         (path (copy-sequence (clime-node-help-path alias))))
    (clime-contract-nodes app :surface 'serve :tree-mode 'prepared
                          :visibility 'all :value-mode 'static-safe)
    (should (equal (clime-node-help-path alias) path))
    (should (equal (clime-option-flags (car (clime-node-options app)))
                   '("--verbose" "-v")))))

(ert-deftest clime-test-contract/adapters-use-explicit-shared-policies ()
  "Skill and HTTP introspection both call the shared projection authority."
  (let* ((app (clime-contract-test--build-app))
         (original (symbol-function 'clime-contract-nodes))
         (policies nil))
    (cl-letf (((symbol-function 'clime-contract-nodes)
               (lambda (projected-app &rest policy)
                 (push policy policies)
                 (apply original projected-app policy))))
      (clime-skill-manifest app :command "demo")
      (clime-serve--inject-api-commands app)
      (clime-serve--dispatch app '("_api" "commands") nil))
    (should (member '(:surface cli :tree-mode declaration
                      :visibility public :value-mode static-safe)
                    policies))
    (should (member '(:surface serve :tree-mode prepared
                      :visibility visible :value-mode declared)
                    policies))))

(ert-deftest clime-test-contract/routes-no-longer-use-legacy-tree-collector ()
  "The route listing flattens the contract and preserves exact output."
  (let ((app (clime-contract-test--build-app)))
    (clime-serve--inject-api-commands app)
    (cl-letf (((symbol-function 'clime-node-collect)
               (lambda (&rest _)
                 (error "legacy route traversal called"))))
      (let ((result (clime-serve--dispatch app '("_routes") nil)))
        (should (= (plist-get result :status) 200))
        (should (equal (plist-get result :body)
                       (concat "/ops/run  Run work\n"
                               "/ops/serve-only  Serve only\n"
                               "/quick  Run work")))))))

(ert-deftest clime-test-contract/divergent-fixture-reaches-adapter-wire-forms ()
  "Adapters retain distinct wire forms while sharing corrected filtering."
  (let* ((app (clime-contract-test--build-app))
         (skill (clime-skill-render
                 (clime-skill-manifest app :command "demo"))))
    (clime-serve--inject-api-commands app)
    (let ((http (plist-get
                 (clime-serve--dispatch app '("_api" "commands") nil)
                 :body)))
      ;; The fixture begins divergent: these declarations target different
      ;; surfaces.  Removing either adapter's shared projection call makes at
      ;; least one of these before/after filtering assertions fail.
      (should (string-match-p "demo ops run" skill))
      (should-not (string-match-p "serve-only" skill))
      (should-not (string-match-p "mcp-only" skill))
      (should (string-match-p "\\\"name\\\":\\\"serve-only\\\"" http))
      (should-not (string-match-p "\\\"name\\\":\\\"mcp-only\\\"" http))
      (should (string-match-p "\\\"flags\\\":\\[\\\"--mode\\\",\\\"-m\\\"\\]"
                              http))
      ;; The contract represents inline metadata, but this refactor keeps the
      ;; adapters' existing omission until clime-e5lt is separately approved.
      (should-not (string-match-p "--inline" skill))
      (should-not (string-match-p "--inline" http)))))

(provide 'clime-contract-tests)
;;; clime-contract-tests.el ends here
