;;; pkm.el --- Example clime app: a mock package manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; A comprehensive example showing every major clime feature.
;; This is a mock package manager CLI called `pkm' — the handlers
;; are stubs that print what they would do, not a real implementation.
;;
;; Features demonstrated (in order of appearance):
;;
;;   1.  Root count flag         -v/--verbose (stackable: -vvv = 3)
;;   2.  Root boolean flag       -q/--quiet (:bool shorthand)
;;   3.  Hidden option           --internal-trace (omitted from help)
;;   4.  JSON output format      clime-output-format with --json flag
;;   5.  Command with args       install <package>
;;   6.  Aliases                 install → i, search → s, list → ls (symbols)
;;   7.  Boolean flags           --force, --dry-run (:bool shorthand)
;;   8.  Multiple option         --tag (repeatable) on install
;;   9.  Default value           --registry defaults to "default"
;;  10.  Optional arg            search [query]
;;  11.  Option groups in help   "Filter" group on list (--author, --since)
;;  12.  Rest args               run <script> [args...]
;;  13.  Nested group            repo add/list/remove
;;  14.  Group handler           config (shows all) / config set|get
;;  15.  Hidden command          debug
;;  16.  Context destructuring   clime-let macro in handlers
;;  17.  Command categories      search/list under "Discovery"
;;  18.  Inline group category   admin inline group with option + command
;;  19.  Parameter template      clime-defopt for shared --force flag
;;  20.  Command alias           clime-alias-for exposes nested cmd at top level
;;  21.  Negatable flag          --color/--no-color with ternary state
;;  22.  Mutual exclusion        --format-table/--format-csv via clime-mutex
;;  23.  Deprecated option       --output with migration hint
;;  24.  clime-param accessor    ternary access for negatable flags
;;
;; Run it (this file is directly executable via the shebang header):
;;   chmod +x examples/pkm.el
;;   ./examples/pkm.el --help
;;   ./examples/pkm.el install foo --tag dev
;;   ./examples/pkm.el -vv search
;;   ./examples/pkm.el config
;;   ./examples/pkm.el repo add myrepo https://example.com
;;   ./examples/pkm.el add-repo myrepo https://example.com
;;
;; Or without the shebang:
;;   emacs --batch -Q -L /path/to/clime -l examples/pkm.el -- --help
;;
;; The shebang uses a relative -L path since this file lives inside
;; the clime repo.  For external apps, use `clime-make.el init' which
;; generates an absolute path.  See DEVELOPMENT.org for details.

;;; Code:

(require 'clime)

;; ─── Parameter Templates ────────────────────────────────────────────────
;; [19] Define once, reuse via :from — shared across install and repo remove

(clime-defopt force-flag
  :bool
  :help "Force the operation (skip safety checks)")

;; ─── App Definition ──────────────────────────────────────────────────────

(clime-app pkm
  :version "0.5.0"
  :help "A package manager for Emacs Lisp projects."
  :examples '(("pkm install foo --tag dev" . "Install with tags")
              ("pkm -vv search" . "Verbose search")
              ("pkm repo add myrepo https://example.com" . "Add a repo")
              "pkm add-repo myrepo https://example.com")

  ;; ── Root Options ─────────────────────────────────────────────────────
  ;; [1] Count flag — stackable verbosity (-v, -vv, -vvv)
  (clime-opt verbose ("-v" "--verbose") :count
             :help "Increase output verbosity")

  ;; [2] Boolean flag — :bool is shorthand for :nargs 0
  (clime-opt quiet ("-q" "--quiet") :bool
             :help "Suppress non-essential output")

  ;; [3] Hidden option — internal use only, omitted from --help
  (clime-opt internal-trace ("--internal-trace") :bool :hidden
             :help "Enable internal tracing")

  ;; [4] Output format — explicit clime-output-format with --json flag
  (clime-output-format json ("--json")
    :help "Output as JSON")

  ;; [21] Negatable flag — --color / --no-color with ternary state
  (clime-opt color ("--color") :negatable :default t
             :help "Colorize output")

  ;; ── install ──────────────────────────────────────────────────────────
  ;; [5] Command with required arg, [6] symbol alias, [7] booleans,
  ;; [8] multiple option, [9] default value, [16] clime-let
  (clime-command install
    :help "Install a package into the project"
    :aliases (i)                                ; [6] symbol alias

    (clime-arg package :help "Package name or URL")

    (clime-opt force ("--force" "-f") :from force-flag  ; [19] from template
               :help "Overwrite existing installation")

    (clime-opt dry-run ("--dry-run" "-n") :bool
               :help "Show what would be installed without doing it")

    (clime-opt tag ("--tag" "-t") :multiple
               :help "Add tag to installed package")

    (clime-opt registry ("--registry" "-r")
      :help "Registry to install from"
      :default "default")

    ;; [25] Parameterized integer type — bounded range
    (clime-opt timeout ("--timeout" "-T") :type '(integer :min 1 :max 300)
               :help "Network timeout in seconds"
               :default "30")

    ;; [26] Requires constraint — retries only makes sense with timeout
    (clime-opt retries ("--retries") :type '(integer :min 0 :max 10)
               :help "Number of retry attempts"
               :requires '(timeout))

    (clime-handler (ctx)
      (clime-let ctx (package force dry-run (tags tag) (reg registry)
                              timeout retries)
        (format "Installing %s from %s%s%s%s (timeout=%s%s)"
                package reg
                (if force " [forced]" "")
                (if dry-run " [dry-run]" "")
                (if tags (format " tags=%s" (string-join tags ",")) "")
                timeout
                (if retries (format ", retries=%s" retries) "")))))

  ;; ── search ───────────────────────────────────────────────────────────
  ;; [10] Optional positional arg, [6] symbol alias, [17] command category
  (clime-command search
    :help "Search the package registry"
    :aliases (s)
    :category "Discovery"

    (clime-arg query :required nil
               :help "Search query (omit to list all)")

    ;; [25] Parameterized integer — bounded limit
    (clime-opt limit ("--limit" "-l") :type '(integer :min 1 :max 500)
               :help "Max results to return"
               :default "20")

    ;; [27] Member type — string enum with invoke completion
    (clime-opt scope ("--scope" "-S")
      :help "Search scope"
      :type '(member "local" "global" "all")
      :default "all")

    (clime-handler (ctx)
      (clime-let ctx (query limit scope)
        (if query
            (format "Searching for \"%s\" (limit %s, scope=%s)" query limit scope)
          (format "Listing all packages (limit %s, scope=%s)" limit scope)))))

  ;; ── list ─────────────────────────────────────────────────────────────
  ;; [11] Option groups in help, [6] symbol alias, [17] command category,
  ;; [22] mutex for format, [23] deprecated option
  (clime-command list
    :help "List installed packages"
    :aliases (ls)
    :category "Discovery"

    (clime-opt author ("--author" "-a")
      :help "Filter by author"
      :category "Filter")

    (clime-opt since ("--since")
      :help "Only packages installed after DATE"
      :category "Filter")

    (clime-opt sort ("--sort" "-s")
      :help "Sort field"
      :choices '("name" "date" "size")
      :default "name")

    ;; [22] Mutually exclusive output format options
    (clime-mutex list-format
      (clime-opt format-table ("--table") :bool
                 :help "Output as table (default)"
                 :category "Format")
      (clime-opt format-csv ("--csv") :bool
                 :help "Output as CSV"
                 :category "Format"))

    ;; [23] Deprecated option with migration hint
    (clime-opt output ("--output" "-o")
      :deprecated "Use --table or --csv instead"
      :help "Output format"
      :hidden)

    (clime-handler (ctx)
      ;; [24] clime-param for ternary access to negatable --color
      (let ((color (clime-param ctx 'color 'auto)))
        (clime-let ctx (author since sort format-table format-csv)
          (format "Listing packages (sort=%s, fmt=%s, color=%s%s%s)"
                  sort
                  (cond (format-csv "csv") (format-table "table") (t "table"))
                  color
                  (if author (format ", author=%s" author) "")
                  (if since (format ", since=%s" since) ""))))))

  ;; ── run ──────────────────────────────────────────────────────────────
  ;; [12] Rest args — collects everything after script name
  (clime-command run
    :help "Run a project script"

    (clime-arg script :help "Script name to run")
    (clime-arg args :nargs :rest :required nil
               :help "Arguments passed to the script")

    ;; [28] Member type with env — string enum
    (clime-opt runner ("--runner") :env "PKM_RUNNER"
               :help "Script runner to use"
               :type '(member "sh" "bash" "zsh" "node" "python"))

    ;; [29] Choice type — member or integer (named preset or explicit count)
    (clime-opt concurrency ("--concurrency" "-j")
      :type '(choice (member "auto" "serial" "cpus") (integer :min 1))
      :default "auto"
      :help "Parallelism level")

    ;; [25] Choice type — number or "off" (mixed type)
    (clime-opt max-time ("--max-time" "-m")
      :type '(choice (number :min 0) (const "off"))
      :help "Maximum execution time in seconds, or \"off\"")

    (clime-handler (ctx)
      (clime-let ctx (script args runner concurrency max-time)
        (format "Running script %s%s%s%s%s"
                script
                (if runner (format " [runner=%s]" runner) "")
                (if concurrency (format " [concurrency=%s]" concurrency) "")
                (if max-time (format " [max-time=%ss]" max-time) "")
                (if args (format " with args: %s" (string-join args " ")) "")))))

  ;; ── repo (nested group) ──────────────────────────────────────────────
  ;; [13] Nested group with subcommands
  (clime-group repo
    :help "Manage package repositories"

    (clime-command add
      :help "Add a repository"
      (clime-arg name :help "Repository name")
      (clime-arg url :help "Repository URL")

      ;; [29] Zip group — paired options must have equal cardinality
      (clime-zip headers
        (clime-opt header-name ("--header-name" "-H")
          :help "Custom header name")
        (clime-opt header-value ("--header-value" "-V")
          :help "Custom header value"))

      (clime-handler (ctx)
        (clime-let ctx (name url headers)
          (format "Added repository %s → %s%s"
                  name url
                  (if headers
                      (format " [headers: %s]"
                              (mapconcat (lambda (row)
                                           (format "%s: %s"
                                                   (cdr (assq 'header-name row))
                                                   (cdr (assq 'header-value row))))
                                         headers ", "))
                    "")))))

    (clime-command list
      :help "List configured repositories"
      (clime-handler (_ctx)
        "Repositories:\n  default  https://elpa.gnu.org\n  melpa    https://melpa.org"))

    (clime-command remove
      :help "Remove a repository"
      :aliases (rm)
      (clime-arg name :help "Repository name")
      (clime-opt force ("--force" "-f") :from force-flag  ; [19] reused
                 :help "Remove even if packages depend on it")
      (clime-handler (ctx)
        (clime-let ctx (name force)
          (format "Removed repository %s%s"
                  name (if force " [forced]" ""))))))

  ;; ── config (group with handler) ──────────────────────────────────────
  ;; [14] Group handler — invoked when no subcommand is given
  (clime-group config
    :help "View or modify configuration"

    (clime-handler (_ctx)
      "Configuration:\n  registry = default\n  cache    = ~/.pkm/cache\n  timeout  = 30s")

    (clime-command get
      :help "Get a config value"
      (clime-arg key :help "Config key")
      (clime-handler (ctx)
        (clime-let ctx (key)
          (format "%s = %s" key
                  (pcase key
                    ("registry" "default")
                    ("cache" "~/.pkm/cache")
                    ("timeout" "30s")
                    (_ "(not set)"))))))

    (clime-command set
      :help "Set a config value"
      (clime-arg key :help "Config key")
      (clime-arg value :help "New value")
      (clime-handler (ctx)
        (clime-let ctx (key value)
          (format "Set %s = %s" key value)))))

  ;; ── project (group with args, options, commands, subgroups, handler) ─
  ;; [30] Group demonstrating every structural feature at once:
  ;;   - Group arg: required positional scoping all subcommands
  ;;   - Group option: --path for project root
  ;;   - Group handler: invoked when no subcommand given (shows summary)
  ;;   - Leaf commands: init, info
  ;;   - Nested subgroup: deps (with add/remove)
  (clime-group project
    :help "Manage projects"

    (clime-arg name :help "Project name")

    (clime-opt path ("--path" "-p")
      :help "Project root directory"
      :default ".")

    (clime-opt profile ("--profile" "-P")
      :help "Build profile"
      :choices '("dev" "release" "test"))

    (clime-handler (ctx)
      (clime-let ctx (name path profile)
        (format "Project %s at %s%s\n  status: ok\n  packages: 12"
                name path
                (if profile (format " [%s]" profile) ""))))

    (clime-command init
      :help "Initialize a new project"
      (clime-opt template ("--template" "-t")
        :help "Project template"
        :choices '("basic" "library" "application")
        :default "basic")
      (clime-handler (ctx)
        (clime-let ctx (name path template)
          (format "Initialized project %s at %s (template=%s)" name path template))))

    (clime-command info
      :help "Show project details"
      (clime-opt full ("--full") :bool
                 :help "Show extended information")
      (clime-handler (ctx)
        (clime-let ctx (name path full)
          (format "Project: %s\nPath: %s\nFull: %s" name path (if full "yes" "no")))))

    (clime-group deps
      :help "Manage project dependencies"

      (clime-handler (ctx)
        (clime-let ctx (name)
          (format "Dependencies for %s:\n  clime 0.4.0\n  dash 2.19.1" name)))

      (clime-command add
        :help "Add a dependency"
        (clime-arg package :help "Package to add")
        (clime-opt dev ("--dev" "-D") :bool
                   :help "Add as dev dependency")
        (clime-handler (ctx)
          (clime-let ctx (name package dev)
            (format "Added %s to %s%s" package name (if dev " [dev]" "")))))

      (clime-command remove
        :help "Remove a dependency"
        :aliases (rm)
        (clime-arg package :help "Package to remove")
        (clime-opt force ("--force" "-f") :from force-flag
                   :help "Remove even if other packages depend on it")
        (clime-handler (ctx)
          (clime-let ctx (name package force)
            (format "Removed %s from %s%s" package name (if force " [forced]" "")))))))

  ;; ── admin (inline group with category) ──────────────────────────────
  ;; [18] Inline group with :category — option and command share a section
  (clime-group admin
    :inline
    :category "Admin"
    :help "Administrative operations"

    (clime-opt admin-token ("--admin-token")
      :help "Admin API token")

    (clime-command gc
      :help "Run garbage collection on cache"
      (clime-handler (_ctx)
        "Garbage collecting cache...")))

  ;; ── debug (hidden) ───────────────────────────────────────────────────
  ;; [15] Hidden command — not shown in help, but still callable
  (clime-command debug
    :help "Dump internal state"
    :hidden
    (clime-handler (ctx)
      (clime-let ctx (verbose internal-trace)
        (format "App: pkm v%s\nVerbosity: %s\nTrace: %s"
                (clime-app-version (clime-context-app ctx))
                (or verbose 0)
                (if internal-trace "on" "off")))))

  ;; ── shortcuts (alias-for) ─────────────────────────────────────────────
  ;; [20] Command alias — expose nested commands at top level without
  ;; duplicating args, options, or handler
  (clime-group shortcuts :inline :category "Shortcuts"
               (clime-alias-for add-repo (repo add)
                 :help "Add a repository (shortcut)")
               (clime-alias-for rm-repo (repo remove)
                 :help "Remove a repository (shortcut)")))

(provide 'pkm)
;;; pkm.el ends here
