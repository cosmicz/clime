#!/bin/sh
":"; S="$(realpath "$0")";D="$(dirname "$S")"; CLIME_ARGV0="$0" exec emacs --batch -Q -L "$D/.." --eval "(setq load-file-name \"$S\")" --eval "(with-temp-buffer(insert-file-contents load-file-name)(setq lexical-binding t)(goto-char(point-min))(condition-case nil(while t(eval(read(current-buffer))t))(end-of-file nil)))" -- "$@" # clime-sh!:v1 -*- mode: emacs-lisp; lexical-binding: t; -*-
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
;;  22.  Mutual exclusion        --format-table/--format-csv via :mutex
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
  :epilog "Examples:
  pkm install foo --tag dev
  pkm -vv search
  pkm repo add myrepo https://example.com"

  ;; ── Root Options ─────────────────────────────────────────────────────
  ;; [1] Count flag — stackable verbosity (-v, -vv, -vvv)
  (clime-option verbose ("-v" "--verbose") :count
    :help "Increase output verbosity")

  ;; [2] Boolean flag — :bool is shorthand for :nargs 0
  (clime-option quiet ("-q" "--quiet") :bool
    :help "Suppress non-essential output")

  ;; [3] Hidden option — internal use only, omitted from --help
  (clime-option internal-trace ("--internal-trace") :bool :hidden
    :help "Enable internal tracing")

  ;; [4] Output format — explicit clime-output-format with --json flag
  (clime-output-format json ("--json")
    :help "Output as JSON")

  ;; [21] Negatable flag — --color / --no-color with ternary state
  (clime-option color ("--color") :negatable :default t
    :help "Colorize output")

  ;; ── install ──────────────────────────────────────────────────────────
  ;; [5] Command with required arg, [6] symbol alias, [7] booleans,
  ;; [8] multiple option, [9] default value, [16] clime-let
  (clime-command install
    :help "Install a package into the project"
    :aliases (i)                                ; [6] symbol alias

    (clime-arg package :help "Package name or URL")

    (clime-option force ("--force" "-f") :from force-flag  ; [19] from template
      :help "Overwrite existing installation")

    (clime-option dry-run ("--dry-run" "-n") :bool
      :help "Show what would be installed without doing it")

    (clime-option tag ("--tag" "-t") :multiple
      :help "Add tag to installed package")

    (clime-option registry ("--registry" "-r")
      :help "Registry to install from"
      :default "default")

    (clime-handler (ctx)
      (clime-let ctx (package force dry-run (tags tag) (reg registry))
        (format "Installing %s from %s%s%s%s"
                package reg
                (if force " [forced]" "")
                (if dry-run " [dry-run]" "")
                (if tags (format " tags=%s" (string-join tags ",")) "")))))

  ;; ── search ───────────────────────────────────────────────────────────
  ;; [10] Optional positional arg, [6] symbol alias, [17] command category
  (clime-command search
    :help "Search the package registry"
    :aliases (s)
    :category "Discovery"

    (clime-arg query :required nil
      :help "Search query (omit to list all)")

    (clime-option limit ("--limit" "-l")
      :help "Max results to return"
      :default "20")

    (clime-handler (ctx)
      (clime-let ctx (query limit)
        (if query
            (format "Searching for \"%s\" (limit %s)" query limit)
          (format "Listing all packages (limit %s)" limit)))))

  ;; ── list ─────────────────────────────────────────────────────────────
  ;; [11] Option groups in help, [6] symbol alias, [17] command category,
  ;; [22] mutex for format, [23] deprecated option
  (clime-command list
    :help "List installed packages"
    :aliases (ls)
    :category "Discovery"

    (clime-option author ("--author" "-a")
      :help "Filter by author"
      :category "Filter")

    (clime-option since ("--since")
      :help "Only packages installed after DATE"
      :category "Filter")

    (clime-option sort ("--sort" "-s")
      :help "Sort field"
      :choices '("name" "date" "size")
      :default "name")

    ;; [22] Mutually exclusive output format options
    (clime-option format-table ("--table") :bool :mutex 'list-format
      :help "Output as table (default)"
      :category "Format")

    (clime-option format-csv ("--csv") :bool :mutex 'list-format
      :help "Output as CSV"
      :category "Format")

    ;; [23] Deprecated option with migration hint
    (clime-option output ("--output" "-o")
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

    (clime-handler (ctx)
      (clime-let ctx (script args)
        (format "Running script %s%s"
                script
                (if args (format " with args: %s" (string-join args " ")) "")))))

  ;; ── repo (nested group) ──────────────────────────────────────────────
  ;; [13] Nested group with subcommands
  (clime-group repo
    :help "Manage package repositories"

    (clime-command add
      :help "Add a repository"
      (clime-arg name :help "Repository name")
      (clime-arg url :help "Repository URL")
      (clime-handler (ctx)
        (clime-let ctx (name url)
          (format "Added repository %s → %s" name url))))

    (clime-command list
      :help "List configured repositories"
      (clime-handler (_ctx)
        "Repositories:\n  default  https://elpa.gnu.org\n  melpa    https://melpa.org"))

    (clime-command remove
      :help "Remove a repository"
      :aliases (rm)
      (clime-arg name :help "Repository name")
      (clime-option force ("--force" "-f") :from force-flag  ; [19] reused
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

  ;; ── admin (inline group with category) ──────────────────────────────
  ;; [18] Inline group with :category — option and command share a section
  (clime-group admin
    :inline
    :category "Admin"
    :help "Administrative operations"

    (clime-option admin-token ("--admin-token")
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
;;; Entrypoint:
(clime-run-batch pkm)
;;; pkm.el ends here
