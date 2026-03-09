#!/bin/sh
":"; exec emacs --batch -Q -L "$(dirname "$0")/.." -l "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
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
;;   2.  Root boolean flag       -q/--quiet (:flag t shorthand)
;;   3.  Hidden option           --internal-trace (omitted from help)
;;   4.  JSON mode               :json-mode t → auto-injected --json
;;   5.  Command with args       install <package>
;;   6.  Aliases                 install → i, search → s, list → ls (symbols)
;;   7.  Boolean flags           --force, --dry-run (:flag t shorthand)
;;   8.  Multiple option         --tag (repeatable) on install
;;   9.  Default value           --registry defaults to "default"
;;  10.  Optional arg            search [query]
;;  11.  Option groups in help   "Filter" group on list (--author, --since)
;;  12.  Rest args               run <script> [args...]
;;  13.  Nested group            repo add/list/remove
;;  14.  Group handler           config (shows all) / config set|get
;;  15.  Hidden command          debug
;;  16.  Context destructuring   clime-let macro in handlers
;;
;; Run it (this file is directly executable via the shebang header):
;;   chmod +x examples/pkm.el
;;   ./examples/pkm.el --help
;;   ./examples/pkm.el install foo --tag dev
;;   ./examples/pkm.el -vv search
;;   ./examples/pkm.el config
;;   ./examples/pkm.el repo add myrepo https://example.com
;;
;; Or without the shebang:
;;   emacs --batch -Q -L /path/to/clime -l examples/pkm.el -- --help
;;
;; The shebang uses a relative -L path since this file lives inside
;; the clime repo.  For external apps, use `clime-make.el init' which
;; generates an absolute path.  See DEVELOPMENT.org for details.

;;; Code:

(require 'clime)

;; ─── App Definition ──────────────────────────────────────────────────────

(clime-app pkm
  :version "0.5.0"
  :help "A package manager for Emacs Lisp projects."
  :json-mode t                                  ; [4] auto-injects --json
  :epilog "Examples:
  pkm install foo --tag dev
  pkm -vv search
  pkm repo add myrepo https://example.com"

  ;; ── Root Options ─────────────────────────────────────────────────────
  ;; [1] Count flag — stackable verbosity (-v, -vv, -vvv)
  (clime-option verbose ("-v" "--verbose") :count t
    :help "Increase output verbosity")

  ;; [2] Boolean flag — :flag t is shorthand for :nargs 0
  (clime-option quiet ("-q" "--quiet") :flag t
    :help "Suppress non-essential output")

  ;; [3] Hidden option — internal use only, omitted from --help
  (clime-option internal-trace ("--internal-trace") :flag t
    :help "Enable internal tracing" :hidden t)

  ;; ── install ──────────────────────────────────────────────────────────
  ;; [5] Command with required arg, [6] symbol alias, [7] booleans,
  ;; [8] multiple option, [9] default value, [16] clime-let
  (clime-command install
    :help "Install a package into the project"
    :aliases (i)                                ; [6] symbol alias

    (clime-arg package :help "Package name or URL")

    (clime-option force ("--force" "-f") :flag t  ; [7] :flag t shorthand
      :help "Overwrite existing installation")

    (clime-option dry-run ("--dry-run" "-n") :flag t
      :help "Show what would be installed without doing it")

    (clime-option tag ("--tag" "-t") :multiple t
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
  ;; [10] Optional positional arg, [6] symbol alias
  (clime-command search
    :help "Search the package registry"
    :aliases (s)

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
  ;; [11] Option groups in help, [6] symbol alias
  (clime-command list
    :help "List installed packages"
    :aliases (ls)

    (clime-option author ("--author" "-a")
      :help "Filter by author"
      :group "Filter")

    (clime-option since ("--since")
      :help "Only packages installed after DATE"
      :group "Filter")

    (clime-option sort ("--sort" "-s")
      :help "Sort field"
      :choices '("name" "date" "size")
      :default "name")

    (clime-handler (ctx)
      (clime-let ctx (author since sort)
        (format "Listing packages (sort=%s%s%s)"
                sort
                (if author (format ", author=%s" author) "")
                (if since (format ", since=%s" since) "")))))

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
      (clime-option force ("--force" "-f") :flag t
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

  ;; ── debug (hidden) ───────────────────────────────────────────────────
  ;; [15] Hidden command — not shown in help, but still callable
  (clime-command debug
    :help "Dump internal state"
    :hidden t
    (clime-handler (ctx)
      (clime-let ctx (verbose internal-trace)
        (format "App: pkm v%s\nVerbosity: %s\nTrace: %s"
                (clime-app-version (clime-context-app ctx))
                (or verbose 0)
                (if internal-trace "on" "off"))))))

;; ─── Batch Entry Point ───────────────────────────────────────────────────

(clime-run-batch pkm)

(provide 'pkm)
;;; pkm.el ends here
