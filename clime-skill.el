;;; clime-skill.el --- Export a clime app as a portable Agent Skill  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; A `clime-app' already declares the full command tree an agent needs to
;; operate the CLI: nested commands, positional arguments, options and
;; their aliases, requiredness, multiplicity, choices, types, and help.
;; This module projects that contract into a single, provider-neutral
;; Agent Skills package headed by `SKILL.md'.
;;
;; Three layers, cleanly separated:
;;
;;   1. Projection  — `clime-skill-manifest' renders live declarations selected
;;      by the shared static-safe `clime-contract' policy into a manifest plist.  It
;;      never runs handlers, :setup, :config, or dotenv, never calls `getenv',
;;      and never resolves function-valued :default or :choices.  Declaration
;;      order is preserved so output is stable.
;;
;;   2. Rendering   — `clime-skill-render' deterministically renders the
;;      manifest to canonical `SKILL.md' text (YAML frontmatter with the
;;      open Agent Skills core keys `name' + `description', followed by a
;;      usage/discovery guide and a command reference).
;;
;;   3. Writing     — `clime-skill-write' materializes the package to a
;;      destination.  A temp directory is built and atomically renamed
;;      into place (no partial directory on failure); an existing
;;      destination fails closed unless :force is supplied.  Target
;;      adapters (`portable', `codex', `claude') affect only the install
;;      layout, never the rendered contract.
;;
;; Security invariant: only static declaration metadata is serialized.
;; Runtime values — resolved env/config/dotenv values, secrets, computed
;; defaults, invocation history, function bodies — are never touched.

;;; Code:

(require 'cl-lib)
(require 'clime-core)
(require 'clime-contract)
(require 'clime-param-type)

;;; ─── Safe formatters (security boundary) ───────────────────────────────

(defun clime-skill--first-line (text)
  "Return the first line of TEXT, or nil when TEXT is not a string.
Descriptions and help are collapsed to a single line so they remain
valid single-line scalars and never leak trailing declaration prose."
  (when (stringp text)
    (car (split-string text "\n"))))

;;; ─── Projection ────────────────────────────────────────────────────────

(defun clime-skill--sorted-flags (flags)
  "Return FLAGS with short spellings before long spellings."
  (let* ((flags flags)
         (short-p (lambda (f) (and (= (length f) 2) (= (aref f 0) ?-))))
         (shorts (cl-remove-if-not short-p flags))
         (longs (cl-remove-if short-p flags)))
    (append shorts longs)))

(defun clime-skill--nargs-plist (nargs)
  "Return a (:nargs N) fragment for NARGS, or nil for the default."
  (cond
   ((eq nargs :rest) (list :nargs :rest))
   ((integerp nargs) (list :nargs nargs))
   (t nil)))

(defun clime-skill--type-plist (type policy)
  "Return a (:type DESC) fragment for TYPE, or nil when opaque/absent."
  (let ((desc (apply #'clime-contract-describe-type type policy)))
    (when desc (list :type desc))))

(defun clime-skill--choices-plist (param policy)
  "Return a (:choices VALUE) fragment for CHOICES, or nil."
  (let ((c (apply #'clime-contract-safe-choices param policy)))
    (when (eq c 'dynamic) (setq c 'runtime))
    (when c (list :choices c))))

(defun clime-skill--default-plist (param policy)
  "Return a (:default VALUE) fragment from PARAM, or nil."
  (when-let ((default (apply #'clime-contract-safe-default param policy)))
    (list :default default)))

(defun clime-skill--project-arg (arg policy)
  "Render declaration ARG into a manifest entry plist."
  (append
   (list :name (symbol-name (clime-arg-name arg))
         :required (and (clime-arg-required arg) t)
         :help (clime-skill--first-line (clime-arg-help arg)))
   (clime-skill--type-plist (clime-arg-type arg) policy)
   (clime-skill--choices-plist arg policy)
   (clime-skill--nargs-plist (clime-arg-nargs arg))
   (clime-skill--default-plist arg policy)
   (when (clime-arg-deprecated arg) (list :deprecated t))))

(defun clime-skill--project-option (option app policy)
  "Render declaration OPTION into a manifest entry plist."
  (append
   (list :name (symbol-name (clime-option-name option))
         :flags (clime-skill--sorted-flags (clime-option-flags option))
         :required (and (clime-option-required option) t)
         :help (clime-skill--first-line (clime-option-help option)))
   (when (clime-option-boolean-p option) (list :boolean t))
   (when (clime-option-count option) (list :count t))
   (when (clime-option-multiple option) (list :multiple t))
   (clime-skill--nargs-plist (clime-option-nargs option))
   (clime-skill--type-plist (clime-option-type option) policy)
   (clime-skill--choices-plist option policy)
   (let ((env (clime--env-var-for-option option app)))
     (when env (list :env env)))
   (clime-skill--default-plist option policy)
   (when (clime-option-deprecated option) (list :deprecated t))))

(defun clime-skill--project-node (entry app policy nodes)
  "Render contract ENTRY and its subtree as a manifest node."
  (let* ((node (car entry))
         (kind (cond ((clime-alias-p node) 'alias)
                     ((clime-command-p node) 'command)
                     ((clime-app-p node) 'app)
                     (t 'group))))
    (append
     (list :kind (if (eq kind 'alias) 'command kind)
           :name (clime-node-name node)
           :path (cdr entry)
           :help (clime-skill--first-line (clime-node-help node))
           :arguments (mapcar (lambda (arg) (clime-skill--project-arg arg policy))
                              (clime-node-args node))
           :options (mapcar (lambda (option)
                              (clime-skill--project-option option app policy))
                            (apply #'clime-contract-options node policy)))
     (when (clime-node-examples node)
       (list :examples (clime-node-examples node)))
     (when (clime-node-deprecated node)
       (list :deprecated t))
     (when (clime-branch-p node)
       (list :commands
             (mapcar (lambda (child)
                       (clime-skill--project-node child app policy nodes))
                     (delq nil
                           (mapcar (lambda (child) (assq (cdr child) nodes))
                                   (clime-group-children node)))))))))

(defconst clime-skill--name-max 64
  "Maximum `name' length permitted by the Agent Skills specification.")

(defconst clime-skill--description-max 1024
  "Maximum `description' length permitted by the Agent Skills spec.")

(defun clime-skill--valid-name-p (s)
  "Return non-nil when S is a valid Agent Skills name/directory token.
Lowercase letters, digits, and single interior hyphens; 1-64 chars.
This is exactly the shape `clime-skill--sanitize-name' produces, so a
sanitized name always passes and a caller-chosen directory basename can
be checked against the same rule."
  (and (stringp s)
       (<= (length s) clime-skill--name-max)
       ;; case-fold-search defaults to t in batch, which would let
       ;; [a-z0-9] match uppercase; force a case-sensitive match.
       (let ((case-fold-search nil))
         (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" s))))

(defun clime-skill--sanitize-name (name)
  "Sanitize NAME into an Agent Skills directory/name token.
Lowercases, collapses non-alphanumeric runs to single hyphens, trims
leading/trailing hyphens, and clamps to the 1-64 char bound the Agent
Skills specification requires (the token must equal the skill
directory name).  Signals when nothing remains."
  (let* ((s (downcase (format "%s" name)))
         (s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
         (s (replace-regexp-in-string "\\`-+\\|-+\\'" "" s))
         ;; Clamp to the spec bound, then re-trim so truncation never
         ;; leaves a trailing hyphen (e.g. "aaa----" -> "aaa").
         (s (if (> (length s) clime-skill--name-max)
                (replace-regexp-in-string
                 "-+\\'" "" (substring s 0 clime-skill--name-max))
              s)))
    (when (string-empty-p s)
      (signal 'clime-usage-error
              (list (format "cannot derive a skill name from %S" name))))
    s))

(defun clime-skill--description (help command)
  "Return a spec-conformant skill description from HELP for COMMAND.
The Agent Skills specification requires a non-empty `description' of at
most 1024 characters.  When HELP is nil or blank a trigger-capable
fallback is synthesized so an agent still learns what the skill is and
when it applies.  The result is the first line only and is clamped to
the length bound."
  (let* ((first (clime-skill--first-line help))
         (desc (if (and first (not (string-empty-p (string-trim first))))
                   first
                 (format (concat "Command-line interface for %s.  Use this "
                                 "skill to discover and invoke its commands, "
                                 "options, and arguments.")
                         command))))
    (if (> (length desc) clime-skill--description-max)
        (substring desc 0 clime-skill--description-max)
      desc)))

(defconst clime-skill--command-token-re
  "\\`\\(?:\\./\\|/\\)?[A-Za-z0-9][A-Za-z0-9._/-]*\\'"
  "Strict grammar for a safe executable spelling.
An optional leading `./' or `/', then an alphanumeric, then the
path-safe set (letters, digits, `.', `_', `-', `/').  Rejects
whitespace and every shell/Markdown metacharacter — `;', backtick,
`$', `(', `)', `|', `&', `>', `<', quotes, `*', `#' — so the rendered
spelling can never smuggle a command substitution or redirection.")

(defun clime-skill--command-spelling (app command)
  "Resolve the executable spelling for APP, preferring COMMAND.
Falls back to the app's argv0 or name.  The resolved spelling must be a
single token matching `clime-skill--command-token-re'; anything with
whitespace or shell/Markdown metacharacters is rejected — whether it
was derived or supplied via COMMAND — since it is rendered into shell
example lines agents may copy verbatim."
  (let ((spelling (or command
                      (clime-app-argv0 app)
                      (clime-node-name app))))
    (unless (and (stringp spelling)
                 (string-match-p clime-skill--command-token-re spelling))
      (signal 'clime-usage-error
              (list (format
                     (concat "unsafe command spelling %S: pass --command with "
                             "a plain executable token (letters, digits, and "
                             "`._-/' only)")
                     spelling))))
    spelling))

(defun clime-skill-manifest (app &rest keys)
  "Project APP into a provider-neutral skill manifest plist.
KEYS accepts :command CMD — the executable spelling agents should type.

The returned plist has keys :name (sanitized skill/directory name),
:command (invocation spelling), :version, :description, and :root (the
projected node tree).  Pure: no handler, setup, config, dotenv, getenv,
or function-valued default/choices is ever executed."
  (let* ((command (plist-get keys :command))
         (name (clime-skill--sanitize-name (clime-node-name app)))
         (spelling (clime-skill--command-spelling app command))
         (policy '(:surface cli :tree-mode declaration
                   :visibility public :value-mode static-safe))
         (nodes (apply #'clime-contract-nodes app policy)))
    (list :name name
          :command spelling
          :version (clime-app-version app)
          :description (clime-skill--description (clime-node-help app) spelling)
          :root (clime-skill--project-node (car nodes) app policy nodes))))

;;; ─── Rendering ─────────────────────────────────────────────────────────

(defconst clime-skill--yaml-ambiguous-re
  (concat "\\`\\(?:"
          "[+-]?[0-9][0-9_]*\\(?:\\.[0-9_]*\\)?"      ; int / float
          "\\|true\\|false\\|yes\\|no\\|on\\|off\\|null\\|y\\|n"
          "\\)\\'")
  "Matches (case-insensitively) a plain scalar YAML would retype.
Booleans, null, and numbers must be quoted so a `name'/`description'
stays a string rather than being parsed as a boolean or number.")

(defun clime-skill--yaml-scalar (s)
  "Render S as a single-line YAML flow scalar, always string-typed.
Emits S plain only when it is unambiguously a plain string scalar;
otherwise double-quotes and escapes it.  A value YAML would coerce to a
boolean, null, or number (e.g. \"true\", \"123\") is quoted so its type
is preserved.  S is assumed newline-free (see `clime-skill--first-line')."
  (let ((s (or s "")))
    (if (and (not (string-empty-p s))
             (string-match-p "\\`[A-Za-z0-9]" s)
             (not (string-match-p "[:#\n]" s))
             (not (string-match-p clime-skill--yaml-ambiguous-re (downcase s))))
        s
      (concat "\""
              (replace-regexp-in-string
               "\"" "\\\\\""
               (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
              "\""))))

(defun clime-skill--fmt-literal (val)
  "Format a safe literal VAL for human-readable display."
  (cond
   ((eq val t) "true")
   ((stringp val) val)
   ((and (proper-list-p val) val)
    (mapconcat #'clime-skill--fmt-literal val ", "))
   (t (format "%s" val))))

(defun clime-skill--annotations (entry)
  "Return a parenthesized annotation string for ENTRY, or nil.
Covers repeatability, type, choices, default, env, and deprecation in a
fixed order.  Requiredness is rendered by the caller."
  (let (parts)
    (when (plist-get entry :count)
      (push "repeatable (accumulates)" parts))
    (when (plist-get entry :multiple)
      (push "repeatable" parts))
    (let ((type (plist-get entry :type)))
      (when (and type (not (equal type "string")))
        (push (format "type: %s" type) parts)))
    (let ((c (plist-get entry :choices)))
      (cond
       ((eq c 'runtime) (push "choices: determined at runtime" parts))
       (c (push (format "choices: %s"
                        (mapconcat (lambda (x) (format "%s" x)) c ", "))
                parts))))
    (when (plist-member entry :default)
      (push (format "default: %s"
                    (clime-skill--fmt-literal (plist-get entry :default)))
            parts))
    (let ((env (plist-get entry :env)))
      (when env (push (format "env: $%s" env) parts)))
    (when (plist-get entry :deprecated)
      (push "deprecated" parts))
    (when parts
      (concat "(" (mapconcat #'identity (nreverse parts) ") (") ")"))))

(defun clime-skill--render-option (opt)
  "Render a single option manifest entry OPT as a Markdown list item."
  (let* ((flags (mapconcat #'identity (plist-get opt :flags) ", "))
         (valpart (cond
                   ((plist-get opt :boolean) "")
                   ((eq (plist-get opt :nargs) :rest) " <value>...")
                   ((integerp (plist-get opt :nargs))
                    (concat " " (mapconcat #'identity
                                           (make-list (plist-get opt :nargs)
                                                      "<value>")
                                           " ")))
                   (t " <value>")))
         (req (when (plist-get opt :required) "(required)"))
         (annots (clime-skill--annotations opt))
         (tail (mapconcat #'identity (delq nil (list req annots)) " "))
         (help (or (plist-get opt :help) "")))
    (concat "- `" flags valpart "`"
            (unless (string-empty-p tail) (concat " " tail))
            (unless (string-empty-p help) (concat " — " help)))))

(defun clime-skill--render-arg (arg)
  "Render a single positional argument manifest entry ARG."
  (let* ((nargs (plist-get arg :nargs))
         (name (plist-get arg :name))
         (label (cond
                 ((eq nargs :rest) (concat "<" name "...>"))
                 ((integerp nargs)
                  (mapconcat #'identity
                             (make-list nargs (concat "<" name ">")) " "))
                 (t (concat "<" name ">"))))
         (opt (unless (plist-get arg :required) "(optional)"))
         (annots (clime-skill--annotations arg))
         (tail (mapconcat #'identity (delq nil (list opt annots)) " "))
         (help (or (plist-get arg :help) "")))
    (concat "- `" label "`"
            (unless (string-empty-p tail) (concat " " tail))
            (unless (string-empty-p help) (concat " — " help)))))

(defun clime-skill--render-examples (examples)
  "Render EXAMPLES (declared invocation examples) as a Markdown block."
  (concat "\nExamples:\n\n"
          (mapconcat
           (lambda (ex)
             (let ((inv (cond ((consp ex) (car ex)) (t ex))))
               (concat "- `" (format "%s" inv) "`")))
           examples "\n")
          "\n"))

(defun clime-skill--render-command-tree (node cmd)
  "Render NODE and its subcommands.  CMD is the invocation spelling."
  (let* ((path (plist-get node :path))
         (full (concat cmd (when path (concat " " (string-join path " ")))))
         (kind (plist-get node :kind))
         (parts (list (concat "\n### `" full "`\n"))))
    (when (plist-get node :help)
      (push (concat "\n" (plist-get node :help) "\n") parts))
    (when (eq kind 'group)
      (push "\nCommand group; see the subcommands below.\n" parts))
    (when (plist-get node :arguments)
      (push (concat "\nArguments:\n\n"
                    (mapconcat #'clime-skill--render-arg
                               (plist-get node :arguments) "\n")
                    "\n")
            parts))
    (when (plist-get node :options)
      (push (concat "\nOptions:\n\n"
                    (mapconcat #'clime-skill--render-option
                               (plist-get node :options) "\n")
                    "\n")
            parts))
    (when (plist-get node :examples)
      (push (clime-skill--render-examples (plist-get node :examples)) parts))
    (concat (apply #'concat (nreverse parts))
            (mapconcat (lambda (c) (clime-skill--render-command-tree c cmd))
                       (plist-get node :commands) ""))))

(defun clime-skill--render-usage (cmd)
  "Render the usage/discovery section for invocation spelling CMD."
  (concat
   "\n## Usage\n\n"
   "Invoke commands as `" cmd
   " [GLOBAL-OPTIONS] GROUP... COMMAND [OPTIONS] [ARGUMENTS]`.\n\n"
   "- Options belong to the level that declares them.  Root options are"
   " global and may appear anywhere before `--`.  A group's or command's"
   " options become available once that group/command name has appeared on"
   " the line and stay in scope for the rest of it; place each subcommand's"
   " options after the subcommand name.\n"
   "- A literal `--` ends option parsing; every token after it is treated"
   " as a positional argument.\n"
   "- `<name>` marks a required positional argument; `<name>...` accepts"
   " one or more values; a fixed-arity value is shown as repeated"
   " `<name>` placeholders; items flagged `(optional)` may be omitted.\n"
   "- A `repeatable` value option may be given more than once"
   " (`--opt x --opt y`); at a command with no positional arguments its"
   " values may also be supplied space-separated (`--opt a b c`).  A"
   " `repeatable (accumulates)` counter increases each time it is repeated"
   " (e.g. `-vvv`).\n"
   "- Exit codes: `0` success, `2` usage error, `1` runtime error.\n"))

(defun clime-skill-render (manifest)
  "Render MANIFEST into canonical `SKILL.md' text.
Deterministic: identical MANIFEST input yields byte-identical output."
  (let* ((root (plist-get manifest :root))
         (cmd (plist-get manifest :command))
         (out (list)))
    (push (concat "---\n"
                  "name: "
                  (clime-skill--yaml-scalar (plist-get manifest :name)) "\n"
                  "description: "
                  (clime-skill--yaml-scalar (plist-get manifest :description))
                  "\n"
                  "---\n")
          out)
    (push (concat "\n# " cmd "\n") out)
    (when (plist-get root :help)
      (push (concat "\n" (plist-get root :help) "\n") out))
    (when (plist-get manifest :version)
      (push (concat "\nVersion: " (plist-get manifest :version) "\n") out))
    (push (clime-skill--render-usage cmd) out)
    (when (plist-get root :options)
      (push (concat "\n## Global options\n\n"
                    "Accepted with any command:\n\n"
                    (mapconcat #'clime-skill--render-option
                               (plist-get root :options) "\n")
                    "\n")
            out))
    (when (plist-get root :arguments)
      (push (concat "\n## Arguments\n\n"
                    (mapconcat #'clime-skill--render-arg
                               (plist-get root :arguments) "\n")
                    "\n")
            out))
    (when (plist-get root :examples)
      (push (clime-skill--render-examples (plist-get root :examples)) out))
    (push "\n## Commands\n" out)
    (dolist (child (plist-get root :commands))
      (push (clime-skill--render-command-tree child cmd) out))
    (apply #'concat (nreverse out))))

;;; ─── Writing / targets ─────────────────────────────────────────────────

(defun clime-skill--destination (target output name)
  "Return the absolute destination directory for TARGET.
NAME is the sanitized skill name.  OUTPUT is the user-selected base:
required for `portable' (it is the skill directory), and the layout
root for `codex' / `claude' (defaulting to `default-directory')."
  (pcase target
    ('portable
     (unless output
       (signal 'clime-usage-error
               (list "portable target requires --output DIR")))
     (directory-file-name (expand-file-name output)))
    ('codex
     (directory-file-name
      (expand-file-name (format ".agents/skills/%s" name)
                        (or output default-directory))))
    ('claude
     (directory-file-name
      (expand-file-name (format ".claude/skills/%s" name)
                        (or output default-directory))))
    (_ (signal 'clime-usage-error
               (list (format "unknown target: %s (portable|codex|claude)"
                             target))))))

(defun clime-skill-write (manifest &rest keys)
  "Render MANIFEST and write the skill package to disk; return the dir.
KEYS: :target (portable|codex|claude, default portable), :output DIR,
:force BOOL.

Atomic: content is built in a sibling temp directory and renamed into
place, so a failure leaves no partial destination.  An existing
destination fails closed unless :force is non-nil.

The rendered `name' is reconciled to equal the destination directory
basename, as the Agent Skills specification requires.  The `codex' and
`claude' layouts build the directory from the manifest name, so it
already matches; a `portable' destination is caller-chosen, so its
basename must itself be a valid skill name (else this signals).

Forced replacement is rollback-safe, including under compound failure:
the previous destination is moved aside to a sibling backup, the new
package is renamed into place, and only then is the backup discarded.
If the install rename fails the backup is restored; if that restore ALSO
fails the backup is preserved (never deleted) and its recovery path is
reported, so a failed replace can never destroy the prior skill."
  (let* ((target (or (plist-get keys :target) 'portable))
         (output (plist-get keys :output))
         (force (plist-get keys :force))
         (name (plist-get manifest :name))
         (dest (clime-skill--destination target output name))
         (dir-name (file-name-nondirectory dest))
         (parent (directory-file-name (file-name-directory dest))))
    ;; Agent Skills conformance: name must equal the directory basename.
    (unless (clime-skill--valid-name-p dir-name)
      (signal 'clime-usage-error
              (list (format (concat "destination directory %S is not a valid "
                                    "Agent Skills name (lowercase letters, "
                                    "digits, single hyphens; 1-64 chars); "
                                    "rename --output or use --target codex|claude")
                            dir-name))))
    (when (and (file-exists-p dest) (not force))
      (signal 'clime-usage-error
              (list (format "destination exists: %s (use --force to replace)"
                            dest))))
    (let ((content (clime-skill-render
                    (if (equal dir-name name)
                        manifest
                      (plist-put (copy-sequence manifest) :name dir-name)))))
      (make-directory parent t)
      (let ((tmp (make-temp-file
                  (expand-file-name "clime-skill-" (file-name-as-directory parent))
                  t))
            (backup nil))
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region content nil (expand-file-name "SKILL.md" tmp)))
              (if (file-exists-p dest)
                  ;; Rollback-safe forced replace: move the old dir aside
                  ;; first so a failed install can restore it.
                  (progn
                    (setq backup (make-temp-name
                                  (expand-file-name
                                   "clime-skill-bak-"
                                   (file-name-as-directory parent))))
                    (rename-file (directory-file-name dest) backup)
                    (condition-case install-err
                        (progn
                          (rename-file (directory-file-name tmp) dest)
                          (setq tmp nil))
                      (error
                       ;; Install failed — try to restore the previous skill.
                       (condition-case _restore-err
                           (progn
                             (rename-file backup (directory-file-name dest))
                             ;; Restored: backup consumed, none to preserve.
                             (setq backup nil))
                         (error
                          ;; Restore ALSO failed — PRESERVE the backup (it is
                          ;; the only surviving copy) and report its path.
                          (signal 'clime-usage-error
                                  (list (format
                                         (concat "forced replace of %s failed and "
                                                 "the previous skill could not be "
                                                 "restored; it is preserved at %s "
                                                 "— move it back manually")
                                         dest backup)))))
                       ;; Restore succeeded — re-raise the install failure.
                       (signal (car install-err) (cdr install-err))))
                    ;; Commit: the new package is in place; drop the backup.
                    (when (and backup (file-exists-p backup))
                      (delete-directory backup t))
                    (setq backup nil))
                ;; Fresh install: no prior destination to preserve.
                (rename-file (directory-file-name tmp) dest)
                (setq tmp nil))
              dest)
          ;; Cleanup: only the temp build dir.  A surviving `backup' here
          ;; means restore failed and it is the user's sole copy — never
          ;; delete it.
          (when (and tmp (file-exists-p tmp))
            (delete-directory tmp t)))))))

(defun clime-skill-export (app &rest keys)
  "Project, render, and write APP as an Agent Skill; return the dir.
KEYS: :command CMD, :target (portable|codex|claude), :output DIR,
:force BOOL.  Convenience wrapper over `clime-skill-manifest' and
`clime-skill-write'."
  (let ((manifest (clime-skill-manifest app :command (plist-get keys :command))))
    (clime-skill-write manifest
                       :target (or (plist-get keys :target) 'portable)
                       :output (plist-get keys :output)
                       :force (plist-get keys :force))))

(provide 'clime-skill)
;;; clime-skill.el ends here
