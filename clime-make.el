;;; clime-make.el --- CLI tool for the clime framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Command-line interface for clime itself.  Provides:
;;   `init'      — add shebang to an Elisp file
;;   `bundle'    — concatenate multiple source files into a single distributable
;;   `scaffold'  — insert ;;; Entrypoint: boilerplate into an Elisp file
;;   `quickstart' — scaffold + init in one shot (auto CLIME_MAIN_APP)
;;
;; Usage:
;;   ./clime-make.el init myapp.el
;;   ./clime-make.el init myapp.el -L ./lib
;;   ./clime-make.el scaffold myapp.el
;;   ./clime-make.el quickstart myapp.el --self-dir
;;   ./clime-make.el bundle -o bundle.el --provide mylib src/*.el
;;   ./clime-make.el --help

;;; Code:

(require 'clime)

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defconst clime-make--env-var-re
  "\\`[A-Za-z_][A-Za-z0-9_]*=[-./:@A-Za-z0-9_]*\\'"
  "Regexp matching safe NAME=VALUE env var assignments for shebang.")

(defun clime-make--validate-env-vars (env-vars)
  "Validate each entry in ENV-VARS matches safe NAME=VALUE format.
Signals `clime-usage-error' on the first invalid entry."
  (dolist (e env-vars)
    (unless (string-match-p clime-make--env-var-re e)
      (signal 'clime-usage-error
              (list (format "invalid --env value %S (expected NAME=VALUE with safe characters)" e))))))


(defconst clime-make--shebang-version "1"
  "Shebang format version, independent of `clime-version'.
Bumped when the polyglot launcher structure changes.")

(defconst clime-make--shebang-tag-re
  "# clime\\(?:-sh!:v[0-9]+\\|:[0-9]+\\.[0-9]+\\.[0-9]+\\)"
  "Regexp matching a clime shebang tag (old or new format) in line 2.
Matches both the legacy `# clime:X.Y.Z' and current `# clime-sh!:vN'.")

(defun clime-make--clime-shebang-p (file)
  "Return non-nil if FILE starts with a clime-tagged shebang."
  (with-temp-buffer
    (insert-file-contents file nil 0 512)
    (goto-char (point-min))
    (and (looking-at "#!")
         (forward-line 1)
         (let ((line2 (buffer-substring (point) (line-end-position))))
           (string-match-p clime-make--shebang-tag-re line2)))))

(defun clime-make--shebang-file-version (file)
  "Return the shebang format version from FILE, or nil.
Returns an integer for `clime-sh!:vN' tags, 0 for legacy `clime:X.Y.Z' tags,
or nil if FILE has no clime shebang."
  (with-temp-buffer
    (insert-file-contents file nil 0 512)
    (goto-char (point-min))
    (when (looking-at "#!")
      (forward-line 1)
      (let ((line2 (buffer-substring (point) (line-end-position))))
        (cond
         ((string-match "# clime-sh!:v\\([0-9]+\\)" line2)
          (string-to-number (match-string 1 line2)))
         ((string-match-p "# clime:[0-9]+\\.[0-9]+\\.[0-9]+" line2)
          0))))))

(defun clime-make--make-shebang (env-vars load-paths resolve)
  "Build a two-line polyglot shebang string.
ENV-VARS is a list of \"NAME=VALUE\" strings.
LOAD-PATHS is a string of formatted -L flags (may be empty).
When RESOLVE is non-nil, prepend symlink resolution via `realpath'
so relative load paths work through symlink chains.  In this mode,
load paths should use \"$D\" (resolved dirname) instead of
\"$(dirname \"$0\")\".
Always includes CLIME_ARGV0=\"$0\" so usage output shows the
executable name rather than the DSL symbol.
Embeds a clime-sh!:vN tag for detection and update support.

Uses --eval with a read/eval loop instead of -l to force
`lexical-binding' to t.  The standard polyglot (\":\" on line 2)
prevents Emacs from recognizing the -*-cookie, so we read forms
from a temp buffer with lexical-binding set and eval each with
the lexical flag."
  (let* ((all-vars (cons "CLIME_ARGV0=\"$0\"" (or env-vars '())))
         (env-prefix (concat (mapconcat #'identity all-vars " ") " "))
         (resolve-prefix (if resolve
                             "S=\"$(realpath \"$0\")\";D=\"$(dirname \"$S\")\"; "
                           ""))
         (load-file-expr (if resolve
                             "\\\"$S\\\""
                           "\\\"$0\\\""))
         (eval-form (concat
                     "--eval \"(setq load-file-name " load-file-expr ")\""
                     " --eval \"(with-temp-buffer"
                     "(insert-file-contents load-file-name)"
                     "(setq lexical-binding t)"
                     "(goto-char(point-min))"
                     "(condition-case nil"
                     "(while t(eval(read(current-buffer))t))"
                     "(end-of-file nil)))\"")))
    (format "#!/bin/sh\n\":\"; %s%sexec emacs --batch -Q%s %s -- \"$@\" # clime-sh!:v%s -*- mode: emacs-lisp; lexical-binding: t; -*-\n"
            resolve-prefix env-prefix load-paths eval-form clime-make--shebang-version)))

(defun clime-make--write-shebang (target env-vars load-paths force &optional resolve)
  "Write a shebang to TARGET file, handling existing headers.
ENV-VARS, LOAD-PATHS, and RESOLVE are passed to `clime-make--make-shebang'.
If TARGET has a clime-tagged shebang with a newer format version,
signal an error (use FORCE to override).
If TARGET has a clime-tagged shebang at same or older version,
replace it (return \"updated\").
If TARGET has a non-clime shebang and FORCE is non-nil, replace it.
If TARGET has a non-clime shebang and FORCE is nil, signal an error.
If TARGET has no shebang, prepend one (return \"done\")."
  (let ((shebang (clime-make--make-shebang env-vars load-paths resolve))
        (action nil))
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-min))
      (cond
       ;; Existing shebang
       ((looking-at "#!")
        (let ((file-ver (clime-make--shebang-file-version target))
              (our-ver (string-to-number clime-make--shebang-version)))
          (cond
           ;; Newer clime shebang — refuse unless forced
           ((and file-ver (> file-ver our-ver) (not force))
            (signal 'clime-usage-error
                    (list (format "%s has a newer shebang format (v%d > v%d); use --force to downgrade"
                                  (file-name-nondirectory target) file-ver our-ver))))
           ;; Clime shebang (same, older, or forced newer) — replace 2 lines
           (file-ver
            (forward-line 2)
            (delete-region (point-min) (point))
            (goto-char (point-min))
            (insert shebang)
            (setq action "updated"))
           ;; Non-clime shebang with force — replace only the #! line
           (force
            (forward-line 1)
            (delete-region (point-min) (point))
            (goto-char (point-min))
            (insert shebang)
            (setq action "updated"))
           ;; Non-clime shebang, no force
           (t
            (signal 'clime-usage-error
                    (list (format "%s already has a shebang line (use --force to replace)"
                                  (file-name-nondirectory target))))))))
       ;; No shebang: prepend
       (t
        (insert shebang)
        (setq action "done")))
      (write-region nil nil target))
    (set-file-modes target #o755)
    action))

;;; ─── Client Wrapper (emacsclient) ───────────────────────────────────

(defconst clime-make--client-wrapper-template "\
#!/usr/bin/env bash
set -euo pipefail
DIR=$(mktemp -d)
trap 'rm -rf \"$DIR\"' EXIT

# Parse emacsclient connection flags
EC_ARGS=()
APP_ARGS=()
while [[ $# -gt 0 ]]; do
  case \"$1\" in
    --socket-name) EC_ARGS+=(--socket-name \"$2\"); shift 2 ;;
    --server-file) EC_ARGS+=(--server-file \"$2\"); shift 2 ;;
    *) APP_ARGS+=(\"$1\"); shift ;;
  esac
done

# Env fallback for connection flags
[[ -n \"${EMACS_SOCKET_NAME:-}\" && ! \" ${EC_ARGS[*]:-} \" =~ \" --socket-name \" ]] && \\
  EC_ARGS+=(--socket-name \"$EMACS_SOCKET_NAME\")
[[ -n \"${EMACS_SERVER_FILE:-}\" && ! \" ${EC_ARGS[*]:-} \" =~ \" --server-file \" ]] && \\
  EC_ARGS+=(--server-file \"$EMACS_SERVER_FILE\")

# Write null-delimited args
printf '%%s\\0' \"${APP_ARGS[@]}\" > \"$DIR/argv\"

# Capture stdin if data is available on a non-tty fd
if [[ ! -t 0 ]] && read -t 0; then
  cat > \"$DIR/in\"
fi

# Dispatch via emacsclient
EC_OUT=$(emacsclient \"${EC_ARGS[@]}\" --eval \\
  \"(progn (dolist (p %s) (add-to-list 'load-path p)) (require 'clime-run) (clime-run-client '%s \\\"$DIR\\\" :load-path %s :file %s))\" \\
  2>&1) || {
  if [[ \"$EC_OUT\" == *\"connect\"* || \"$EC_OUT\" == *\"server file\"* || \"$EC_OUT\" == *\"No such file\"* ]]; then
    echo \"Error: Cannot connect to Emacs server\" >&2
    echo \"  Start a daemon with: emacs --daemon\" >&2
  else
    echo \"Error: $EC_OUT\" >&2
  fi
  exit 1
}

# Output results
[[ -s \"$DIR/out\" ]] && cat \"$DIR/out\"
[[ -s \"$DIR/err\" ]] && cat \"$DIR/err\" >&2
exit \"$(cat \"$DIR/exit\" 2>/dev/null || echo 1)\"
"
  "Template for the emacsclient bash wrapper.
Format args: LOAD-PATHS APP-SYM LOAD-PATHS APP-FILE.")

(defun clime-make--bash-escape-quotes (str)
  "Escape double quotes in STR for embedding in a bash double-quoted string."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun clime-make--make-client-wrapper (app-sym load-paths app-file)
  "Build a bash wrapper script for emacsclient invocation.
APP-SYM is the clime-app symbol name (a symbol).
LOAD-PATHS is a list of absolute directory path strings.
APP-FILE is the absolute path to the .el file defining the app.
Paths are escaped for embedding inside a bash double-quoted string."
  (let ((lp-form (clime-make--bash-escape-quotes
                  (if load-paths
                      (concat "'("
                              (mapconcat (lambda (p) (format "%S" p))
                                         load-paths " ")
                              ")")
                    "nil")))
        (file-form (clime-make--bash-escape-quotes
                    (format "%S" app-file))))
    (format clime-make--client-wrapper-template
            lp-form (symbol-name app-sym) lp-form file-form)))

(defun clime-make--write-client-wrapper (target app-sym load-paths app-file force)
  "Write an emacsclient wrapper to TARGET.
APP-SYM, LOAD-PATHS, APP-FILE are passed to `clime-make--make-client-wrapper'.
If TARGET exists and FORCE is nil, signal an error."
  (when (and (file-exists-p target) (not force))
    (signal 'clime-usage-error
            (list (format "%s already exists (use --force to replace)"
                          (file-name-nondirectory target)))))
  (let ((wrapper (clime-make--make-client-wrapper app-sym load-paths app-file)))
    (with-temp-file target
      (insert wrapper)))
  (set-file-modes target #o755)
  "done")

;;; ─── Code Extraction ────────────────────────────────────────────────

(defun clime-make--extract-code (file)
  "Extract library code from FILE using GNU-style section markers.
Returns text between `;;; Code:' and `;;; Entrypoint:' (or
`;;; ... ends here' if no entrypoint marker).  The entrypoint
section, if present, is excluded."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((start (and (re-search-forward "^;;; Code:" nil t)
                       (forward-line 1)
                       (point)))
           (end (when start
                  (if (re-search-forward "^;;; Entrypoint:" nil t)
                      (line-beginning-position)
                    (goto-char start)
                    (and (re-search-forward "^;;; .* ends here" nil t)
                         (line-beginning-position))))))
      (unless (and start end)
        (signal 'clime-usage-error
                (list (format "%s: missing ;;; Code: or ;;; ... ends here markers"
                              (file-name-nondirectory file)))))
      (buffer-substring start end))))

(defun clime-make--extract-entry (file)
  "Extract entrypoint code from FILE.
Returns text between `;;; Entrypoint:' and `;;; ... ends here',
or nil if FILE has no `;;; Entrypoint:' marker."
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward "^;;; Entrypoint:" nil t)
      (forward-line 1)
      (let ((start (point))
            (end (and (re-search-forward "^;;; .* ends here" nil t)
                      (line-beginning-position))))
        (unless end
          (signal 'clime-usage-error
                  (list (format "%s: missing ;;; ... ends here after ;;; Entrypoint:"
                                (file-name-nondirectory file)))))
        (buffer-substring start end)))))

(defun clime-make--detect-app (file-path)
  "Detect the clime-app symbol in FILE-PATH.
Reads the file and finds the first `(clime-app SYMBOL ...)' form.
Returns SYMBOL as a symbol, or nil if not found."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (when (re-search-forward "^(clime-app[[:space:]]+\\([^ \t\n)]+\\)" nil t)
      (intern (match-string 1)))))

(defun clime-make--detect-feature (file-path)
  "Detect the provide feature symbol in FILE-PATH.
Reads the file and finds the first `(provide \\='FEATURE)' form.
Returns FEATURE as a symbol, or nil if not found."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (when (re-search-forward "^(provide '\\([^ \t\n)]+\\))" nil t)
      (intern (match-string 1)))))

;;; ─── Handlers ──────────────────────────────────────────────────────────

(defun clime-make--init-handler (ctx)
  "Handle the `init' command: add a polyglot shebang to an Elisp file.
When --client is set, generate an emacsclient wrapper instead.
CTX is the clime context."
  (clime-let ctx (file output (extras extra-load-path)
                       (rels rel-load-path) self-dir standalone force env
                       client)
    (let* ((clime-dir (file-name-directory (locate-library "clime")))
           (source file)
           (target (or output source)))
      (let ((result
             (if client
                 ;; ── emacsclient wrapper ──
                 (clime-make--init-client source target clime-dir
                                          extras rels self-dir standalone force)
               ;; ── batch shebang (original path) ──
               (let* ((resolve (or self-dir rels))
                      (self-dir-flag (if self-dir " -L \"$D\"" ""))
                      (rel-flags
                       (if rels
                           (mapconcat (lambda (p) (format " -L \"$D/%s\"" p))
                                      rels "")
                         ""))
                      (extra-flags
                       (if extras
                           (mapconcat (lambda (p) (format " -L %S" (expand-file-name p)))
                                      extras "")
                         ""))
                      (clime-flag (if standalone "" (format " -L %S" clime-dir)))
                      (load-paths (concat self-dir-flag rel-flags clime-flag extra-flags)))
                 (when output
                   (let ((dir (file-name-directory target)))
                     (when (and dir (not (file-directory-p dir)))
                       (make-directory dir t)))
                   (copy-file source target t))
                 (let ((action (clime-make--write-shebang target env load-paths force resolve)))
                   (format "%s: %s is now executable" action target))))))
        (when (and output (not (string= source target)))
          (setq result (format "%s\n  output: %s" result target)))
        result))))

(defun clime-make--init-client (source target clime-dir
                                       extras rels self-dir standalone force)
  "Generate an emacsclient wrapper for SOURCE at TARGET.
CLIME-DIR, EXTRAS, RELS, SELF-DIR, STANDALONE, FORCE mirror the
init handler's resolved values."
  (let* ((app-sym (clime-make--detect-app source))
         (source-dir (file-name-directory source))
         (load-paths
          (append (when self-dir (list source-dir))
                  (mapcar (lambda (p) (expand-file-name p source-dir)) (or rels '()))
                  (unless standalone (list clime-dir))
                  (mapcar #'expand-file-name (or extras '())))))
    (unless app-sym
      (signal 'clime-usage-error
              (list (format "%s: no (clime-app SYMBOL ...) form found"
                            (file-name-nondirectory source)))))
    (let ((action (clime-make--write-client-wrapper target app-sym load-paths source force)))
      (format "%s: %s is now an emacsclient wrapper" action target))))

(defun clime-make--bundle-handler (ctx)
  "Handle the `bundle' command: concatenate source files into one.
CTX is the clime context."
  (clime-let ctx (files output provide main description)
    (let* ((out output)
           (feature (or provide
                        (file-name-sans-extension
                         (file-name-nondirectory out))))
           (desc (or description "Bundled Elisp distribution"))
           (out-name (file-name-nondirectory out)))
      ;; Validate inputs
      (when main
        (when (clime-make--extract-entry main)
          (signal 'clime-usage-error
                  (list (format "%s: --main file must not contain ;;; Entrypoint: marker"
                                (file-name-nondirectory main))))))
      ;; Build the bundle
      (with-temp-buffer
        ;; Header
        (insert (format ";;; %s --- %s  -*- lexical-binding: t; -*-\n"
                        out-name desc))
        (insert "\n;;; Code:\n\n")
        ;; Extract and concatenate source files
        (dolist (f files)
          (let ((code (clime-make--extract-code f)))
            (when (string-match-p "^(clime-run-batch " code)
              (signal 'clime-usage-error
                      (list (format "%s: (clime-run-batch ...) in library section; move it below ;;; Entrypoint:"
                                    (file-name-nondirectory f)))))
            (insert (format ";; --- %s ---\n" (file-name-nondirectory f)))
            (insert code)
            (insert "\n")))
        ;; Provide
        (insert (format "(provide '%s)\n" feature))
        ;; Guarded entry point from --main file
        (when main
          (let ((entry-code (clime-make--extract-code main)))
            (insert ";;; Entrypoint:\n")
            (insert (format "(when (clime-main-script-p '%s)\n" feature))
            (insert entry-code)
            (insert ")\n")))
        ;; Footer
        (insert (format ";;; %s ends here\n" out-name))
        ;; Write output
        (let ((dir (file-name-directory out)))
          (when (and dir (not (file-directory-p dir)))
            (make-directory dir t)))
        (write-region nil nil out))
      (format "Wrote %s" out))))

(defun clime-make--scaffold-handler (ctx)
  "Handle the `scaffold' command: insert entrypoint boilerplate.
CTX is the clime context."
  (clime-let ctx (file)
    (let* ((target file))
      (let* ((app-sym (clime-make--detect-app target))
             (feature (or (clime-make--detect-feature target) app-sym)))
        (unless app-sym
          (signal 'clime-usage-error
                  (list (format "no clime-app form found in %s"
                                (file-name-nondirectory file)))))
        ;; Check for existing entrypoint
        (with-temp-buffer
          (insert-file-contents target)
          (goto-char (point-min))
          (if (re-search-forward "^;;; Entrypoint:" nil t)
              (format "skipped: %s already has ;;; Entrypoint: section" target)
            ;; Check for unguarded clime-run-batch in the file
            (goto-char (point-min))
            (let ((has-bare-run-batch
                   (re-search-forward "^(clime-run-batch " nil t)))
              ;; Build entrypoint text
              (let ((entry (format ";;; Entrypoint:\n(when (clime-main-script-p '%s)\n  (clime-run-batch %s))\n"
                                   feature app-sym)))
                (goto-char (point-max))
                (if (re-search-backward "^;;; .* ends here" nil t)
                    (progn
                      (goto-char (line-beginning-position))
                      (insert entry))
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert entry))
                (write-region nil nil target))
              (let ((msg (format "done: added entrypoint to %s" target)))
                (if has-bare-run-batch
                    (format "%s\nwarning: %s has (clime-run-batch ...) outside ;;; Entrypoint: section — it will run unconditionally alongside the guarded entrypoint"
                            msg (file-name-nondirectory file))
                  msg)))))))))

(defun clime-make--quickstart-handler (ctx)
  "Handle the `quickstart' command: scaffold + init with auto CLIME_MAIN_APP.
CTX is the clime context."
  (clime-let ctx (file output env)
    (let* ((source file)
           (target (or output source))
           (app-sym (clime-make--detect-app source)))
      ;; When -o is set, copy source to output first, then redirect
      ;; scaffold and init to operate on the copy
      (when output
        (let ((dir (file-name-directory target)))
          (when (and dir (not (file-directory-p dir)))
            (make-directory dir t)))
        (copy-file source target t)
        (setf (clime-context-params ctx)
              (plist-put (clime-context-params ctx) 'file target))
        ;; Clear output so init handler doesn't copy again
        (setf (clime-context-params ctx)
              (plist-put (clime-context-params ctx) 'output nil)))
      ;; Scaffold first
      (let ((scaffold-result (clime-make--scaffold-handler ctx)))
        ;; Auto-inject CLIME_MAIN_APP unless user already passed it
        (when (and app-sym
                   (not (cl-some (lambda (e) (string-prefix-p "CLIME_MAIN_APP=" e))
                                 (or env '()))))
          (let ((new-env (cons (format "CLIME_MAIN_APP=%s" app-sym)
                               (or env '()))))
            (setf (clime-context-params ctx)
                  (plist-put (clime-context-params ctx) 'env new-env))))
        ;; Delegate to init
        (let ((init-result (clime-make--init-handler ctx)))
          (format "%s\n%s" scaffold-result init-result))))))

(defun clime-make--strip-handler (ctx)
  "Handle the `strip' command: remove a clime shebang from an Elisp file.
CTX is the clime context."
  (clime-let ctx (file)
    (let ((target file))
      (unless (clime-make--clime-shebang-p target)
        (signal 'clime-usage-error
                (list (format "%s has no clime shebang to strip"
                              (file-name-nondirectory file)))))
      (with-temp-buffer
        (insert-file-contents target)
        (goto-char (point-min))
        (forward-line 2)
        (delete-region (point-min) (point))
        (write-region nil nil target))
      (set-file-modes target (logand (file-modes target) #o666))
      (format "done: stripped shebang from %s" target))))

;;; ─── Option Templates ──────────────────────────────────────────────────

(clime-defopt make-self-dir
  :bool :help "Add script's own directory to load path (uses $(dirname \"$0\") at runtime)")

(clime-defopt make-standalone
  :bool :help "Skip the automatic clime load path (for vendored/bundled setups)")

(clime-defopt make-force
  :bool :help "Replace an existing non-clime shebang")

(clime-defopt make-env
  :type '(string :match "\\`[A-Za-z_][A-Za-z0-9_]*=[-./:@A-Za-z0-9_]*\\'")
  :multiple :help "Set environment variable in shebang (NAME=VALUE)")

(clime-defopt make-load-path
  :env "CLIME_LOAD_PATH"
  :multiple :help "Additional load paths to include in the shebang")

(clime-defopt make-rel-load-path
  :env "CLIME_REL_LOAD_PATH"
  :multiple :help "Load path relative to script dir")

(clime-defopt make-output
  :type 'file
  :help "Write to OUTPUT instead of modifying the source file")

;;; ─── App Definition ─────────────────────────────────────────────────────

(clime-app clime-make
  :version clime-version
  :help "clime — declarative CLI framework for Emacs Lisp."

  ;; ── init ─────────────────────────────────────────────────────────────
  (clime-command init
    :help "Add a polyglot shebang header to an Emacs Lisp file"

    (clime-arg file
      :type '(file :must-exist t)
      :help "The .el file to initialize")

    (clime-opt self-dir ("--self-dir") :from make-self-dir)

    (clime-opt extra-load-path ("--load-path" "-L") :from make-load-path :type 'path)
    (clime-opt rel-load-path ("--rel-load-path" "-R") :from make-rel-load-path)
    (clime-opt standalone ("--standalone") :from make-standalone)
    (clime-opt force ("--force" "-f") :from make-force)
    (clime-opt env ("--env" "-e") :from make-env)
    (clime-opt output ("--output" "-o") :from make-output)

    (clime-opt client ("--client")
      :bool :requires '(output)
      :help "Generate an emacsclient wrapper instead of a shebang")

    (clime-handler (ctx) (clime-make--init-handler ctx)))

  ;; ── bundle ──────────────────────────────────────────────────────────
  (clime-command bundle
    :help "Concatenate multiple Elisp source files into a single file"

    (clime-arg files :nargs :rest :type '(file :must-exist t)
               :help "Source files in dependency order")

    (clime-opt output ("--output" "-o")
      :type 'file
      :required :help "Output file path")

    (clime-opt provide ("--provide" "-p")
      :help "Feature name for (provide 'FEATURE) (default: output filename)")

    (clime-opt main ("--main" "-m")
      :type '(file :must-exist t)
      :help "Entrypoint file whose code is appended with a clime-main-script-p guard")

    (clime-opt description ("--description" "-d")
      :help "One-line description for the file header")

    (clime-handler (ctx) (clime-make--bundle-handler ctx)))

  ;; ── scaffold ───────────────────────────────────────────────────────
  (clime-command scaffold
    :help "Insert ;;; Entrypoint: boilerplate into an Elisp file"

    (clime-arg file :type '(file :must-exist t)
               :help "The .el file to scaffold")

    (clime-handler (ctx) (clime-make--scaffold-handler ctx)))

  ;; ── quickstart ──────────────────────────────────────────────────────
  (clime-command quickstart
    :help "Scaffold entrypoint + add shebang (scaffold + init)"

    (clime-arg file :type '(file :must-exist t)
               :help "The .el file to set up")

    (clime-opt self-dir ("--self-dir") :from make-self-dir)

    (clime-opt extra-load-path ("--load-path" "-L") :from make-load-path :type 'dir)
    (clime-opt rel-load-path ("--rel-load-path" "-R") :from make-rel-load-path)
    (clime-opt standalone ("--standalone") :from make-standalone)
    (clime-opt force ("--force" "-f") :from make-force)
    (clime-opt env ("--env" "-e") :from make-env)
    (clime-opt output ("--output" "-o") :from make-output)

    (clime-handler (ctx) (clime-make--quickstart-handler ctx)))

  ;; ── strip ────────────────────────────────────────────────────────
  (clime-command strip
    :help "Remove the clime shebang header from an Elisp file"

    (clime-arg file :type '(file :must-exist t)
               :help "The .el file to strip")

    (clime-handler (ctx) (clime-make--strip-handler ctx))))

(provide 'clime-make)
;;; clime-make.el ends here
