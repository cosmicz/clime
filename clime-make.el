#!/bin/sh
":"; CLIME_ARGV0="$0" exec emacs --batch -Q -L "$(dirname "$0")" --eval "(setq load-file-name \"$0\")" --eval "(with-temp-buffer(insert-file-contents load-file-name)(setq lexical-binding t)(goto-char(point-min))(condition-case nil(while t(eval(read(current-buffer))t))(end-of-file nil)))" -- "$@" # clime:0.1.1 -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; clime-make.el --- CLI tool for the clime framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Command-line interface for clime itself.  Provides `init' (add
;; shebang to an Elisp file) and `bundle' (concatenate multiple
;; source files into a single distributable).
;;
;; Usage:
;;   ./clime-make.el init myapp.el
;;   ./clime-make.el init myapp.el -L ./lib
;;   ./clime-make.el bundle -o bundle.el --provide mylib src/*.el
;;   ./clime-make.el --help

;;; Code:

(require 'clime)

(defvar clime-make--self-path
  (and load-file-name (file-truename load-file-name))
  "Absolute path to this file, resolved through symlinks.")

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


(defconst clime-make--shebang-tag-re
  "# clime:[0-9]+\\.[0-9]+\\.[0-9]+"
  "Regexp matching the clime version tag in a shebang line 2.")

(defun clime-make--clime-shebang-p (file)
  "Return non-nil if FILE starts with a clime-tagged shebang."
  (with-temp-buffer
    (insert-file-contents file nil 0 512)
    (goto-char (point-min))
    (and (looking-at "#!")
         (forward-line 1)
         (let ((line2 (buffer-substring (point) (line-end-position))))
           (string-match-p clime-make--shebang-tag-re line2)))))

(defun clime-make--make-shebang (env-vars load-paths)
  "Build a two-line polyglot shebang string.
ENV-VARS is a list of \"NAME=VALUE\" strings.
LOAD-PATHS is a string of formatted -L flags (may be empty).
Always includes CLIME_ARGV0=\"$0\" so usage output shows the
executable name rather than the DSL symbol.
Embeds a clime:VERSION tag for detection and update support.

Uses --eval with a read/eval loop instead of -l to force
`lexical-binding' to t.  The standard polyglot (\":\" on line 2)
prevents Emacs from recognizing the -*-cookie, so we read forms
from a temp buffer with lexical-binding set and eval each with
the lexical flag."
  (let* ((all-vars (cons "CLIME_ARGV0=\"$0\"" (or env-vars '())))
         (env-prefix (concat (mapconcat #'identity all-vars " ") " "))
         (eval-form (concat
                     "--eval \"(setq load-file-name \\\"$0\\\")\""
                     " --eval \"(with-temp-buffer"
                     "(insert-file-contents load-file-name)"
                     "(setq lexical-binding t)"
                     "(goto-char(point-min))"
                     "(condition-case nil"
                     "(while t(eval(read(current-buffer))t))"
                     "(end-of-file nil)))\"")))
    (format "#!/bin/sh\n\":\"; %sexec emacs --batch -Q%s %s -- \"$@\" # clime:%s -*- mode: emacs-lisp; lexical-binding: t; -*-\n"
            env-prefix load-paths eval-form clime-version)))

(defun clime-make--write-shebang (target env-vars load-paths force)
  "Write a shebang to TARGET file, handling existing headers.
ENV-VARS and LOAD-PATHS are passed to `clime-make--make-shebang'.
If TARGET has a clime-tagged shebang, replace it (return \"updated\").
If TARGET has a non-clime shebang and FORCE is non-nil, replace it.
If TARGET has a non-clime shebang and FORCE is nil, signal an error.
If TARGET has no shebang, prepend one (return \"done\")."
  (let ((shebang (clime-make--make-shebang env-vars load-paths))
        (action nil))
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-min))
      (cond
       ;; Existing shebang
       ((looking-at "#!")
        (if (or (clime-make--clime-shebang-p target) force)
            (progn
              ;; Delete the two shebang lines
              (forward-line 2)
              (delete-region (point-min) (point))
              (goto-char (point-min))
              (insert shebang)
              (setq action "updated"))
          (signal 'clime-usage-error
                  (list (format "%s already has a shebang line (use --force to replace)"
                                (file-name-nondirectory target))))))
       ;; No shebang: prepend
       (t
        (insert shebang)
        (setq action "done")))
      (write-region nil nil target))
    (set-file-modes target #o755)
    action))

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

;;; ─── Handlers ──────────────────────────────────────────────────────────

(defun clime-make--init-handler (ctx)
  "Handle the `init' command: add a polyglot shebang to an Elisp file.
CTX is the clime context."
  (clime-let ctx (file (extras extra-load-path)
                       (rels rel-load-path) self-dir standalone force env)
    (let* ((clime-dir (file-name-directory clime-make--self-path))
           (target (expand-file-name file))
           (self-dir-flag (if self-dir
                              " -L \"$(dirname \"$0\")\""
                            ""))
           (rel-flags
            (if rels
                (mapconcat (lambda (p)
                             (format " -L \"$(dirname \"$0\")/%s\"" p))
                           rels "")
              ""))
           (extra-flags
            (if extras
                (mapconcat (lambda (p)
                             (format " -L %S" (expand-file-name p)))
                           extras "")
              ""))
           (clime-flag
            (if standalone ""
              (format " -L %S" clime-dir)))
           (load-paths
            (concat self-dir-flag rel-flags clime-flag extra-flags)))
      (unless (file-exists-p target)
        (signal 'clime-usage-error
                (list (format "%s does not exist" file))))
      (when env
        (clime-make--validate-env-vars env))
      (let ((action (clime-make--write-shebang target env load-paths force)))
        (format "%s: %s is now executable" action target)))))

(defun clime-make--bundle-handler (ctx)
  "Handle the `bundle' command: concatenate source files into one.
CTX is the clime context."
  (clime-let ctx (files output provide main description)
    (let* ((out (expand-file-name output))
           (feature (or provide
                        (file-name-sans-extension
                         (file-name-nondirectory out))))
           (desc (or description "Bundled Elisp distribution"))
           (out-name (file-name-nondirectory out)))
      ;; Validate inputs
      (when main
        (let ((main-path (expand-file-name main)))
          (unless (file-exists-p main-path)
            (signal 'clime-usage-error
                    (list (format "%s does not exist" main))))
          (when (clime-make--extract-entry main-path)
            (signal 'clime-usage-error
                    (list (format "%s: --main file must not contain ;;; Entrypoint: marker"
                                  (file-name-nondirectory main)))))))
      (dolist (f files)
        (unless (file-exists-p f)
          (signal 'clime-usage-error
                  (list (format "%s does not exist" f)))))
      ;; Build the bundle
      (with-temp-buffer
        ;; Header
        (insert (format ";;; %s --- %s  -*- lexical-binding: t; -*-\n"
                        out-name desc))
        (insert "\n;;; Code:\n\n")
        ;; Extract and concatenate source files
        (dolist (f files)
          (let ((code (clime-make--extract-code (expand-file-name f))))
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
          (let ((entry-code (clime-make--extract-code (expand-file-name main))))
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

;;; ─── App Definition ─────────────────────────────────────────────────────

(clime-app clime-make
           :version clime-version
           :help "clime — declarative CLI framework for Emacs Lisp."

           ;; ── init ─────────────────────────────────────────────────────────────
           (clime-command
            init
            :help "Add a polyglot shebang header to an Emacs Lisp file"

            (clime-arg file :help "The .el file to initialize")

            (clime-option extra-load-path ("--load-path" "-L") :multiple t
                          :help "Additional load paths to include in the shebang")

            (clime-option self-dir ("--self-dir") :flag t
                          :help "Add script's own directory to load path (uses $(dirname \"$0\") at runtime)")

            (clime-option rel-load-path ("--rel-load-path" "-R") :multiple t
                          :help "Load path relative to script dir (e.g. -R .. adds $(dirname \"$0\")/..)")

            (clime-option standalone ("--standalone") :flag t
                          :help "Skip the automatic clime load path (for vendored/bundled setups)")

            (clime-option force ("--force" "-f") :flag t
                          :help "Replace an existing non-clime shebang")

            (clime-option env ("--env" "-e") :multiple t
                          :help "Set environment variable in shebang (NAME=VALUE)")

            (clime-handler (ctx) (clime-make--init-handler ctx)))

           ;; ── bundle ──────────────────────────────────────────────────────────
           (clime-command bundle
                          :help "Concatenate multiple Elisp source files into a single file"

                          (clime-arg files :nargs :rest :help "Source files in dependency order")

                          (clime-option output ("--output" "-o") :required t
                                        :help "Output file path")

                          (clime-option provide ("--provide" "-p")
                                        :help "Feature name for (provide 'FEATURE) (default: output filename)")

                          (clime-option main ("--main" "-m")
                                        :help "Entrypoint file whose code is appended with a clime-main-script-p guard")

                          (clime-option description ("--description" "-d")
                                        :help "One-line description for the file header")

                          (clime-handler (ctx) (clime-make--bundle-handler ctx))))

(provide 'clime-make)
;;; Entrypoint:
(clime-run-batch clime-make)
;;; clime-make.el ends here
