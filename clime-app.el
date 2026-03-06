#!/bin/sh
":"; exec emacs --batch -Q -L "$(dirname "$0")" -l "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; clime-app.el --- CLI tool for the clime framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Octavian <cosmicz@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Command-line interface for clime itself.  Provides `init' (add
;; shebang to an Elisp file) and `bundle' (concatenate multiple
;; source files into a single distributable).
;;
;; Usage:
;;   ./clime-app.el init myapp.el
;;   ./clime-app.el init myapp.el -L ./lib
;;   ./clime-app.el bundle -o bundle.el --provide mylib src/*.el
;;   ./clime-app.el --help

;;; Code:

(require 'clime)

(defvar clime-app--self-path
  (and load-file-name (file-truename load-file-name))
  "Absolute path to this file, resolved through symlinks.")

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defconst clime-app--env-var-re
  "\\`[A-Za-z_][A-Za-z0-9_]*=[-./:@A-Za-z0-9_]*\\'"
  "Regexp matching safe NAME=VALUE env var assignments for shebang.")

(defconst clime-app--symbol-re
  "\\`[[:alpha:]_][[:alnum:]_:+*/<>=!?-]*\\'"
  "Regexp matching safe Elisp symbol names.")

(defun clime-app--validate-env-vars (env-vars)
  "Validate each entry in ENV-VARS matches safe NAME=VALUE format.
Signals `clime-usage-error' on the first invalid entry."
  (dolist (e env-vars)
    (unless (string-match-p clime-app--env-var-re e)
      (signal 'clime-usage-error
              (list (format "invalid --env value %S (expected NAME=VALUE with safe characters)" e))))))

(defun clime-app--validate-symbol (name flag)
  "Validate NAME is a safe Elisp symbol name.
FLAG is the option name for error messages.
Signals `clime-usage-error' if invalid."
  (unless (string-match-p clime-app--symbol-re name)
    (signal 'clime-usage-error
            (list (format "invalid %s value %S (must be a valid symbol name)" flag name)))))

(defun clime-app--make-shebang (env-vars load-paths)
  "Build a two-line polyglot shebang string.
ENV-VARS is a list of \"NAME=VALUE\" strings.
LOAD-PATHS is a string of formatted -L flags (may be empty)."
  (let ((env-prefix (if env-vars
                        (concat (mapconcat #'identity env-vars " ") " ")
                      "")))
    (format "#!/bin/sh\n\":\"; %sexec emacs --batch -Q%s -l \"$0\" -- \"$@\" # -*- mode: emacs-lisp; lexical-binding: t; -*-\n"
            env-prefix load-paths)))

(defun clime-app--insert-shebang (target env-vars load-paths)
  "Prepend a shebang to TARGET file.
ENV-VARS and LOAD-PATHS are passed to `clime-app--make-shebang'."
  (let ((shebang (clime-app--make-shebang env-vars load-paths)))
    (with-temp-buffer
      (insert-file-contents target)
      (goto-char (point-min))
      (when (looking-at "#!")
        (signal 'clime-usage-error
                (list (format "%s already has a shebang line"
                              (file-name-nondirectory target)))))
      (insert shebang)
      (write-region nil nil target))
    (set-file-modes target #o755)))

(defun clime-app--extract-code (file)
  "Extract code from FILE using GNU-style library section markers.
FILE must contain a top-level `;;; Code:' section and a terminating
`;;; ... ends here' line.  Strips top-level `(clime-run-batch ...)'
forms but keeps `(provide ...)' forms so `require' chains between
bundled modules continue to work."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((start (and (re-search-forward "^;;; Code:" nil t)
                      (forward-line 1)
                      (point)))
          (end (and (re-search-forward "^;;; .* ends here" nil t)
                    (line-beginning-position))))
      (unless (and start end)
        (signal 'clime-usage-error
                (list (format "%s: missing ;;; Code: or ;;; ... ends here markers"
                              (file-name-nondirectory file)))))
      (let ((code (buffer-substring start end)))
        ;; Strip clime-run-batch lines.  Keep (provide ...) so that
        ;; (require ...) calls between modules are satisfied.
        (with-temp-buffer
          (insert code)
          (goto-char (point-min))
          (while (re-search-forward "^(clime-run-batch .*\n?" nil t)
            (replace-match ""))
          (buffer-string))))))

;;; ─── App Definition ─────────────────────────────────────────────────────

(clime-app clime
           :version "0.1.0"
           :help "clime — declarative CLI framework for Emacs Lisp."

           ;; ── init ─────────────────────────────────────────────────────────────
           (clime-command
            init
            :help "Add a polyglot shebang header to an Emacs Lisp file"

            (clime-arg file :help "The .el file to initialize")

            (clime-option extra-load-path ("--load-path" "-L") :multiple t
                          :help "Additional load paths to include in the shebang")

            (clime-option standalone ("--standalone") :flag t
                          :help "Skip the automatic clime load path (for vendored/bundled setups)")

            (clime-option env ("--env" "-e") :multiple t
                          :help "Set environment variable in shebang (NAME=VALUE)")

            (clime-handler (ctx)
                           (clime-let ctx (file (extras extra-load-path) standalone env)
                                      (let* ((clime-dir (file-name-directory clime-app--self-path))
                                             (target (expand-file-name file))
                                             (load-paths
                                              (if standalone
                                                  (if extras
                                                      (mapconcat (lambda (p)
                                                                   (format " -L %S" (expand-file-name p)))
                                                                 extras "")
                                                    "")
                                                (mapconcat (lambda (p)
                                                             (format " -L %S" (expand-file-name p)))
                                                           (cons clime-dir (or extras '()))
                                                           ""))))
                                        (unless (file-exists-p target)
                                          (signal 'clime-usage-error
                                                  (list (format "%s does not exist" file))))
                                        (when env
                                          (clime-app--validate-env-vars env))
                                        (clime-app--insert-shebang target env load-paths)
                                        (format "done: %s is now executable" target)))))

           ;; ── bundle ──────────────────────────────────────────────────────────
           (clime-command bundle
                          :help "Concatenate multiple Elisp source files into a single file"

                          (clime-arg files :nargs :rest :help "Source files in dependency order")

                          (clime-option output ("--output" "-o") :required t
                                        :help "Output file path")

                          (clime-option provide ("--provide" "-p")
                                        :help "Feature name for (provide 'FEATURE) (default: output filename)")

                          (clime-option main ("--main" "-m")
                                        :help "Add guarded entry point (use init --env CLIME_MAIN_APP=APP to activate)")

                          (clime-option description ("--description" "-d")
                                        :help "One-line description for the file header")

                          (clime-handler (ctx)
                                         (clime-let ctx (files output provide main description)
                                                    (let* ((out (expand-file-name output))
                                                           (feature (or provide
                                                                        (file-name-sans-extension
                                                                         (file-name-nondirectory out))))
                                                           (desc (or description "Bundled Elisp distribution"))
                                                           (out-name (file-name-nondirectory out)))
                                                      ;; Validate inputs
                                                      (when main
                                                        (clime-app--validate-symbol main "--main"))
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
                                                          (insert (format ";; --- %s ---\n"
                                                                          (file-name-nondirectory f)))
                                                          (insert (clime-app--extract-code (expand-file-name f)))
                                                          (insert "\n"))
                                                        ;; Provide
                                                        (insert (format "(provide '%s)\n" feature))
                                                        ;; Guarded entry point
                                                        (when main
                                                          (insert (format "(when (clime-main-script-p '%s)\n" main))
                                                          (insert (format "  (clime-run-batch %s))\n\n" main)))
                                                        ;; Footer
                                                        (insert (format ";;; %s ends here\n" out-name))
                                                        ;; Write output
                                                        (let ((dir (file-name-directory out)))
                                                          (when (and dir (not (file-directory-p dir)))
                                                            (make-directory dir t)))
                                                        (write-region nil nil out))
                                                      (format "Wrote %s" out))))))

(clime-run-batch clime)

(provide 'clime-app)
;;; clime-app.el ends here
