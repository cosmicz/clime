#!/bin/sh
":"; exec emacs --batch -Q -L "$(dirname "$0")" -l "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; clime-app.el --- CLI tool for the clime framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Octavian <cosmicz@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Command-line interface for clime itself.  Currently provides the
;; `init' command which adds a polyglot shebang header to an Emacs
;; Lisp file, making it directly executable.
;;
;; Usage:
;;   ./clime-app.el init myapp.el
;;   ./clime-app.el init myapp.el -L ./lib
;;   ./clime-app.el --help

;;; Code:

(require 'clime)

(defvar clime-app--self-path
  (and load-file-name (file-truename load-file-name))
  "Absolute path to this file, resolved through symlinks.")

(clime-app clime
  :version "0.1.0"
  :help "clime — declarative CLI framework for Emacs Lisp."

  (clime-command init
    :help "Add a polyglot shebang header to an Emacs Lisp file"

    (clime-arg file :help "The .el file to initialize")

    (clime-option extra-load-path ("--load-path" "-L") :multiple t
      :help "Additional load paths to include in the shebang")

    (clime-option standalone ("--standalone") :flag t
      :help "Use self-relative load path (for single-file distributions)")

    (clime-option env ("--env" "-e") :multiple t
      :help "Set environment variable in shebang (NAME=VALUE)")

    (clime-handler (ctx)
      (clime-let ctx (file (extras extra-load-path) standalone env)
        (let* ((clime-dir (file-name-directory clime-app--self-path))
               (target (expand-file-name file))
               (load-paths
                (if standalone
                    " -L \"$(dirname \"$0\")\""
                  (mapconcat (lambda (p)
                               (format " -L %S" (expand-file-name p)))
                             (cons clime-dir (or extras '()))
                             "")))
               (env-prefix
                (if env
                    (concat (mapconcat #'identity env " ") " ")
                  ""))
               (shebang
                (format "#!/bin/sh\n\":\"; %sexec emacs --batch -Q%s -l \"$0\" -- \"$@\" # -*- mode: emacs-lisp; lexical-binding: t; -*-\n"
                        env-prefix load-paths)))
          (unless (file-exists-p target)
            (signal 'clime-usage-error
                    (list (format "%s does not exist" file))))
          (with-temp-buffer
            (insert-file-contents target)
            (goto-char (point-min))
            (when (looking-at "#!")
              (signal 'clime-usage-error
                      (list (format "%s already has a shebang line" file))))
            (insert shebang)
            (write-region nil nil target))
          (set-file-modes target #o755)
          (format "done: %s is now executable" target))))))

(clime-run-batch clime)

(provide 'clime-app)
;;; clime-app.el ends here
