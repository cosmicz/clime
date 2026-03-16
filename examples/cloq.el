#!/bin/sh
":"; CLIME_ARGV0="$0" CLIME_MAIN_APP=cloq exec emacs --batch -Q -L "/Users/cosmic/dev/clime/" -L "/Users/cosmic/.emacs.d/.local/straight/repos/org-ql" -L "/Users/cosmic/.emacs.d/.local/straight/repos/dash.el" -L "/Users/cosmic/.emacs.d/.local/straight/repos/ts.el" -L "/Users/cosmic/.emacs.d/.local/straight/repos/s.el" --eval "(setq load-file-name \"$0\")" --eval "(with-temp-buffer(insert-file-contents load-file-name)(setq lexical-binding t)(goto-char(point-min))(condition-case nil(while t(eval(read(current-buffer))t))(end-of-file nil)))" -- "$@" # clime-sh!:v1 -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; cloq.el --- Query org files from the command line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; A concise demo app for clime v0.3 — wraps org-ql into a CLI tool.
;;
;; Requires: org-ql

;;; Code:

(require 'clime)

(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/org-ql/")
(require 'org-ql)

(clime-app cloq
  :version "1.0"
  :help "Query org files from the command line."
  :env-prefix "CLOQ"
  :epilog "Examples:
  cloq query -f tasks.org --todo TODO
  cloq query -f tasks.org --sexp '(deadline :to today)' --json
  cloq waiting -f tasks.org"

  (clime-output-format json ("--json") :help "Output as JSON")

  (clime-option verbose ("-v" "--verbose") :count
                :help "Increase verbosity")

  (clime-option file ("--file" "-f") :multiple :required
                :coerce #'expand-file-name
                :help "Org file(s) to query")

  (clime-command query
    :help "Run an org-ql query"
    :aliases (q)

    (clime-mutex query
      (clime-option todo ("--todo" "-t")
                    :help "Match TODO keyword" :category "Query")
      (clime-option sexp ("--sexp")
                    :help "S-expression query" :category "Query")
      (clime-option search ("--search" "-s")
                    :help "Plain-text search query" :category "Query"))

    (clime-option sort ("--sort")
      :choices '("deadline" "priority" "todo")
      :help "Sort results" :category "Output")
    (clime-option limit ("--limit" "-n") :type 'integer
                  :help "Max results to return" :category "Output")
    (clime-option tags ("--tags") :negatable :default t
                  :help "Include tags in output" :category "Output")

    (clime-handler (ctx)
      (clime-let ctx (file todo sexp search sort limit verbose tags)
        (let* ((q (cond (sexp (car (read-from-string sexp)))
                        (search (org-ql--query-string-to-sexp search))
                        (todo `(todo ,todo))
                        (t '(todo))))
               (results (org-ql-select file q
                          :action (if tags
                                      '(org-get-heading t nil t t)
                                    '(org-get-heading t t t t))
                          :sort (when sort (intern sort)))))
          (when limit (setq results (seq-take results limit)))
          (when (and verbose (> verbose 0))
            (message "cloq: %d hit%s" (length results)
                     (if (= 1 (length results)) "" "s")))
          (if (eq (clime-output-name) 'json)
              (mapcar (lambda (h) `((heading . ,h))) results)
            (mapconcat #'identity results "\n"))))))

  (clime-group shortcuts :inline :category "Shortcuts"
               (clime-alias-for waiting (query)
               :help "Show WAITING items"
               :vals '((todo . "WAITING")))))

(provide 'cloq)
;;; Entrypoint:
(when (clime-main-script-p 'cloq)
  (clime-run-batch cloq))
;;; cloq.el ends here
