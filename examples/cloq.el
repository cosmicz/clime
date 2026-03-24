#!/bin/sh
":"; S="$(realpath "$0")";D="$(dirname "$S")"; CLIME_ARGV0="$0" CLIME_MAIN_APP=cloq exec emacs --batch -Q -L "$D/.." -L "/Users/cosmic/dev/clime/" -L "/Users/cosmic/.emacs.d/.local/straight/repos/org-ql" -L "/Users/cosmic/.emacs.d/.local/straight/repos/dash.el" -L "/Users/cosmic/.emacs.d/.local/straight/repos/ts.el" -L "/Users/cosmic/.emacs.d/.local/straight/repos/s.el" --eval "(setq load-file-name \"$S\")" --eval "(with-temp-buffer(insert-file-contents load-file-name)(setq lexical-binding t)(goto-char(point-min))(condition-case nil(while t(eval(read(current-buffer))t))(end-of-file nil)))" -- "$@" # clime-sh!:v1 -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; cloq.el --- Query org files from the command line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; cloq = clime + org-ql.  A CLI and interactive UI for querying org
;; files, built as a demo of clime v0.4 features:
;;
;;   - clime-mutex (exclusive query modes)
;;   - :conform (sexp validation)
;;   - :negatable flags (--tags / --no-tags)
;;   - clime-alias-for with :vals (shortcut commands)
;;   - clime-output-format (--json)
;;   - :examples on commands
;;   - env var integration (CLOQ_FILE)
;;
;; Requires: org-ql, dash, ts, s
;;
;; CLI:
;;   ./examples/cloq.el query -f tasks.org --todo TODO
;;   CLOQ_FILE=a.org,b.org ./examples/cloq.el query --todo WAITING
;;
;; Interactive:
;;   (require 'cloq)
;;   (require 'clime-invoke)
;;   (clime-invoke cloq)

;;; Code:

(require 'clime)
(require 'org-ql)

(clime-app cloq
  :version "1.0"
  :help "Query org files from the command line."
  :env-prefix "CLOQ"
  :examples '(("cloq query -f proj.org --todo TODO" . "Find all TODOs")
              ("cloq query -f proj.org --sexp '(deadline :to today)'" . "Overdue items")
              ("cloq waiting -f proj.org -f side.org" . "WAITING items across files")
              ("cloq tags -f proj.org" . "List all tags"))

  ;; ── Root Options ──────────────────────────────────────────────────

  (clime-output-format json ("--json") :help "Output as JSON")

  (clime-opt verbose ("-v" "--verbose")
    :count :help "Increase verbosity")

  (clime-opt files ("--file" "-f")
    :multiple :required
    :coerce #'expand-file-name
    :env "FILES"
    :help "Org file(s) to query")

  ;; ── query ─────────────────────────────────────────────────────────

  (clime-command query
    :help "Run an org-ql query against one or more org files."
    :aliases (q)
    :examples '(("cloq query -f proj.org --todo TODO" . "Find TODOs")
                ("cloq query -f proj.org --sexp '(and (todo) (tags \"urgent\"))'" . "Urgent TODOs")
                ("cloq query -f proj.org --search 'Emacs' --no-tags" . "Search without tags"))

    (clime-mutex query-mode
      (clime-opt todo ("--todo" "-t")
        :help "Match TODO keyword (e.g. TODO, WAITING, DONE)"
        :category "Query")
      (clime-opt sexp ("--sexp")
        :help "S-expression org-ql query"
        :category "Query"
        :conform (lambda (val)
                   (condition-case err
                       (progn (car (read-from-string val)) val)
                     (error (error "Invalid sexp: %s" (error-message-string err))))))
      (clime-opt search ("--search" "-s")
        :help "Plain-text search"
        :category "Query"))

    (clime-opt sort ("--sort")
      :choices '("deadline" "priority" "todo")
      :help "Sort results"
      :category "Output")

    (clime-opt limit ("--limit" "-n")
      :type 'integer
      :help "Max results to return"
      :category "Output")

    (clime-opt tags ("--tags")
      :negatable :default t
      :help "Include tags in output"
      :category "Output")

    (clime-handler (ctx)
      (clime-let ctx (files todo sexp search sort limit verbose tags)
        (let* ((q (cond (sexp (car (read-from-string sexp)))
                        (search (org-ql--query-string-to-sexp search))
                        (todo `(todo ,todo))
                        (t '(todo))))
               (results (org-ql-select files q
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

  ;; ── tags ──────────────────────────────────────────────────────────

  (clime-command tags
    :help "List all tags found across the input files."
    :examples '(("cloq tags -f proj.org" . "Tags in one file")
                ("cloq tags -f proj.org -f side.org --json" . "Tags as JSON"))

    (clime-handler (ctx)
      (clime-let ctx (files)
        (let ((all-tags '()))
          (dolist (f (if (listp files) files (list files)))
            (with-current-buffer (find-file-noselect f)
              (org-with-wide-buffer
               (goto-char (point-min))
               (while (re-search-forward org-tag-line-re nil t)
                 (let ((tags-str (match-string-no-properties 1)))
                   (when tags-str
                     (dolist (tag (split-string tags-str ":" t))
                       (cl-pushnew tag all-tags :test #'string=))))))))
          (setq all-tags (sort all-tags #'string<))
          (if (eq (clime-output-name) 'json)
              (mapcar (lambda (tag) `((tag . ,tag))) all-tags)
            (mapconcat #'identity all-tags "\n"))))))

  ;; ── Shortcuts ─────────────────────────────────────────────────────

  (clime-group shortcuts :inline :category "Shortcuts"
               (clime-alias-for waiting (query)
                 :help "Show WAITING items"
                 :vals '((todo . "WAITING")))

               (clime-alias-for overdue (query)
                 :help "Show items past their deadline"
                 :vals '((sexp . "(deadline :to today)")))

               (clime-alias-for today (query)
                 :help "Show items scheduled for today"
                 :vals '((sexp . "(scheduled :on today)")))))

(provide 'cloq)
;;; Entrypoint:
(when (clime-main-script-p 'cloq)
  (clime-run-batch cloq))
;;; cloq.el ends here
