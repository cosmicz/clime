;;; clime-dotenv.el --- Dotenv-style .env file loading for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Loads KEY=VALUE pairs from .env files into a scoped
;; `process-environment' binding so the existing :env / :env-prefix
;; option machinery picks them up.  Configured per-app via the
;; `clime-app' :dotenv slot.
;;
;; Grammar (v1, intentional subset of node-dotenv):
;;   - KEY=value                bare value, trailing ws/comment stripped
;;   - KEY="value"              double-quoted; \n \r \t \\ \" expanded
;;   - KEY='value'              single-quoted; literal contents
;;   - export KEY=value         leading `export ' tolerated and dropped
;;   - # comment                full-line or trailing-on-bare-value
;;   - blank lines              ignored
;;   - KEY shape                [A-Za-z_][A-Za-z0-9_]*
;;
;; Out of scope for v1: ${VAR} interpolation, multi-line unquoted
;; values, command substitution.
;;
;; Precedence: real process env always wins over .env values.

;;; Code:

(require 'cl-lib)
(require 'clime-core)

(define-error 'clime-usage-error "CLI usage error")

;;; ─── Parser ────────────────────────────────────────────────────────────

(defconst clime-dotenv--key-re "\\`[A-Za-z_][A-Za-z0-9_]*\\'"
  "Regexp a .env KEY must fully match.")

(defun clime-dotenv--err (file line reason)
  "Signal `clime-usage-error' with FILE + LINE + REASON.
FILE may be nil, in which case only line + reason are included."
  (signal 'clime-usage-error
          (list (if file
                    (format "%s: line %d: %s" file line reason)
                  (format "line %d: %s" line reason)))))

(defun clime-dotenv--parse-double-quoted (raw file line)
  "Parse a double-quoted value from RAW (starting with `\"').
Return (VALUE . REST-AFTER-CLOSING-QUOTE).
Signal with FILE and LINE context on unterminated strings."
  (let ((i 1)
        (n (length raw))
        (out (list))
        (closed nil))
    (catch 'done
      (while (< i n)
        (let ((c (aref raw i)))
          (cond
           ((eq c ?\\)
            (when (>= (1+ i) n)
              (clime-dotenv--err file line "dangling backslash in double-quoted value"))
            (let ((nxt (aref raw (1+ i))))
              (push (pcase nxt
                      (?n ?\n) (?r ?\r) (?t ?\t)
                      (?\\ ?\\) (?\" ?\")
                      (_ nxt))
                    out))
            (setq i (+ i 2)))
           ((eq c ?\")
            (cl-incf i)
            (setq closed t)
            (throw 'done nil))
           (t
            (push c out)
            (cl-incf i))))))
    (unless closed
      (clime-dotenv--err file line "unterminated double-quoted value"))
    (cons (apply #'string (nreverse out))
          (substring raw i))))

(defun clime-dotenv--parse-single-quoted (raw file line)
  "Parse a single-quoted value from RAW (starting with `'').
Return (VALUE . REST-AFTER-CLOSING-QUOTE).
Signal with FILE and LINE context on unterminated strings."
  (let* ((closing (string-match "'" raw 1)))
    (unless closing
      (clime-dotenv--err file line "unterminated single-quoted value"))
    (cons (substring raw 1 closing)
          (substring raw (1+ closing)))))

(defun clime-dotenv--parse-bare (raw)
  "Parse a bare value from RAW.
Strip surrounding whitespace and any trailing `# comment' (when
preceded by whitespace).  Return the value string.  Quoted values
bypass this and preserve internal whitespace."
  (let* ((no-comment
          (if (string-match "[ \t]+#" raw)
              (substring raw 0 (match-beginning 0))
            raw)))
    (string-trim no-comment)))

(defun clime-dotenv--parse-line (line file lineno)
  "Parse a single non-blank, non-comment LINE.
Return (KEY . VALUE).  FILE / LINENO drive error messages."
  (let ((s (string-trim-left line)))
    (when (string-prefix-p "export " s)
      (setq s (string-trim-left (substring s 7))))
    (let ((eq-pos (string-match "=" s)))
      (unless eq-pos
        (clime-dotenv--err file lineno "missing '='"))
      (let* ((key (string-trim (substring s 0 eq-pos)))
             (raw (substring s (1+ eq-pos))))
        (unless (string-match-p clime-dotenv--key-re key)
          (clime-dotenv--err file lineno (format "invalid key %S" key)))
        (let ((value
               (cond
                ((and (> (length raw) 0) (eq (aref raw 0) ?\"))
                 (car (clime-dotenv--parse-double-quoted raw file lineno)))
                ((and (> (length raw) 0) (eq (aref raw 0) ?'))
                 (car (clime-dotenv--parse-single-quoted raw file lineno)))
                (t (clime-dotenv--parse-bare raw)))))
          (cons key value))))))

(defun clime-dotenv-parse (string &optional file)
  "Parse STRING as dotenv contents, returning an alist of (KEY . VALUE).
FILE, when non-nil, is included in error messages.  Lines are processed
in order; the returned alist preserves order.  Duplicate keys: later
wins within a single file."
  (let ((lineno 0)
        (out '()))
    (dolist (line (split-string string "\n"))
      (cl-incf lineno)
      (let ((trimmed (string-trim-left line)))
        (unless (or (string-empty-p trimmed)
                    (eq (aref trimmed 0) ?#))
          (let* ((pair (clime-dotenv--parse-line line file lineno))
                 (existing (assoc (car pair) out)))
            (if existing
                (setcdr existing (cdr pair))
              (push pair out))))))
    (nreverse out)))

;;; ─── File loading ──────────────────────────────────────────────────────

(defun clime-dotenv-load (file)
  "Load FILE and return its parsed alist, or nil if FILE does not exist.
Malformed FILE signals `clime-usage-error' with FILE + line in the
message."
  (when (file-exists-p file)
    (clime-dotenv-parse
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string))
     file)))

(defun clime-dotenv-resolve-files (spec)
  "Resolve SPEC into a list of absolute file paths.
SPEC is the value of `clime-app-dotenv': nil, t, string, or list of
strings.  Returns nil for nil SPEC.  Existence is not filtered here."
  (cond
   ((null spec) nil)
   ((eq spec t)
    (list (expand-file-name ".env" default-directory)))
   ((stringp spec)
    (list (expand-file-name spec default-directory)))
   ((listp spec)
    (mapcar (lambda (p) (expand-file-name p default-directory)) spec))
   (t (error "clime-dotenv: invalid :dotenv value: %S" spec))))

(defun clime-dotenv-merge (files)
  "Load FILES in order and merge into one alist.
Earlier files win per key.  Missing files are skipped silently;
malformed files propagate `clime-usage-error'."
  (let ((seen (make-hash-table :test #'equal))
        (out '()))
    (dolist (f files)
      (dolist (pair (clime-dotenv-load f))
        (unless (gethash (car pair) seen)
          (puthash (car pair) t seen)
          (push pair out))))
    (nreverse out)))

;;; ─── App-level scoped env binding ──────────────────────────────────────

(defmacro clime-dotenv-with-app-env (app &rest body)
  "Evaluate BODY with .env values from APP's :dotenv merged into env.
Real `process-environment' entries win over .env values.  When APP has
no :dotenv configured, BODY runs unchanged.  Restoration is automatic
via lexical rebinding of `process-environment'."
  (declare (indent 1))
  (let ((app-sym (gensym "app"))
        (spec-sym (gensym "spec")))
    `(let* ((,app-sym ,app)
            (,spec-sym (and (clime-app-p ,app-sym)
                            (clime-app-dotenv ,app-sym))))
       (if (null ,spec-sym)
           (progn ,@body)
         (let ((process-environment
                (append
                 (cl-loop for (k . v) in
                          (clime-dotenv-merge
                           (clime-dotenv-resolve-files ,spec-sym))
                          unless (getenv k)
                          collect (concat k "=" v))
                 process-environment)))
           ,@body)))))

(provide 'clime-dotenv)
;;; clime-dotenv.el ends here
