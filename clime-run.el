;;; clime-run.el --- Dispatch and runner for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Command dispatch and handler invocation.  Connects parsing → context
;; creation → handler invocation → output → exit code.

;;; Code:

(require 'cl-lib)
(require 'clime-core)
(require 'clime-parse)

;;; ─── Error Formatter ───────────────────────────────────────────────────

(defvar clime-format-error #'clime--format-error-default
  "Function called to format error messages before output.
Signature: (FORMAT-FN MESSAGE) where MESSAGE is the error string.
Default implementation prints plain text to stderr via `message'.
Rebind for JSON error output (see clime-og6).")

(defun clime--format-error-default (msg)
  "Print MSG as a plain-text error to stderr."
  (message "Error: %s" msg))

;;; ─── Context Builder ───────────────────────────────────────────────────

(defun clime--build-context (app parse-result)
  "Build a `clime-context' from APP and PARSE-RESULT."
  (clime-context--create
   :app app
   :command (clime-parse-result-command parse-result)
   :path (clime-parse-result-path parse-result)
   :params (clime-parse-result-params parse-result)))

;;; ─── Help Stub ─────────────────────────────────────────────────────────

(defun clime--print-help-stub (data)
  "Print a minimal help stub from help-requested signal DATA.
Real help formatting is provided by the help system (clime-8fr)."
  (let ((node (plist-get data :node))
        (version-p (plist-get data :version)))
    (if version-p
        (princ (format "%s %s\n"
                       (clime--node-name node)
                       (clime-app-version node)))
      (princ (format "Usage: %s\n"
                     (string-join (plist-get data :path) " "))))))

;;; ─── Public API ────────────────────────────────────────────────────────

(defun clime-run (app argv)
  "Run APP with ARGV, returning an exit code.
Exit codes: 0 = success/help/version, 1 = runtime error, 2 = usage error.
Does NOT call `kill-emacs'; the caller decides what to do with the code."
  (condition-case err
      (let* ((result (clime-parse app argv))
             (cmd (clime-parse-result-command result))
             (handler (when cmd (clime-command-handler cmd)))
             (ctx (clime--build-context app result)))
        (when handler
          (let ((retval (funcall handler ctx)))
            (when retval
              (princ retval))))
        0)
    (clime-help-requested
     (clime--print-help-stub (cdr err))
     0)
    (clime-usage-error
     (funcall clime-format-error (cadr err))
     2)
    (error
     (if debug-on-error
         ;; Re-signal so backtrace prints
         (signal (car err) (cdr err))
       (funcall clime-format-error (error-message-string err))
       1))))

(defun clime-run-batch (app)
  "Run APP in batch mode.
Read argv from `command-line-args-left', strip leading \"--\",
call `clime-run', then `kill-emacs' with the exit code."
  (let ((argv command-line-args-left))
    ;; Strip leading "--" inserted by the shell wrapper
    (when (and argv (string= (car argv) "--"))
      (setq argv (cdr argv)))
    ;; Prevent Emacs from processing these args itself
    (setq command-line-args-left nil)
    (kill-emacs (clime-run app argv))))

(provide 'clime-run)
;;; clime-run.el ends here
