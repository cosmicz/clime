;;; clime-run.el --- Dispatch and runner for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Octavian <cosmicz@protonmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Command dispatch and handler invocation.  Connects parsing → context
;; creation → handler invocation → output → exit code.

;;; Code:

(require 'cl-lib)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-help)
(require 'clime-output)

;;; ─── Error Formatter ───────────────────────────────────────────────────

(defvar clime-format-error #'clime--format-error-default
  "Function called to format error messages before output.
Signature: (FORMAT-FN MESSAGE) where MESSAGE is the error string.
Default implementation prints plain text to stderr via `message'.
Rebound to `clime-output-error' in JSON mode.")

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

;;; ─── Help Printing ─────────────────────────────────────────────────────

(defun clime--print-help (data)
  "Print help or version from help-requested signal DATA."
  (let ((node (plist-get data :node))
        (path (plist-get data :path))
        (version-p (plist-get data :version)))
    (if version-p
        (princ (clime-format-version node))
      (princ (clime-format-help node path)))))

;;; ─── Public API ────────────────────────────────────────────────────────

(defun clime-run (app argv)
  "Run APP with ARGV, returning an exit code.
Exit codes: 0 = success/help/version, 1 = runtime error, 2 = usage error.
Does NOT call `kill-emacs'; the caller decides what to do with the code.

When APP has :json-mode t and ARGV contains \"--json\", output is
JSON-encoded.  The --json option is auto-injected into the app."
  ;; Reset stdin cache so each invocation reads fresh
  (setq clime--stdin-content nil)
  ;; Pre-parse --json before full parse so even parse errors emit JSON
  (let* ((json-p (and (clime-app-json-mode app)
                      (clime--pre-parse-json-p argv)))
         (clime--json-mode-p json-p)
         (clime-format-error (if json-p
                                 #'clime-output-error
                               clime-format-error)))
    (condition-case err
        (let* ((result (clime-parse app argv))
               (node (clime-parse-result-node result))
               (handler (clime-node-handler node))
               (ctx (clime--build-context app result)))
          (when handler
            (let ((retval (funcall handler ctx)))
              (when retval
                (if clime--json-mode-p
                    (clime-output-success retval)
                  (princ retval)))))
          0)
      (clime-help-requested
       (clime--print-help (cdr err))
       0)
      (clime-usage-error
       (funcall clime-format-error (cadr err))
       2)
      (error
       (if debug-on-error
           ;; Re-signal so backtrace prints
           (signal (car err) (cdr err))
         (funcall clime-format-error (error-message-string err))
         1)))))

(defun clime-main-script-p (app-name)
  "Return non-nil if APP-NAME is the main entry point.
This is the Emacs equivalent of Python's `if __name__ == \"__main__\"'.
The polyglot shebang sets CLIME_MAIN_APP=<name> before invoking
Emacs; this function checks that environment variable against
APP-NAME (a symbol).  When a file is loaded transitively via
`require', the env var is absent or names a different app."
  (and noninteractive
       (string= (or (getenv "CLIME_MAIN_APP") "")
                (symbol-name app-name))))

(defun clime-run-batch (app)
  "Run APP in batch mode.
Read argv from `command-line-args-left', strip leading \"--\",
call `clime-run', then `kill-emacs' with the exit code."
  ;; Copy args before clearing.  Use `args' not `argv' — the latter
  ;; is a defvaralias for `command-line-args-left' and would be
  ;; clobbered by the setq below under dynamic binding.
  (let ((args command-line-args-left))
    ;; Strip leading "--" inserted by the shell wrapper
    (when (and args (string= (car args) "--"))
      (setq args (cdr args)))
    ;; Prevent Emacs from processing these args itself
    (setq command-line-args-left nil)
    (kill-emacs (clime-run app args))))

(provide 'clime-run)
;;; clime-run.el ends here
