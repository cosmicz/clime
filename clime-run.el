;;; clime-run.el --- Dispatch and runner for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
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

(defun clime--detect-output-format (app argv)
  "Detect active output format from APP's output-formats and ARGV.
Returns the matching `clime-output-format' struct, or nil for text mode."
  (cl-find-if (lambda (fmt)
                (cl-some (lambda (flag) (member flag argv))
                         (clime-option-flags fmt)))
              (clime-app-output-formats app)))

(defun clime-run (app argv)
  "Run APP with ARGV, returning an exit code.
Exit codes: 0 = success/help/version, 1 = runtime error, 2 = usage error.
Does NOT call `kill-emacs'; the caller decides what to do with the code.

Output format detection: checks `clime-app-output-formats' for a matching
flag in ARGV.  When matched, `clime-output-mode' is bound to the format
name and the format's finalize/streaming config drives output behavior."
  ;; Reset stdin cache so each invocation reads fresh
  (setq clime--stdin-content nil)
  ;; Pre-parse output format before full parse so even parse errors emit correctly
  (let* ((active-fmt (clime--detect-output-format app argv))
         (fmt-name (and active-fmt (clime-output-format-name active-fmt)))
         (json-p (eq fmt-name 'json))
         (clime-output-mode (or fmt-name 'text))
         (clime-format-error (if json-p
                                 #'clime-output-error
                               clime-format-error)))
    (condition-case err
        (let* ((setup (clime-app-setup app))
               (result (clime-parse app argv (and setup t))))
          ;; When setup hook exists: call it, then finalize (pass 2)
          (when setup
            (funcall setup app result)
            (clime-parse-finalize result))
          (let* ((node (clime-parse-result-node result))
                 (handler (clime-node-handler node))
                 (ctx (clime--build-context app result)))
            (let ((dep (clime-node-deprecated node)))
              (when dep
                (message "Warning: %s is deprecated%s"
                         (clime-node-name node)
                         (if (stringp dep) (format ". %s" dep) ""))))
            (when handler
              (let* ((streaming (and active-fmt
                                     (clime-output-format-streaming active-fmt)))
                     (clime--output-buffer (and active-fmt (not streaming)
                                               '(:buffer)))
                     (retval (funcall handler ctx)))
                (cond
                 ;; Streaming mode: output already emitted per-call, nothing to flush
                 (streaming nil)
                 ;; Buffered mode with items: flush with format's finalize
                 ((and active-fmt (cdr clime--output-buffer))
                  (clime--output-flush
                   (clime-output-format-finalize active-fmt)
                   retval))
                 ;; No buffered output — use finalize with retval only
                 (retval
                  (if active-fmt
                      (let ((fn (or (clime-output-format-finalize active-fmt)
                                    #'clime--output-finalize-default)))
                        (let ((data (funcall fn nil retval)))
                          (when data
                            (princ (concat (clime-json-encode data) "\n")))))
                    (princ retval))))))
            0))
      (clime-help-requested
       (clime--print-help (cdr err))
       0)
      (clime-usage-error
       (funcall clime-format-error (cadr err))
       (let ((err-path (caddr err)))
         (when err-path
           (funcall clime-format-error
                    (format "Try '%s --help' for more information."
                            (string-join err-path " ")))))
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
call `clime-run', then `kill-emacs' with the exit code.
When CLIME_ARGV0 is set (by the shebang), uses its basename
as the program name in usage output instead of the DSL symbol.
No-op when called from an interactive Emacs session."
  (unless noninteractive
    (message "Warning: (clime-run-batch %s) ignored in interactive mode"
             (clime-node-name app)))
  (when noninteractive
    ;; Copy args before clearing.  Use `args' not `argv' — the latter
    ;; is a defvaralias for `command-line-args-left' and would be
    ;; clobbered by the setq below under dynamic binding.
    (let ((args command-line-args-left)
          (argv0 (getenv "CLIME_ARGV0")))
      ;; Override usage program name with the executable filename
      (when (and argv0 (not (string-empty-p argv0)))
        (setf (clime-app-argv0 app)
              (file-name-nondirectory argv0)))
      ;; Strip leading "--" inserted by the shell wrapper
      (when (and args (string= (car args) "--"))
        (setq args (cdr args)))
      ;; Prevent Emacs from processing these args itself
      (setq command-line-args-left nil)
      (kill-emacs (clime-run app args)))))

(provide 'clime-run)
;;; clime-run.el ends here
