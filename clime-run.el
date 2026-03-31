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

;;; ─── Handler Execution ────────────────────────────────────────────────

(defun clime-run--execute (handler ctx)
  "Call HANDLER with CTX, flushing output and returning exit code.
Uses `clime-out--active-format' for output routing.
Re-signals `clime-usage-error' and `clime-help-requested' to caller.
Returns integer exit code: 0 success, 1 runtime error."
  (let* ((fmt clime-out--active-format)
         (streaming (clime-output-format-streaming fmt))
         (clime-out--items nil)
         (clime-out--errors nil)
         (retval nil)
         (exit-code
          (condition-case err
              (progn (setq retval (funcall handler ctx)) 0)
            (clime-usage-error
             (signal (car err) (cdr err)))
            (clime-help-requested
             (signal (car err) (cdr err)))
            (error
             (if debug-on-error
                 (signal (car err) (cdr err))
               (if streaming
                   (funcall (clime-output-format-error-handler fmt)
                            (error-message-string err))
                 (push (error-message-string err) clime-out--errors))
               1)))))
    (let ((has-errors (or clime-out--errors (> exit-code 0))))
      (if streaming
          (when retval
            (princ (funcall (clime-output-format-encoder fmt) retval))
            (terpri))
        (clime-out--flush (clime-output-format-finalize fmt) retval))
      (if has-errors 1 0))))

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
flag in ARGV.  When matched, `clime-out--active-format' is bound to
the format and drives all output behavior through the format struct."
  ;; Reset stdin cache so each invocation reads fresh
  (setq clime--stdin-content nil)
  ;; Pre-parse output format before full parse so even parse errors emit correctly
  (let* ((active-fmt (clime--detect-output-format app argv))
         (clime-out--active-format (or active-fmt clime-out--active-format)))
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
            (if (not handler)
                0
              (clime-run--execute handler ctx))))
      (clime-help-requested
       (clime--print-help (cdr err))
       0)
      (clime-usage-error
       (funcall (clime-output-format-error-handler clime-out--active-format) (cadr err))
       (let ((err-path (caddr err)))
         (when err-path
           (funcall (clime-output-format-error-handler clime-out--active-format)
                    (format "Try '%s --help' for more information."
                            (string-join err-path " ")))))
       2)
      (error
       (if debug-on-error
           ;; Re-signal so backtrace prints
           (signal (car err) (cdr err))
         (funcall (clime-output-format-error-handler clime-out--active-format) (error-message-string err))
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
    (display-warning 'clime
                     (format "(clime-run-batch %s) ignored in interactive mode"
                             (clime-node-name app))))
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

;;; ─── Emacsclient Dispatch ─────────────────────────────────────────────

(defun clime-run-client (app-sym dir &rest plist)
  "Run the clime app named by APP-SYM via emacsclient IPC.
DIR is the temp directory for file-based communication:
  DIR/argv — null-delimited argument list (input)
  DIR/in   — stdin content, read via CLIME_STDIN_FILE (input, optional)
  DIR/out  — captured stdout (output)
  DIR/err  — captured stderr/messages (output)
  DIR/exit — integer exit code (output)

PLIST accepts:
  :load-path  List of directories to add to `load-path'.
  :file       Path to the .el file defining the app (reloaded each call).

Does NOT call `kill-emacs'.  Returns nil."
  ;; Load app — always reload so file changes are picked up
  (dolist (p (plist-get plist :load-path))
    (add-to-list 'load-path p))
  (let ((app-file (plist-get plist :file)))
    (when app-file
      (load app-file nil t)))
  ;; Read args from null-delimited file
  (let* ((argv-file (expand-file-name "argv" dir))
         (args (when (file-exists-p argv-file)
                 (with-temp-buffer
                   (insert-file-contents argv-file)
                   (split-string (buffer-string) "\0" t))))
         ;; Set up stdin file if present
         (in-file (expand-file-name "in" dir))
         (process-environment
          (if (file-exists-p in-file)
              (cons (concat "CLIME_STDIN_FILE=" in-file) process-environment)
            process-environment))
         ;; Capture stdout
         (out-buf (generate-new-buffer " *clime-client-out*"))
         (err-msgs nil)
         (exit-code
          (let ((standard-output out-buf)
                (inhibit-message t))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest margs)
                         (push (apply #'format fmt margs) err-msgs))))
              (clime-run (symbol-value app-sym) args)))))
    ;; Write results
    (with-temp-file (expand-file-name "out" dir)
      (insert-buffer-substring out-buf))
    (with-temp-file (expand-file-name "err" dir)
      (dolist (msg (nreverse err-msgs))
        (insert msg "\n")))
    (with-temp-file (expand-file-name "exit" dir)
      (insert (number-to-string (or exit-code 0))))
    (kill-buffer out-buf)
    nil))

(provide 'clime-run)
;;; clime-run.el ends here
