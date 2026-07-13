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
(require 'clime-dotenv)

;;; ─── Invocation Surface / Error Capture ────────────────────────────────

(defvar clime--invocation-surface nil
  "Entry surface for the current invocation, bound by the entry points.
`clime-run' uses `cli' directly; `clime-run-from-values' reads this var,
defaulting to `run-from-values'.  Bound to `serve' by serve dispatch and
to `invoke' by the interactive runner.")

(defvar clime-run--last-error nil
  "Internal: (TYPE . MESSAGE) of the most recent handler runtime error.
Set by `clime-run--execute' when a handler signals (and `debug-on-error'
is nil), consumed by the invocation-event firing in `clime-run' and
`clime-run-from-values' to populate `runtime-error' events.  Reset to nil
at the start of every `clime-run--execute'.")

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
Returns integer exit code: 0 success, 1 runtime error.
After execution, fires `:after-execute' hooks on the app (if any)."
  (let* ((fmt clime-out--active-format)
         (streaming (clime-output-format-streaming fmt))
         (clime-out--items nil)
         (clime-out--errors nil)
         (retval nil)
         (_t0 (setf (clime-context-start-time ctx) (float-time)))
         (_reset (setq clime-run--last-error nil))
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
               (setq clime-run--last-error
                     (cons (car err) (error-message-string err)))
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
      (let ((final-code (if has-errors 1 0)))
        (clime-run--fire-after-execute ctx final-code
                                        (- (float-time)
                                           (clime-context-start-time ctx)))
        final-code))))

(defun clime-run--fire-after-execute (ctx exit-code duration)
  "Fire after-execute hooks from CTX's app with EXIT-CODE and DURATION.
Each hook is called inside `condition-case'; errors are reported via
`message' and do not propagate or alter the exit code."
  (when-let ((app (clime-context-app ctx)))
    (dolist (hook (clime-app-after-execute app))
      (condition-case err
          (funcall hook ctx exit-code duration)
        (error
         (message "clime: after-execute hook error: %s"
                  (error-message-string err)))))))

(defun clime-run--fire-invocation (app event)
  "Fire `:on-invocation' hooks on APP with the `clime-invocation-event' EVENT.
Each hook is called inside `condition-case'; errors are reported via
`message' and never propagate or alter the outcome.  No-op when APP has
no hooks."
  (when app
    (dolist (hook (clime-app-on-invocation app))
      (condition-case err
          (funcall hook event)
        (error
         (message "clime: on-invocation hook error: %s"
                  (error-message-string err)))))))

;;; ─── Values → Execute Pipeline ──────────────────────────────────────────

(defun clime-run-from-values (app node path values)
  "Run NODE's handler from VALUES.
Return (EXIT-CODE . OUTPUT).
Creates a parse-result, finalizes it, builds a context, and executes
the handler.  Output is captured via `with-output-to-string'.

APP is the root app.  NODE is the terminal node (command or group with
handler).  PATH is the command path list.  VALUES is an alist of
\(NAME . (VALUE . SOURCE)) pairs.

Exit codes: 0 = success/help/version, 1 = runtime error, 2 = usage error."
  ;; Coerce string values to declared types before building the parse-result.
  ;; CLI pass-1 and invoke UI produce typed values; this handles serve/IPC
  ;; where query params arrive as strings.
  (let ((scope (cons node (clime-node-ancestors node))))
    (setq values (clime--coerce-string-values scope values)))
  (let* ((start-time (float-time))
         (event (clime-invocation-event--create
                 :app app
                 :surface (or clime--invocation-surface 'run-from-values)
                 :path path :display-path path
                 :params (clime-values-plist values)
                 :command (and (clime-command-p node) node)
                 :format clime-out--active-format
                 :start-time start-time))
         (result (clime-parse-result--create
                  :command (if (clime-command-p node) node nil)
                  :node node
                  :path path
                  :display-path path
                  :params (clime-values-plist values)
                  :values (copy-sequence values)
                  :tree app))
         (exit-code nil)
         (output (with-output-to-string
                   (setq exit-code
                         (condition-case err
                             (clime-dotenv-with-app-env app
                               (clime-parse-finalize result)
                               (let ((ctx (clime--build-context app result)))
                                 (setf (clime-invocation-event-context event) ctx
                                       (clime-invocation-event-params event) (clime-context-params ctx)
                                       (clime-invocation-event-path event) (clime-context-path ctx))
                                 (let ((code (clime-run--execute
                                              (clime-node-handler node) ctx)))
                                   (if clime-run--last-error
                                       (setf (clime-invocation-event-phase event) 'runtime-error
                                             (clime-invocation-event-error-type event) (car clime-run--last-error)
                                             (clime-invocation-event-error-message event) (cdr clime-run--last-error))
                                     (setf (clime-invocation-event-phase event) 'completed))
                                   code)))
                           (clime-usage-error
                            ;; For a STRUCTURED format (e.g. clime-serve with
                            ;; a json default-format) render the message via
                            ;; the format's error handler so the body is the
                            ;; format's error envelope, not raw text.  The
                            ;; default `text' handler emits to `message'
                            ;; (stderr); keep the historical raw `princ' to
                            ;; stdout for it so plain-text callers (e.g.
                            ;; clime-invoke) are unchanged.
                            (if (eq (clime-output-format-name
                                     clime-out--active-format)
                                    'text)
                                (princ (cadr err))
                              (funcall (clime-output-format-error-handler
                                        clime-out--active-format)
                                       (cadr err)))
                            (setf (clime-invocation-event-phase event) 'usage-error
                                  (clime-invocation-event-error-type event) (car err)
                                  (clime-invocation-event-error-message event) (cadr err))
                            2)
                           (clime-help-requested
                            (clime--print-help (cdr err))
                            (setf (clime-invocation-event-phase event)
                                  (if (plist-get (cdr err) :version) 'version 'help))
                            0)
                           (error
                            (princ (error-message-string err))
                            (setf (clime-invocation-event-phase event) 'runtime-error
                                  (clime-invocation-event-error-type event) (car err)
                                  (clime-invocation-event-error-message event) (error-message-string err))
                            1))))))
    (setf (clime-invocation-event-exit-code event) (or exit-code 0)
          (clime-invocation-event-duration event) (- (float-time) start-time))
    (clime-run--fire-invocation app event)
    (cons (or exit-code 0) output)))

;;; ─── Public API ────────────────────────────────────────────────────────

(defun clime--collect-output-formats (node)
  "Collect output-format structs declared on NODE and its descendants.
NODE's own formats come first, so app-level (root) formats take precedence
over group-declared ones with the same flag.  Non-branch nodes (aliases)
have no formats and are skipped."
  (when (clime-group-p node)
    (append (clime-group-output-formats node)
            (cl-mapcan (lambda (entry) (clime--collect-output-formats (cdr entry)))
                       (clime-group-children node)))))

(defun clime--detect-output-format (app argv)
  "Detect active output format from APP's tree and ARGV.
Scans output-formats declared anywhere in the tree (app + group/command
subtrees) for a flag present in ARGV.  Returns the matching
`clime-output-format' struct, or nil for text mode."
  (cl-find-if (lambda (fmt)
                (cl-some (lambda (flag) (member flag argv))
                         (clime-option-flags fmt)))
              (clime--collect-output-formats app)))

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
         (clime-out--active-format (or active-fmt clime-out--active-format))
         (start-time (float-time))
         (event (clime-invocation-event--create
                 :app app :surface 'cli :argv argv
                 :format active-fmt :start-time start-time))
         (exit-code
          (condition-case err
              (clime-dotenv-with-app-env app
                (let* ((setup (clime-app-setup app))
                       (config-factory (clime-app-config app))
                       (two-pass (or setup config-factory))
                       (result (clime-parse app argv (and two-pass t))))
                  ;; When setup or config exists: run between passes, then finalize
                  (when two-pass
                    (when setup
                      (funcall setup app result))
                    (when config-factory
                      (let ((provider (funcall config-factory app result)))
                        (when provider
                          (setf (clime-parse-result-config-provider result) provider))))
                    (clime-parse-finalize result))
                  (let* ((node (clime-parse-result-node result))
                         (handler (clime-node-handler node))
                         (ctx (clime--build-context app result)))
                    (let ((dep (clime-node-deprecated node)))
                      (when dep
                        (message "Warning: %s is deprecated%s"
                                 (clime-node-name node)
                                 (if (stringp dep) (format ". %s" dep) ""))))
                    (setf (clime-invocation-event-context event) ctx
                          (clime-invocation-event-command event) (clime-context-command ctx)
                          (clime-invocation-event-path event) (clime-context-path ctx)
                          (clime-invocation-event-display-path event) (clime-parse-result-display-path result)
                          (clime-invocation-event-params event) (clime-context-params ctx))
                    (if (not handler)
                        (progn
                          (setf (clime-invocation-event-phase event) 'no-handler)
                          0)
                      (let ((code (clime-run--execute handler ctx)))
                        (if clime-run--last-error
                            (setf (clime-invocation-event-phase event) 'runtime-error
                                  (clime-invocation-event-error-type event) (car clime-run--last-error)
                                  (clime-invocation-event-error-message event) (cdr clime-run--last-error))
                          (setf (clime-invocation-event-phase event) 'completed))
                        code)))))
            (clime-help-requested
             (clime--print-help (cdr err))
             (setf (clime-invocation-event-phase event)
                   (if (plist-get (cdr err) :version) 'version 'help)
                   (clime-invocation-event-path event) (plist-get (cdr err) :path))
             0)
            (clime-usage-error
             (funcall (clime-output-format-error-handler clime-out--active-format) (cadr err))
             (when-let ((err-path (plist-get (cddr err) :path)))
               (funcall (clime-output-format-error-handler clime-out--active-format)
                        (format "Try '%s --help' for more information."
                                (string-join err-path " "))))
             (setf (clime-invocation-event-phase event) 'usage-error
                   (clime-invocation-event-error-type event) (car err)
                   (clime-invocation-event-error-message event) (cadr err)
                   (clime-invocation-event-path event) (plist-get (cddr err) :path))
             2)
            (error
             ;; In debug mode, re-signal so a backtrace prints; the unified
             ;; hook does NOT fire on this path (consistent with after-execute).
             (if debug-on-error
                 (signal (car err) (cdr err))
               (funcall (clime-output-format-error-handler clime-out--active-format) (error-message-string err))
               (setf (clime-invocation-event-phase event) 'runtime-error
                     (clime-invocation-event-error-type event) (car err)
                     (clime-invocation-event-error-message event) (error-message-string err))
               1)))))
    (setf (clime-invocation-event-exit-code event) exit-code
          (clime-invocation-event-duration event) (- (float-time) start-time))
    (clime-run--fire-invocation app event)
    exit-code))

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
                (inhibit-message t)
                (debug-on-error nil))
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
