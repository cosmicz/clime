;;; greeter.el --- Example clime app: emacsclient invocation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minimal example demonstrating clime's emacsclient mode.
;; Instead of spawning `emacs --batch' per invocation (~300ms),
;; the generated wrapper talks to a running Emacs daemon via
;; `emacsclient --eval', giving near-instant startup (~10ms).
;;
;; Features demonstrated:
;;
;;   1.  emacsclient wrapper      `clime-make init --client --output'
;;   2.  JSON output format       `clime-output-format' with --json
;;   3.  Command with args        hello <name>
;;   4.  Boolean flag             --shout
;;   5.  Optional arg             fortune [topic]
;;   6.  Live Emacs access        buffers command reads buffer list
;;
;; Setup (from the clime repo root):
;;   make bin/greeter
;;
;; Usage (requires a running Emacs daemon — `emacs --daemon'):
;;   bin/greeter hello World
;;   bin/greeter hello World --shout
;;   bin/greeter hello World --json
;;   bin/greeter fortune
;;   bin/greeter buffers
;;   bin/greeter buffers --json
;;
;; The `buffers' command is the interesting one — it queries the
;; live Emacs session's buffer list, something batch mode cannot do.
;;
;; Connection options (parsed by the wrapper, not the app):
;;   bin/greeter --socket-name /tmp/emacs1000/server hello World
;;   EMACS_SOCKET_NAME=/tmp/emacs1000/server bin/greeter hello World

;;; Code:

(require 'clime)

(clime-app greeter
  :version "1.0"
  :help "A greeting CLI demonstrating emacsclient invocation."
  :examples '(("greeter hello World" . "Say hello")
              ("greeter hello World --shout" . "Say hello loudly")
              ("greeter buffers --json" . "List daemon buffers as JSON"))

  (clime-output-format json ("--json") :help "Output as JSON")

  ;; ── hello ──────────────────────────────────────────────────────────

  (clime-command hello
    :help "Greet someone by name."

    (clime-arg name :help "Name to greet")

    (clime-opt shout ("--shout" "-s") :bool
      :help "SHOUT the greeting")

    (clime-handler (ctx)
      (clime-let ctx (name shout)
        (let ((msg (format "Hello, %s!" name)))
          (when shout (setq msg (upcase msg)))
          (clime-out `((greeting . ,msg)) :text msg)
          nil))))

  ;; ── fortune ────────────────────────────────────────────────────────

  (clime-command fortune
    :help "Print a random fortune."

    (clime-arg topic :required nil
      :help "Topic for the fortune")

    (clime-handler (ctx)
      (clime-let ctx (topic)
        (let* ((fortunes '("Languid afternoons are underrated."
                           "The parenthesis is mightier than the sword."
                           "You will write one more Emacs package."))
               (msg (nth (random (length fortunes)) fortunes))
               (msg (if topic (format "[%s] %s" topic msg) msg)))
          (clime-out `((fortune . ,msg)) :text msg)
          nil))))

  ;; ── buffers ────────────────────────────────────────────────────────
  ;; This command demonstrates the key advantage of emacsclient mode:
  ;; it can query the live Emacs session's state.

  (clime-command buffers
    :help "List buffers in the running Emacs daemon."
    :examples '(("greeter buffers" . "List buffer names")
                ("greeter buffers --json" . "List as JSON array"))

    (clime-opt files-only ("--files" "-f") :bool
      :help "Only show file-visiting buffers")

    (clime-handler (ctx)
      (clime-let ctx (files-only)
        (let ((bufs (if files-only
                       (cl-remove-if-not #'buffer-file-name (buffer-list))
                     (buffer-list))))
          (dolist (buf bufs)
            (let ((name (buffer-name buf)))
              (clime-out `((buffer . ,name)) :text name))))))))

(provide 'greeter)
;;; Entrypoint:
(when (clime-main-script-p 'greeter)
  (clime-run-batch greeter))
;;; greeter.el ends here
