;;; transports.el --- One command through two local RPC adapters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; From a source checkout:
;; emacs --batch -Q -L . -l examples/transports.el \
;;   --eval '(prin1 (clime-example-transports-demo))'
;; Both responses have status 200 and body "Hello, Ada!\n".  No server starts.

;;; Code:

(require 'clime)
(require 'clime-pipe)
(require 'clime-spool)

(clime-app clime-example-transports
  :help "One greeting command, shared by CLI and local RPC."
  (clime-command hello
    :surfaces '(cli serve)
    :help "Greet a name."
    (clime-arg name :help "Name to greet")
    (clime-handler (ctx)
      (format "Hello, %s!" (clime-ctx-get ctx 'name)))))

(defun clime-example-transports-demo ()
  "Return pipe and spool responses for the same greeting request."
  (let ((request '(:id "r1" :path ("hello")
                  :inputs ((:name name :value "Ada"))))
        (spool-root (make-temp-file "clime-transports-example-" t)))
    (unwind-protect
        (let ((pipe-response
               (clime-pipe-dispatch-frame
                clime-example-transports
                (append '(:clime/pipe-request 1) request))))
          ;; No blocking wait until this process has also done the worker's job.
          (clime-spool-submit-request
           spool-root (append '(:clime/spool-request 1) request) :wait nil)
          (clime-spool-process-one clime-example-transports spool-root)
          (list pipe-response
                (clime-spool-wait-for-response spool-root "r1" 1)))
      (delete-directory spool-root t))))

(provide 'clime-example-transports)
;;; transports.el ends here
