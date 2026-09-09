;;; clime-tests-preflight.el --- Preflight checks for clime tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Syntax/read checks that run before loading the normal test runner.

;;; Code:

(require 'cl-lib)

(defun clime-test-preflight--location ()
  "Return current buffer location as a cons cell of line and column."
  (cons (line-number-at-pos) (1+ (current-column))))

(defun clime-test-preflight--fail (file phase err)
  "Signal a preflight error for FILE PHASE and underlying ERR."
  (pcase-let ((`(,line . ,column) (clime-test-preflight--location)))
    (error "Preflight %s failed for %s at line %d column %d: %s"
           phase file line column (error-message-string err))))

(defun clime-test-preflight--check-parens (file)
  "Run `check-parens' against FILE."
  (goto-char (point-min))
  (condition-case err
      (let ((inhibit-message t))
        (check-parens))
    (error
     (clime-test-preflight--fail file "check-parens" err))))

(defun clime-test-preflight--read-forms (file)
  "Read every form in FILE without evaluating it."
  (goto-char (point-min))
  (condition-case err
      (while t
        (read (current-buffer)))
    (end-of-file t)
    (error
     (clime-test-preflight--fail file "read" err))))

(defun clime-test-preflight-file (file)
  "Preflight one Emacs Lisp FILE before test loading."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (clime-test-preflight--check-parens file)
    (clime-test-preflight--read-forms file))
  t)

(defun clime-test-preflight-files (files)
  "Preflight FILES before test loading."
  (dolist (file files t)
    (clime-test-preflight-file file)))

(defun clime-test-preflight--elisp-files (directory)
  "Return sorted Emacs Lisp files directly under DIRECTORY."
  (when (file-directory-p directory)
    (sort (directory-files directory t "\\.el\\'") #'string<)))

(defun clime-test-preflight-default-files (&optional root)
  "Return project source and test files to preflight under ROOT."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (tests-dir (expand-file-name "tests" root))
         (preflight-file (file-truename
                          (expand-file-name "tests/clime-tests-preflight.el"
                                            root)))
         (files (append (clime-test-preflight--elisp-files root)
                        (clime-test-preflight--elisp-files tests-dir))))
    (cl-remove-if
     (lambda (file)
       (string= (file-truename file) preflight-file))
     files)))

(defun clime-test-preflight-project (&optional root)
  "Preflight Clime source and test files under ROOT."
  (clime-test-preflight-files (clime-test-preflight-default-files root)))

(provide 'clime-tests-preflight)
;;; clime-tests-preflight.el ends here
