;;; clime-tests-preflight-tests.el --- Tests for test preflight checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Clime test runner preflight.

;;; Code:

(require 'ert)
(require 'clime-tests-preflight)

(defmacro clime-test-preflight--with-file (contents &rest body)
  "Write CONTENTS to a temporary Elisp file, then run BODY with `file'."
  (declare (indent 1))
  `(let ((file (make-temp-file "clime-preflight-test-" nil ".el")))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert ,contents))
           ,@body)
       (when (file-exists-p file)
         (delete-file file)))))

(ert-deftest clime-test-preflight/valid-file-passes ()
  "A syntactically valid file passes preflight."
  (clime-test-preflight--with-file "(message \"ok\")\n"
    (should (clime-test-preflight-files (list file)))))

(ert-deftest clime-test-preflight/default-files-include-sources-and-tests ()
  "Default preflight files include source and test files."
  (let* ((root (make-temp-file "clime-preflight-root-" t))
         (tests-dir (expand-file-name "tests" root))
         (source-file (expand-file-name "clime-example.el" root))
         (test-file (expand-file-name "clime-example-tests.el" tests-dir))
         (preflight-file (expand-file-name "clime-tests-preflight.el" tests-dir)))
    (unwind-protect
        (progn
          (make-directory tests-dir)
          (dolist (file (list source-file test-file preflight-file))
            (with-temp-file file
              (insert "(message \"ok\")\n")))
          (let ((files (clime-test-preflight-default-files root)))
            (should (member source-file files))
            (should (member test-file files))
            (should-not (member preflight-file files))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest clime-test-preflight/unbalanced-paren-fails-with-location ()
  "An unbalanced file fails before test evaluation and names the location."
  (clime-test-preflight--with-file "(message \"ok\"))\n"
    (let ((err (should-error (clime-test-preflight-files (list file))
                             :type 'error)))
      (let ((msg (error-message-string err)))
        (should (string-match-p (regexp-quote file) msg))
        (should (string-match-p "line [0-9]+" msg))
        (should (string-match-p "column [0-9]+" msg))))))

(ert-deftest clime-test-preflight/read-syntax-fails-with-location ()
  "A read-syntax error fails before test evaluation and names the location."
  (clime-test-preflight--with-file "(message \"unterminated)\n"
    (let ((err (should-error (clime-test-preflight-files (list file))
                             :type 'error)))
      (let ((msg (error-message-string err)))
        (should (string-match-p (regexp-quote file) msg))
        (should (string-match-p "line [0-9]+" msg))
        (should (string-match-p "column [0-9]+" msg))))))

(provide 'clime-tests-preflight-tests)
;;; clime-tests-preflight-tests.el ends here
