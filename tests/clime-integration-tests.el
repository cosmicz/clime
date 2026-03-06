;;; clime-integration-tests.el --- Integration tests for clime  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Integration tests that spawn real Emacs subprocesses to verify
;; end-to-end behavior: shebang execution, dist as CLI, dist as
;; library, and the CLIME_MAIN guard.

;;; Code:

(require 'ert)
(require 'clime-test-helpers)

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defvar clime-test--project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the clime project.")

(defun clime-test--run-script (script-path args &optional env)
  "Run SCRIPT-PATH with ARGS as a subprocess, returning (EXIT . OUTPUT).
ENV is an optional alist of (NAME . VALUE) pairs to add to the
process environment."
  (let* ((process-environment
          (append (mapcar (lambda (e) (format "%s=%s" (car e) (cdr e)))
                          env)
                  process-environment))
         (buf (generate-new-buffer " *clime-test-subprocess*"))
         (exit-code
          (apply #'call-process script-path nil buf nil args))
         (output (with-current-buffer buf
                   (string-trim (buffer-string)))))
    (kill-buffer buf)
    (cons exit-code output)))

(defun clime-test--write-app-source (file-path)
  "Write a minimal clime app to FILE-PATH (without shebang)."
  (with-temp-file file-path
    (insert ";;; test-app.el --- test  -*- lexical-binding: t; -*-\n"
            ";;; Code:\n"
            "(require 'clime)\n"
            "(clime-app test-app :version \"1.0.0\" :help \"Test app.\"\n"
            "  (clime-command hello :help \"Say hello\"\n"
            "    (clime-arg name :help \"Name\")\n"
            "    (clime-handler (ctx)\n"
            "      (clime-let ctx (name) (format \"Hello, %s!\" name)))))\n"
            "(clime-run-batch test-app)\n"
            "(provide 'test-app)\n"
            ";;; test-app.el ends here\n")))

;;; ─── Dist as CLI ────────────────────────────────────────────────────────

(ert-deftest clime-test-integration/dist-as-cli ()
  "The dist file works as a CLI tool when executed directly."
  (let* ((dist (expand-file-name "dist/clime.el" clime-test--project-root))
         (result (clime-test--run-script dist '("--help"))))
    (should (= 0 (car result)))
    (should (string-match-p "clime COMMAND" (cdr result)))
    (should (string-match-p "init" (cdr result)))))

;;; ─── Dist as Library ────────────────────────────────────────────────────

(ert-deftest clime-test-integration/dist-as-library ()
  "The dist file works as a library without triggering the CLI entry point."
  (clime-test-with-temp-dir
    (let* ((dist-dir (expand-file-name "dist/" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      ;; Write app source without shebang
      (clime-test--write-app-source app-file)
      ;; Run it via emacs --batch, loading dist as library
      (let* ((buf (generate-new-buffer " *clime-test-lib*"))
             (exit-code
              (call-process "emacs" nil buf nil
                            "--batch" "-Q"
                            "-L" dist-dir
                            "-l" app-file
                            "--" "hello" "world"))
             (output (with-current-buffer buf
                       (string-trim (buffer-string)))))
        (kill-buffer buf)
        (should (= 0 exit-code))
        (should (equal "Hello, world!" output))))))

;;; ─── Init ───────────────────────────────────────────────────────────────

(ert-deftest clime-test-integration/init-adds-shebang ()
  "clime-app.el init adds a working shebang to an .el file."
  (clime-test-with-temp-dir
    (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; Init it
      (let ((result (clime-test--run-script clime-app (list "init" app-file))))
        (should (= 0 (car result)))
        (should (string-match-p "is now executable" (cdr result))))
      ;; Verify shebang was added
      (with-temp-buffer
        (insert-file-contents app-file)
        (should (string-prefix-p "#!/bin/sh" (buffer-string))))
      ;; Verify the file is executable and runs
      (let ((result (clime-test--run-script app-file '("hello" "world"))))
        (should (= 0 (car result)))
        (should (equal "Hello, world!" (cdr result)))))))

(ert-deftest clime-test-integration/init-standalone ()
  "clime-app.el init --standalone uses self-relative load path."
  (clime-test-with-temp-dir
    (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-app (list "init" "--standalone" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        (should (string-match-p "\\$(dirname \"\\$0\")" (buffer-string)))
        ;; Should NOT contain an absolute path
        (should-not (string-match-p "-L \"/.*\"" (buffer-string)))))))

(ert-deftest clime-test-integration/init-env ()
  "clime-app.el init --env injects environment variables into shebang."
  (clime-test-with-temp-dir
    (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-app
                              (list "init" "--env" "FOO=bar"
                                    "--env" "BAZ=qux" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        (let ((line2 (progn (forward-line 1)
                            (buffer-substring (point) (line-end-position)))))
          (should (string-match-p "FOO=bar" line2))
          (should (string-match-p "BAZ=qux" line2)))))))

(ert-deftest clime-test-integration/init-rejects-existing-shebang ()
  "clime-app.el init errors on a file that already has a shebang."
  (clime-test-with-temp-dir
    (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; First init succeeds
      (clime-test--run-script clime-app (list "init" app-file))
      ;; Second init should fail
      (let ((result (clime-test--run-script clime-app (list "init" app-file))))
        (should (= 2 (car result)))
        (should (string-match-p "already has a shebang" (cdr result)))))))

(ert-deftest clime-test-integration/init-rejects-missing-file ()
  "clime-app.el init errors on a nonexistent file."
  (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
         (result (clime-test--run-script
                  clime-app (list "init" "/tmp/does-not-exist.el"))))
    (should (= 2 (car result)))
    (should (string-match-p "does not exist" (cdr result)))))

;;; ─── Example App ────────────────────────────────────────────────────────

(ert-deftest clime-test-integration/pkm-example ()
  "The pkm example app runs end-to-end."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("install" "foo" "--tag" "dev"))))
    (should (= 0 (car result)))
    (should (string-match-p "Installing foo" (cdr result)))
    (should (string-match-p "tags=dev" (cdr result)))))

(provide 'clime-integration-tests)
;;; clime-integration-tests.el ends here
