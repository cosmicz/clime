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
  "clime-app.el init --standalone skips the automatic clime load path."
  (clime-test-with-temp-dir
    (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-app (list "init" "--standalone" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        ;; Should NOT contain any -L path (case-sensitive: -L not -l)
        (let ((case-fold-search nil))
          (should-not (string-match-p " -L " (buffer-string))))))))

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

;;; ─── Dist Command ───────────────────────────────────────────────────────

(defun clime-test--write-module (file-path feature code)
  "Write a minimal Elisp module to FILE-PATH.
FEATURE is the provide symbol, CODE is the body between markers."
  (with-temp-file file-path
    (insert (format ";;; %s --- test module  -*- lexical-binding: t; -*-\n"
                    (file-name-nondirectory file-path))
            ";;; Code:\n"
            code "\n"
            (format "(provide '%s)\n" feature)
            (format ";;; %s ends here\n"
                    (file-name-nondirectory file-path)))))

(ert-deftest clime-test-integration/dist-basic-bundle ()
  "clime dist bundles multiple source files into one."
  (clime-test-with-temp-dir
    (let ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      ;; Write two source modules
      (clime-test--write-module "core.el" 'mycore
                                "(defun mycore-greet () \"hi\")")
      (clime-test--write-module "main.el" 'mymain
                                "(require 'mycore)\n(defun mymain-run () (mycore-greet))")
      ;; Bundle them
      (let ((result (clime-test--run-script
                     clime-app
                     (list "dist" "-o" out "--provide" "mybundle"
                           "--no-shebang"
                           (expand-file-name "core.el")
                           (expand-file-name "main.el")))))
        (should (= 0 (car result))))
      ;; Verify output
      (with-temp-buffer
        (insert-file-contents out)
        (let ((content (buffer-string)))
          ;; Has header with lexical-binding
          (should (string-match-p "lexical-binding: t" content))
          ;; Contains extracted code from both files
          (should (string-match-p "mycore-greet" content))
          (should (string-match-p "mymain-run" content))
          ;; Has provide
          (should (string-match-p "(provide 'mybundle)" content))
          ;; Does NOT contain original provide lines
          (should-not (string-match-p "(provide 'mycore)" content))
          (should-not (string-match-p "(provide 'mymain)" content))
          ;; No shebang
          (should-not (string-prefix-p "#!" content)))))))

(ert-deftest clime-test-integration/dist-strips-run-batch ()
  "clime dist strips (clime-run-batch ...) from extracted code."
  (clime-test-with-temp-dir
    (let ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp
                                "(defvar myapp nil)\n(clime-run-batch myapp)")
      (let ((result (clime-test--run-script
                     clime-app
                     (list "dist" "-o" out "--no-shebang"
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "defvar myapp" (buffer-string)))
        (should-not (string-match-p "clime-run-batch" (buffer-string)))))))

(ert-deftest clime-test-integration/dist-main-guard ()
  "clime dist --main adds guarded entry point and CLIME_MAIN_APP env."
  (clime-test-with-temp-dir
    (let ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-app
                     (list "dist" "-o" out "--main" "myapp"
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (let ((content (buffer-string)))
          ;; Has shebang with CLIME_MAIN_APP
          (should (string-match-p "CLIME_MAIN_APP=myapp" content))
          ;; Has guarded entry point
          (should (string-match-p "clime-main-script-p 'myapp" content))
          (should (string-match-p "clime-run-batch myapp" content)))))))

(ert-deftest clime-test-integration/dist-with-shebang ()
  "clime dist adds a shebang and makes the file executable by default."
  (clime-test-with-temp-dir
    (let ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-app
                     (list "dist" "-o" out
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-prefix-p "#!/bin/sh" (buffer-string))))
      ;; File should be executable
      (should (file-executable-p out)))))

(ert-deftest clime-test-integration/dist-provide-defaults-to-filename ()
  "clime dist defaults --provide to the output filename sans extension."
  (clime-test-with-temp-dir
    (let ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
          (out (expand-file-name "mything.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-app
                     (list "dist" "-o" out "--no-shebang"
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "(provide 'mything)" (buffer-string)))))))

(ert-deftest clime-test-integration/dist-description ()
  "clime dist --description sets the file header line."
  (clime-test-with-temp-dir
    (let ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-app
                     (list "dist" "-o" out "--no-shebang"
                           "--description" "My awesome tool"
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "My awesome tool" (buffer-string)))))))

(ert-deftest clime-test-integration/dist-rejects-missing-source ()
  "clime dist errors when a source file doesn't exist."
  (let* ((clime-app (expand-file-name "clime-app.el" clime-test--project-root))
         (result (clime-test--run-script
                  clime-app
                  (list "dist" "-o" "/tmp/out.el"
                        "/tmp/nonexistent.el"))))
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
