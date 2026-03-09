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
    (should (string-match-p "clime\\.el COMMAND" (cdr result)))
    (should (string-match-p "init" (cdr result)))
    (should (string-match-p "bundle" (cdr result)))))

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
  "clime-make.el init adds a working shebang to an .el file."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; Init it
      (let ((result (clime-test--run-script clime-make (list "init" app-file))))
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
  "clime-make.el init --standalone skips the automatic clime load path."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-make (list "init" "--standalone" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        ;; Should NOT contain any -L path (case-sensitive: -L not -l)
        (let ((case-fold-search nil))
          (should-not (string-match-p " -L " (buffer-string))))))))

(ert-deftest clime-test-integration/init-env ()
  "clime-make.el init --env injects environment variables into shebang."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-make
                              (list "init" "--env" "FOO=bar"
                                    "--env" "BAZ=qux" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        (let ((line2 (progn (forward-line 1)
                            (buffer-substring (point) (line-end-position)))))
          (should (string-match-p "FOO=bar" line2))
          (should (string-match-p "BAZ=qux" line2)))))))

(ert-deftest clime-test-integration/init-includes-version-tag ()
  "clime-make.el init embeds a clime:VERSION tag in the shebang."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-make (list "init" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        (let ((line2 (progn (forward-line 1)
                            (buffer-substring (point) (line-end-position)))))
          (should (string-match-p "# clime:[0-9]+\\.[0-9]+\\.[0-9]+" line2)))))))

(ert-deftest clime-test-integration/init-updates-clime-shebang ()
  "clime-make.el init replaces an existing clime-tagged shebang."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; First init
      (clime-test--run-script clime-make (list "init" app-file))
      ;; Second init should succeed (update)
      (let ((result (clime-test--run-script clime-make (list "init" app-file))))
        (should (= 0 (car result)))
        (should (string-match-p "updated" (cdr result))))
      ;; File should still work
      (let ((result (clime-test--run-script app-file '("hello" "world"))))
        (should (= 0 (car result)))
        (should (equal "Hello, world!" (cdr result)))))))

(ert-deftest clime-test-integration/init-update-changes-options ()
  "clime-make.el init update replaces shebang with new options."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; First init without env
      (clime-test--run-script clime-make (list "init" app-file))
      (with-temp-buffer
        (insert-file-contents app-file)
        (should-not (string-match-p "MY_VAR=hello" (buffer-string))))
      ;; Update with --env
      (let ((result (clime-test--run-script
                     clime-make (list "init" "--env" "MY_VAR=hello" app-file))))
        (should (= 0 (car result)))
        (should (string-match-p "updated" (cdr result))))
      ;; Verify env var is now in shebang
      (with-temp-buffer
        (insert-file-contents app-file)
        (should (string-match-p "MY_VAR=hello" (buffer-string)))))))

(ert-deftest clime-test-integration/init-rejects-non-clime-shebang ()
  "clime-make.el init errors on a file with a non-clime shebang."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; Manually prepend a non-clime shebang
      (let ((original (with-temp-buffer
                        (insert-file-contents app-file)
                        (buffer-string))))
        (with-temp-file app-file
          (insert "#!/usr/bin/env python\n# not clime\n")
          (insert original)))
      ;; Init should fail
      (let ((result (clime-test--run-script clime-make (list "init" app-file))))
        (should (= 2 (car result)))
        (should (string-match-p "already has a shebang" (cdr result)))))))

(ert-deftest clime-test-integration/init-force-replaces-non-clime-shebang ()
  "clime-make.el init --force replaces a non-clime shebang."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      ;; Manually add a non-clime shebang
      (let ((original (with-temp-buffer
                        (insert-file-contents app-file)
                        (buffer-string))))
        (with-temp-file app-file
          (insert "#!/usr/bin/env python\n# some other header\n")
          (insert original)))
      ;; Init --force should succeed
      (let ((result (clime-test--run-script clime-make
                                            (list "init" "--force" app-file))))
        (should (= 0 (car result))))
      ;; Should have clime shebang now
      (with-temp-buffer
        (insert-file-contents app-file)
        (should (string-prefix-p "#!/bin/sh" (buffer-string)))
        (let ((line2 (progn (forward-line 1)
                            (buffer-substring (point) (line-end-position)))))
          (should (string-match-p "# clime:" line2)))))))

(ert-deftest clime-test-integration/init-rejects-missing-file ()
  "clime-make.el init errors on a nonexistent file."
  (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (result (clime-test--run-script
                  clime-make (list "init" "/tmp/does-not-exist.el"))))
    (should (= 2 (car result)))
    (should (string-match-p "does not exist" (cdr result)))))

(ert-deftest clime-test-integration/init-usage-shows-exe-name ()
  "Usage output shows the executable filename, not the DSL symbol."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           ;; Name the file differently from the DSL symbol (test-app)
           (app-file (expand-file-name "my-tool.el")))
      (clime-test--write-app-source app-file)
      (clime-test--run-script clime-make (list "init" app-file))
      ;; --help should show "my-tool.el" (executable name) not "test-app" (DSL symbol)
      (let ((result (clime-test--run-script app-file '("--help"))))
        (should (= 0 (car result)))
        (should (string-match-p "my-tool\\.el" (cdr result)))
        (should-not (string-match-p "test-app" (cdr result)))))))

(ert-deftest clime-test-integration/init-rejects-unsafe-env ()
  "clime-make.el init rejects env values with shell metacharacters."
  (clime-test-with-temp-dir
    (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
           (app-file (expand-file-name "test-app.el")))
      (clime-test--write-app-source app-file)
      (let ((result (clime-test--run-script
                     clime-make
                     (list "init" "--env" "FOO=$(rm -rf /)" app-file))))
        (should (= 2 (car result)))
        (should (string-match-p "invalid --env" (cdr result)))))))

;;; ─── Bundle Command ─────────────────────────────────────────────────────

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

(ert-deftest clime-test-integration/bundle-basic ()
  "clime bundle concatenates multiple source files into one."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      ;; Write two source modules
      (clime-test--write-module "core.el" 'mycore
                                "(defun mycore-greet () \"hi\")")
      (clime-test--write-module "main.el" 'mymain
                                "(require 'mycore)\n(defun mymain-run () (mycore-greet))")
      ;; Bundle them
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out "--provide" "mybundle"
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
          ;; Has bundle-level provide
          (should (string-match-p "(provide 'mybundle)" content))
          ;; Module provides are kept (required for require chains)
          (should (string-match-p "(provide 'mycore)" content))
          ;; No shebang (bundle never adds shebang)
          (should-not (string-prefix-p "#!" content)))))))

(ert-deftest clime-test-integration/bundle-strips-run-batch ()
  "clime bundle strips (clime-run-batch ...) from extracted code."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp
                                "(defvar myapp nil)\n(clime-run-batch myapp)")
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "defvar myapp" (buffer-string)))
        (should-not (string-match-p "clime-run-batch" (buffer-string)))))))

(ert-deftest clime-test-integration/bundle-main-guard ()
  "clime bundle --main adds guarded entry point."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out "--main" "myapp"
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (let ((content (buffer-string)))
          ;; Has guarded entry point
          (should (string-match-p "clime-main-script-p 'myapp" content))
          (should (string-match-p "clime-run-batch myapp" content))
          ;; No shebang (that's init's job)
          (should-not (string-prefix-p "#!" content)))))))

(ert-deftest clime-test-integration/bundle-provide-defaults-to-filename ()
  "clime bundle defaults --provide to the output filename sans extension."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "mything.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "(provide 'mything)" (buffer-string)))))))

(ert-deftest clime-test-integration/bundle-description ()
  "clime bundle --description sets the file header line."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out
                           "--description" "My awesome tool"
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "My awesome tool" (buffer-string)))))))

(ert-deftest clime-test-integration/bundle-rejects-unsafe-main ()
  "clime bundle --main rejects values that aren't valid symbol names."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out "--main" "foo bar"
                           (expand-file-name "app.el")))))
        (should (= 2 (car result)))
        (should (string-match-p "invalid --main" (cdr result)))))))

(ert-deftest clime-test-integration/bundle-rejects-missing-source ()
  "clime bundle errors when a source file doesn't exist."
  (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (result (clime-test--run-script
                  clime-make
                  (list "bundle" "-o" "/tmp/out.el"
                        "/tmp/nonexistent.el"))))
    (should (= 2 (car result)))
    (should (string-match-p "does not exist" (cdr result)))))

(ert-deftest clime-test-integration/bundle-rejects-missing-markers ()
  "clime bundle errors when a source file lacks ;;; Code: markers."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "bundle.el"))
          (bad-file (expand-file-name "bad.el")))
      (with-temp-file bad-file
        (insert "(defvar bad nil)\n"))
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out bad-file))))
        (should (= 2 (car result)))
        (should (string-match-p "missing.*Code:" (cdr result)))))))

(ert-deftest clime-test-integration/bundle-creates-output-dir ()
  "clime bundle creates the output directory if it doesn't exist."
  (clime-test-with-temp-dir
    (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (out (expand-file-name "sub/dir/bundle.el")))
      (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
      (let ((result (clime-test--run-script
                     clime-make
                     (list "bundle" "-o" out
                           (expand-file-name "app.el")))))
        (should (= 0 (car result))))
      (should (file-exists-p out)))))

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
