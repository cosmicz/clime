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
  (let* ((dist (expand-file-name "dist/clime.el" clime-test--project-root)))
    (skip-unless (file-exists-p dist))
    (let ((result (clime-test--run-script dist '("--help"))))
      (should (= 0 (car result)))
      (should (string-match-p "clime\\.el COMMAND" (cdr result)))
      (should (string-match-p "init" (cdr result)))
      (should (string-match-p "bundle" (cdr result))))))

;;; ─── Dist as Library ────────────────────────────────────────────────────

(ert-deftest clime-test-integration/dist-as-library ()
  "The dist file works as a library without triggering the CLI entry point."
  (skip-unless (file-exists-p (expand-file-name "dist/clime.el" clime-test--project-root)))
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
         (should (string-match-p "# clime-sh!:v[0-9]+" line2)))))))

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

(ert-deftest clime-test-integration/init-skips-same-version-shebang ()
  "clime-make.el init is idempotent: re-running on same version preserves content."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-source app-file)
     ;; First init
     (clime-test--run-script clime-make (list "init" app-file))
     (let ((after-first (with-temp-buffer
                          (insert-file-contents app-file)
                          (buffer-string))))
       ;; Second init
       (clime-test--run-script clime-make (list "init" app-file))
       (let ((after-second (with-temp-buffer
                             (insert-file-contents app-file)
                             (buffer-string))))
         (should (equal after-first after-second)))))))

(ert-deftest clime-test-integration/init-rejects-newer-shebang-version ()
  "clime-make.el init refuses to downgrade a newer shebang version."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-source app-file)
     ;; Manually prepend a future-version shebang
     (let ((original (with-temp-buffer
                       (insert-file-contents app-file)
                       (buffer-string))))
       (with-temp-file app-file
         (insert "#!/bin/sh\n\":\"; exec emacs --batch -Q -- \"$@\" # clime-sh!:v999 -*- lexical-binding: t; -*-\n")
         (insert original)))
     ;; Init should refuse (exit non-zero)
     (let ((result (clime-test--run-script clime-make (list "init" app-file))))
       (should (/= 0 (car result)))
       (should (string-match-p "newer" (cdr result))))
     ;; --force should override
     (let ((result (clime-test--run-script clime-make (list "init" "--force" app-file))))
       (should (= 0 (car result)))))))

(ert-deftest clime-test-integration/init-updates-legacy-shebang ()
  "clime-make.el init detects and replaces a legacy clime:X.Y.Z shebang."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-source app-file)
     ;; Manually prepend a legacy-format shebang
     (let ((original (with-temp-buffer
                       (insert-file-contents app-file)
                       (buffer-string))))
       (with-temp-file app-file
         (insert "#!/bin/sh\n\":\"; exec emacs --batch -Q -- \"$@\" # clime:0.1.1 -*- mode: emacs-lisp; lexical-binding: t; -*-\n")
         (insert original)))
     ;; Init should detect the old tag and update it
     (let ((result (clime-test--run-script clime-make (list "init" app-file))))
       (should (= 0 (car result)))
       (should (string-match-p "updated" (cdr result))))
     ;; New shebang should have the new tag format
     (with-temp-buffer
       (insert-file-contents app-file)
       (forward-line 1)
       (let ((line2 (buffer-substring (point) (line-end-position))))
         (should (string-match-p "# clime-sh!:v[0-9]+" line2)))))))

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
         (should (string-match-p "# clime-sh!:v[0-9]+" line2)))))))

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

(ert-deftest clime-test-integration/init-lexical-binding ()
  "Scripts generated by clime init run with lexical-binding enabled."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-closure.el")))
     ;; Write an app that uses a closure to prove lexical binding
     (with-temp-file app-file
       (insert ";;; test-closure.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(clime-app test-closure :version \"1.0.0\" :help \"Test closures.\"\n"
               "  (clime-command check :help \"Check lexical binding\"\n"
               "    (clime-handler (ctx)\n"
               "      (let ((counter 0))\n"
               "        (let ((inc (lambda () (setq counter (1+ counter)))))\n"
               "          (funcall inc)\n"
               "          (funcall inc)\n"
               "          (format \"%s:%s\" lexical-binding counter))))))\n"
               "(clime-run-batch test-closure)\n"
               ";;; test-closure.el ends here\n"))
     ;; Init it
     (clime-test--run-script clime-make (list "init" app-file))
     ;; Run: should output "t:2" (lexical-binding=t, closure counter=2)
     (let ((result (clime-test--run-script app-file '("check"))))
       (should (= 0 (car result)))
       (should (equal "t:2" (cdr result)))))))

;;; ─── Symlink Resolution ─────────────────────────────────────────────────

(defun clime-test--write-lib-module (file-path feature code)
  "Write an Elisp library to FILE-PATH with FEATURE and CODE."
  (with-temp-file file-path
    (insert (format ";;; %s --- test lib  -*- lexical-binding: t; -*-\n"
                    (file-name-nondirectory file-path))
            ";;; Code:\n"
            code "\n"
            (format "(provide '%s)\n" feature)
            (format ";;; %s ends here\n"
                    (file-name-nondirectory file-path)))))

(ert-deftest clime-test-integration/init-self-dir-resolves-symlinks ()
  "init --self-dir resolves symlinks so sibling files are found."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-dir (expand-file-name "app/"))
          (link-dir (expand-file-name "links/")))
     (make-directory app-dir t)
     (make-directory link-dir t)
     ;; Write a lib module in app/
     (clime-test--write-lib-module
      (expand-file-name "mylib.el" app-dir) 'mylib
      "(defun mylib-greet (name) (format \"Hello, %s!\" name))")
     ;; Write an app that requires mylib in app/
     (with-temp-file (expand-file-name "myapp.el" app-dir)
       (insert ";;; myapp.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(require 'mylib)\n"
               "(clime-app myapp :version \"1.0\" :help \"Test.\"\n"
               "  (clime-command greet :help \"Greet\"\n"
               "    (clime-arg name :help \"Name\")\n"
               "    (clime-handler (ctx)\n"
               "      (clime-let ctx (name) (mylib-greet name)))))\n"
               "(clime-run-batch myapp)\n"
               "(provide 'myapp)\n"
               ";;; myapp.el ends here\n"))
     ;; Init with --self-dir so it finds mylib.el
     (clime-test--run-script clime-make
                             (list "init" "--self-dir"
                                   (expand-file-name "myapp.el" app-dir)))
     ;; Symlink from a different directory
     (make-symbolic-link (expand-file-name "myapp.el" app-dir)
                         (expand-file-name "myapp.el" link-dir))
     ;; Run via symlink — should resolve to app/ and find mylib
     (let ((result (clime-test--run-script
                    (expand-file-name "myapp.el" link-dir)
                    '("greet" "world"))))
       (should (= 0 (car result)))
       (should (equal "Hello, world!" (cdr result)))))))

(ert-deftest clime-test-integration/init-rel-load-path-resolves-symlinks ()
  "init -R resolves symlinks so relative paths work through symlinks."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (project-dir (expand-file-name "project/"))
          (lib-dir (expand-file-name "project/lib/"))
          (bin-dir (expand-file-name "project/bin/"))
          (link-dir (expand-file-name "links/")))
     (make-directory lib-dir t)
     (make-directory bin-dir t)
     (make-directory link-dir t)
     ;; Write a lib module in project/lib/
     (clime-test--write-lib-module
      (expand-file-name "mylib.el" lib-dir) 'mylib
      "(defun mylib-greet (name) (format \"Hi, %s!\" name))")
     ;; Write an app in project/bin/ that requires mylib
     (with-temp-file (expand-file-name "myapp.el" bin-dir)
       (insert ";;; myapp.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(require 'mylib)\n"
               "(clime-app myapp :version \"1.0\" :help \"Test.\"\n"
               "  (clime-command greet :help \"Greet\"\n"
               "    (clime-arg name :help \"Name\")\n"
               "    (clime-handler (ctx)\n"
               "      (clime-let ctx (name) (mylib-greet name)))))\n"
               "(clime-run-batch myapp)\n"
               "(provide 'myapp)\n"
               ";;; myapp.el ends here\n"))
     ;; Init with -R ../lib so it finds mylib relative to script
     (clime-test--run-script clime-make
                             (list "init" "-R" "../lib"
                                   (expand-file-name "myapp.el" bin-dir)))
     ;; Symlink from a different directory
     (make-symbolic-link (expand-file-name "myapp.el" bin-dir)
                         (expand-file-name "myapp.el" link-dir))
     ;; Run via symlink — should resolve to bin/ and find ../lib/mylib
     (let ((result (clime-test--run-script
                    (expand-file-name "myapp.el" link-dir)
                    '("greet" "world"))))
       (should (= 0 (car result)))
       (should (equal "Hi, world!" (cdr result)))))))

;;; ─── Scaffold Command ──────────────────────────────────────────────
;;; scaffold inserts ;;; Entrypoint: boilerplate by detecting clime-app
;;; and (provide) in the target file.

(defun clime-test--write-app-with-provide (file-path)
  "Write a clime app to FILE-PATH with (provide) and ends-here but no entrypoint.
The app has a `clime-app' form and `provide' but no `;;; Entrypoint:' section."
  (with-temp-file file-path
    (insert ";;; test-app.el --- test  -*- lexical-binding: t; -*-\n"
            ";;; Code:\n"
            "(require 'clime)\n"
            "(clime-app test-app :version \"1.0.0\" :help \"Test app.\"\n"
            "  (clime-command hello :help \"Say hello\"\n"
            "    (clime-arg name :help \"Name\")\n"
            "    (clime-handler (ctx)\n"
            "      (clime-let ctx (name) (format \"Hello, %s!\" name)))))\n"
            "(provide 'test-app)\n"
            ";;; test-app.el ends here\n")))

(ert-deftest clime-test-integration/scaffold-inserts-entrypoint ()
  "scaffold adds ;;; Entrypoint: section with guard and run-batch."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-with-provide app-file)
     (let ((result (clime-test--run-script clime-make
                                           (list "scaffold" app-file))))
       (should (= 0 (car result))))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((content (buffer-string)))
         ;; Has entrypoint marker
         (should (string-match-p "^;;; Entrypoint:" content))
         ;; Has guard using provide feature
         (should (string-match-p "clime-main-script-p 'test-app" content))
         ;; Has clime-run-batch call
         (should (string-match-p "clime-run-batch test-app" content))
         ;; Ends-here marker is still last
         (should (string-match-p ";;; test-app\\.el ends here\n\\'" content)))))))

(ert-deftest clime-test-integration/scaffold-uses-provide-over-app-symbol ()
  "scaffold uses (provide 'FEATURE) for the guard, not the clime-app symbol."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "mycli.el")))
     ;; App symbol is 'mycli-app' but provide is 'mycli'
     (with-temp-file app-file
       (insert ";;; mycli.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(clime-app mycli-app :version \"1.0.0\" :help \"Test.\"\n"
               "  (clime-command hello :help \"Hi\"\n"
               "    (clime-handler (ctx) \"hello\")))\n"
               "(provide 'mycli)\n"
               ";;; mycli.el ends here\n"))
     (clime-test--run-script clime-make (list "scaffold" app-file))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((content (buffer-string)))
         ;; Guard uses provide feature, not app symbol
         (should (string-match-p "clime-main-script-p 'mycli\\b" content))
         ;; But run-batch uses the app symbol
         (should (string-match-p "clime-run-batch mycli-app" content)))))))

(ert-deftest clime-test-integration/scaffold-falls-back-to-app-symbol ()
  "scaffold falls back to clime-app symbol when no (provide) form exists."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     ;; App with no (provide) form
     (with-temp-file app-file
       (insert ";;; test-app.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(clime-app test-app :version \"1.0.0\" :help \"Test.\"\n"
               "  (clime-command hello :help \"Hi\"\n"
               "    (clime-handler (ctx) \"hello\")))\n"
               ";;; test-app.el ends here\n"))
     (clime-test--run-script clime-make (list "scaffold" app-file))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((content (buffer-string)))
         ;; Falls back to app symbol for both
         (should (string-match-p "clime-main-script-p 'test-app" content))
         (should (string-match-p "clime-run-batch test-app" content)))))))

(ert-deftest clime-test-integration/scaffold-skips-when-present ()
  "scaffold skips when ;;; Entrypoint: already exists."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     ;; Write app WITH an existing entrypoint
     (with-temp-file app-file
       (insert ";;; test-app.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(clime-app test-app :version \"1.0.0\" :help \"Test app.\"\n"
               "  (clime-command hello :help \"Say hello\"\n"
               "    (clime-arg name :help \"Name\")\n"
               "    (clime-handler (ctx)\n"
               "      (clime-let ctx (name) (format \"Hello, %s!\" name)))))\n"
               "(provide 'test-app)\n"
               ";;; Entrypoint:\n"
               "(when (clime-main-script-p 'test-app)\n"
               "  (clime-run-batch test-app))\n"
               ";;; test-app.el ends here\n"))
     (let ((result (clime-test--run-script clime-make
                                           (list "scaffold" app-file))))
       (should (= 0 (car result))))
     ;; Verify no duplicate entrypoint marker
     (with-temp-buffer
       (insert-file-contents app-file)
       (should (= 1 (how-many ";;; Entrypoint:" (point-min) (point-max))))))))

(ert-deftest clime-test-integration/scaffold-errors-without-clime-app ()
  "scaffold errors when no clime-app form is found."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "plain.el")))
     (with-temp-file app-file
       (insert ";;; plain.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(message \"hello\")\n"
               "(provide 'plain)\n"
               ";;; plain.el ends here\n"))
     (let ((result (clime-test--run-script clime-make
                                           (list "scaffold" app-file))))
       (should (/= 0 (car result)))
       (should (string-match-p "no.*clime-app" (cdr result)))))))

(ert-deftest clime-test-integration/scaffold-appends-without-ends-here ()
  "scaffold appends entrypoint at end when no ;;; ... ends here marker."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     ;; App without ends-here marker
     (with-temp-file app-file
       (insert ";;; test-app.el --- test  -*- lexical-binding: t; -*-\n"
               ";;; Code:\n"
               "(require 'clime)\n"
               "(clime-app test-app :version \"1.0.0\" :help \"Test.\"\n"
               "  (clime-command hello :help \"Hi\"\n"
               "    (clime-handler (ctx) \"hello\")))\n"
               "(provide 'test-app)\n"))
     (clime-test--run-script clime-make (list "scaffold" app-file))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((content (buffer-string)))
         (should (string-match-p ";;; Entrypoint:" content))
         (should (string-match-p "clime-run-batch test-app" content)))))))

;;; ─── Quickstart Command (scaffold + init) ──────────────────────────
;;; quickstart is the impure composite: detects clime-app, runs scaffold,
;;; then runs init with auto CLIME_MAIN_APP.

(ert-deftest clime-test-integration/quickstart-scaffolds-and-inits ()
  "quickstart composes scaffold + init: adds entrypoint and shebang."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-with-provide app-file)
     (let ((result (clime-test--run-script clime-make
                                           (list "quickstart" app-file))))
       (should (= 0 (car result))))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((content (buffer-string)))
         ;; Has shebang (from init)
         (should (string-prefix-p "#!/bin/sh" content))
         ;; Has entrypoint (from scaffold)
         (should (string-match-p ";;; Entrypoint:" content))
         (should (string-match-p "clime-main-script-p 'test-app" content))
         (should (string-match-p "clime-run-batch test-app" content))
         ;; Has auto CLIME_MAIN_APP in shebang (quickstart auto-detects)
         (should (string-match-p "CLIME_MAIN_APP=test-app" content)))))))

(ert-deftest clime-test-integration/quickstart-auto-clime-main-app ()
  "quickstart auto-adds CLIME_MAIN_APP env var to the shebang."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-with-provide app-file)
     (clime-test--run-script clime-make (list "quickstart" app-file))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((line2 (progn (forward-line 1)
                           (buffer-substring (point) (line-end-position)))))
         (should (string-match-p "CLIME_MAIN_APP=test-app" line2)))))))

(ert-deftest clime-test-integration/quickstart-explicit-env-overrides-auto ()
  "Explicit -e CLIME_MAIN_APP=custom wins over quickstart's auto-detection."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-with-provide app-file)
     (clime-test--run-script clime-make
                             (list "quickstart" "-e" "CLIME_MAIN_APP=custom" app-file))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((line2 (progn (forward-line 1)
                           (buffer-substring (point) (line-end-position)))))
         (should (string-match-p "CLIME_MAIN_APP=custom" line2))
         (should-not (string-match-p "CLIME_MAIN_APP=test-app" line2)))))))

(ert-deftest clime-test-integration/quickstart-forwards-init-flags ()
  "quickstart forwards flags like --self-dir and -e to init."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-with-provide app-file)
     (let ((result (clime-test--run-script
                    clime-make
                    (list "quickstart" "--self-dir" "-e" "MY_VAR=hello" app-file))))
       (should (= 0 (car result))))
     (with-temp-buffer
       (insert-file-contents app-file)
       (let ((content (buffer-string)))
         ;; Has --self-dir flag effect (resolved dir load path)
         (should (string-match-p "-L \"\\$D\"" content))
         ;; Has custom env var
         (should (string-match-p "MY_VAR=hello" content))
         ;; Has entrypoint
         (should (string-match-p ";;; Entrypoint:" content)))))))

(ert-deftest clime-test-integration/quickstart-works-end-to-end ()
  "quickstart produces a file that runs correctly as a script."
  (clime-test-with-temp-dir
   (let* ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
          (app-file (expand-file-name "test-app.el")))
     (clime-test--write-app-with-provide app-file)
     (let ((result (clime-test--run-script clime-make
                                           (list "quickstart" app-file))))
       (should (= 0 (car result))))
     ;; Run the app
     (let ((result (clime-test--run-script app-file '("hello" "world"))))
       (should (= 0 (car result)))
       (should (equal "Hello, world!" (cdr result)))))))

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

(ert-deftest clime-test-integration/bundle-rejects-run-batch-in-library ()
  "clime bundle errors when source has (clime-run-batch ...) in library section.
Users must move it below ;;; Entrypoint: marker."
  (clime-test-with-temp-dir
   (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (out (expand-file-name "bundle.el")))
     (clime-test--write-module "app.el" 'myapp
                               "(defvar myapp nil)\n(clime-run-batch myapp)")
     (let ((result (clime-test--run-script
                    clime-make
                    (list "bundle" "-o" out
                          (expand-file-name "app.el")))))
       (should (= 2 (car result)))
       (should (string-match-p "clime-run-batch.*Entrypoint" (cdr result)))))))

(ert-deftest clime-test-integration/bundle-main-guard ()
  "clime bundle --main FILE adds guarded entry point from file."
  (clime-test-with-temp-dir
   (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (out (expand-file-name "bundle.el")))
     (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
     ;; Main file — plain script, no ;;; Entrypoint: marker
     (clime-test--write-module "app-main.el" 'myapp-main
                               "(require 'myapp)\n(clime-run-batch myapp)")
     (let ((result (clime-test--run-script
                    clime-make
                    (list "bundle" "-o" out
                          "--main" (expand-file-name "app-main.el")
                          (expand-file-name "app.el")))))
       (should (= 0 (car result))))
     (with-temp-buffer
       (insert-file-contents out)
       (let ((content (buffer-string)))
         ;; Has guarded entry point using provide feature
         (should (string-match-p "clime-main-script-p 'bundle" content))
         (should (string-match-p "clime-run-batch myapp" content))
         ;; Has ;;; Entrypoint: marker
         (should (string-match-p "^;;; Entrypoint:" content))
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

(ert-deftest clime-test-integration/bundle-rejects-missing-main ()
  "clime bundle --main errors when the main file doesn't exist."
  (clime-test-with-temp-dir
   (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (out (expand-file-name "bundle.el")))
     (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
     (let ((result (clime-test--run-script
                    clime-make
                    (list "bundle" "-o" out
                          "--main" (expand-file-name "nonexistent.el")
                          (expand-file-name "app.el")))))
       (should (= 2 (car result)))
       (should (string-match-p "does not exist" (cdr result)))))))

(ert-deftest clime-test-integration/bundle-rejects-main-with-entry-marker ()
  "clime bundle --main rejects files that contain ;;; Entrypoint: marker."
  (clime-test-with-temp-dir
   (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (out (expand-file-name "bundle.el")))
     (clime-test--write-module "app.el" 'myapp "(defvar myapp nil)")
     (clime-test--write-module-with-entry "bad-main.el" 'myapp
                                          "(require 'myapp)"
                                          "(clime-run-batch myapp)")
     (let ((result (clime-test--run-script
                    clime-make
                    (list "bundle" "-o" out
                          "--main" (expand-file-name "bad-main.el")
                          (expand-file-name "app.el")))))
       (should (= 2 (car result)))
       (should (string-match-p "Entrypoint" (cdr result)))))))

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

;;; ─── Entrypoint Section Markers ─────────────────────────────────────────

(defun clime-test--write-module-with-entry (file-path feature lib-code entry-code)
  "Write an Elisp module with an ;;; Entrypoint: section to FILE-PATH.
FEATURE is the provide symbol, LIB-CODE is the library body,
ENTRY-CODE is the entrypoint body after the Entrypoint marker."
  (with-temp-file file-path
    (insert (format ";;; %s --- test module  -*- lexical-binding: t; -*-\n"
                    (file-name-nondirectory file-path))
            ";;; Code:\n"
            lib-code "\n"
            (format "(provide '%s)\n" feature)
            ";;; Entrypoint:\n"
            entry-code "\n"
            (format ";;; %s ends here\n"
                    (file-name-nondirectory file-path)))))

(ert-deftest clime-test-integration/bundle-strips-entry-section ()
  "clime bundle strips ;;; Entrypoint: section from source files."
  (clime-test-with-temp-dir
   (let ((clime-make (expand-file-name "clime-make.el" clime-test--project-root))
         (out (expand-file-name "bundle.el")))
     (clime-test--write-module-with-entry "app.el" 'myapp
                                          "(defvar myapp nil)"
                                          "(clime-run-batch myapp)")
     (let ((result (clime-test--run-script
                    clime-make
                    (list "bundle" "-o" out
                          (expand-file-name "app.el")))))
       (should (= 0 (car result))))
     (with-temp-buffer
       (insert-file-contents out)
       (let ((content (buffer-string)))
         ;; Library code present
         (should (string-match-p "defvar myapp" content))
         ;; Entrypoint code stripped
         (should-not (string-match-p "clime-run-batch" content)))))))

;;; ─── Example App ────────────────────────────────────────────────────────

(ert-deftest clime-test-integration/pkm-example ()
  "The pkm example app runs end-to-end."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("install" "foo" "--tag" "dev"))))
    (should (= 0 (car result)))
    (should (string-match-p "Installing foo" (cdr result)))
    (should (string-match-p "tags=dev" (cdr result)))))

(ert-deftest clime-test-integration/pkm-invalid-sort-choice ()
  "pkm list --sort with invalid choice reports a usage error."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("list" "--sort" "invalid"))))
    (should (= 2 (car result)))
    (should (string-match-p "Invalid value" (cdr result)))))

(ert-deftest clime-test-integration/pkm-mutex-table-csv ()
  "pkm list --table --csv reports mutual exclusion error."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("list" "--table" "--csv"))))
    (should (= 2 (car result)))
    (should (string-match-p "mutually exclusive\\|cannot be used together\\|only one"
                            (cdr result)))))

(ert-deftest clime-test-integration/pkm-missing-required-arg ()
  "pkm install without package name reports a usage error."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("install"))))
    (should (= 2 (car result)))
    (should (string-match-p "required\\|missing" (cdr result)))))

(ert-deftest clime-test-integration/pkm-valid-sort-choice ()
  "pkm list --sort date succeeds."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("list" "--sort" "date"))))
    (should (= 0 (car result)))
    (should (string-match-p "sort=date" (cdr result)))))

(ert-deftest clime-test-integration/pkm-negatable-color ()
  "pkm list --no-color reports color=off."
  (let* ((pkm (expand-file-name "examples/pkm.el" clime-test--project-root))
         (result (clime-test--run-script pkm '("list" "--no-color"))))
    (should (= 0 (car result)))
    (should (string-match-p "color=nil" (cdr result)))))

(provide 'clime-integration-tests)
;;; clime-integration-tests.el ends here
