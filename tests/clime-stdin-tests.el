;;; clime-stdin-tests.el --- Tests for stdin sentinel support  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the `-' stdin sentinel: reading values from a buffered
;; stdin file, caching, type coercion, and error handling.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)

;;; ─── Helper ────────────────────────────────────────────────────────────

(defmacro clime-test-with-stdin (content &rest body)
  "Execute BODY with stdin simulated via a temp file containing CONTENT.
Sets CLIME_STDIN_FILE env var and resets `clime--stdin-content'."
  (declare (indent 1))
  (let ((file (gensym "stdin-file"))
        (old-env (gensym "old-env")))
    `(let ((,file (make-temp-file "clime-stdin-test"))
           (,old-env (getenv "CLIME_STDIN_FILE"))
           (clime--stdin-content nil))
       (unwind-protect
           (progn
             (with-temp-file ,file
               (insert ,content))
             (setenv "CLIME_STDIN_FILE" ,file)
             ,@body)
         (setenv "CLIME_STDIN_FILE" ,old-env)
         (when (file-exists-p ,file)
           (delete-file ,file))))))

;;; ─── Option value from stdin ───────────────────────────────────────────

(ert-deftest clime-test-stdin/option-value ()
  "Option value `-' reads from stdin."
  (clime-test-with-stdin "hello from stdin"
    (let* ((opt (clime-make-option :name 'desc :flags '("--desc")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "--desc" "-"))))
      (should (equal (plist-get (clime-parse-result-params result) 'desc)
                     "hello from stdin")))))

(ert-deftest clime-test-stdin/option-equals-syntax ()
  "Option value `--name=-' reads from stdin."
  (clime-test-with-stdin "equals stdin"
    (let* ((opt (clime-make-option :name 'desc :flags '("--desc")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "--desc=-"))))
      (should (equal (plist-get (clime-parse-result-params result) 'desc)
                     "equals stdin")))))

;;; ─── Positional arg from stdin ─────────────────────────────────────────

(ert-deftest clime-test-stdin/positional-arg ()
  "Positional arg `-' reads from stdin."
  (clime-test-with-stdin "positional stdin"
    (let* ((arg (clime-make-arg :name 'content))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :args (list arg)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "-"))))
      (should (equal (plist-get (clime-parse-result-params result) 'content)
                     "positional stdin")))))

;;; ─── Caching ───────────────────────────────────────────────────────────

(ert-deftest clime-test-stdin/multiple-sentinels-same-content ()
  "Multiple `-' values all get the same cached content."
  (clime-test-with-stdin "shared content"
    (let* ((opt1 (clime-make-option :name 'a :flags '("--a")))
           (opt2 (clime-make-option :name 'b :flags '("--b")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt1 opt2)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "--a" "-" "--b" "-"))))
      (should (equal (plist-get (clime-parse-result-params result) 'a)
                     "shared content"))
      (should (equal (plist-get (clime-parse-result-params result) 'b)
                     "shared content")))))

;;; ─── After -- ──────────────────────────────────────────────────────────

(ert-deftest clime-test-stdin/after-double-dash ()
  "`-' still reads stdin after `--' (value sentinel, not option)."
  (clime-test-with-stdin "after dash dash"
    (let* ((arg (clime-make-arg :name 'content))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :args (list arg)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "--" "-"))))
      (should (equal (plist-get (clime-parse-result-params result) 'content)
                     "after dash dash")))))

;;; ─── Rest args ─────────────────────────────────────────────────────────

(ert-deftest clime-test-stdin/rest-args ()
  "`-' token in rest args is replaced with stdin content."
  (clime-test-with-stdin "rest stdin"
    (let* ((arg1 (clime-make-arg :name 'script))
           (arg2 (clime-make-arg :name 'rest-args :nargs :rest :required nil))
           (cmd (clime-make-command :name "run" :handler #'ignore
                                    :args (list arg1 arg2)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "run" cmd))))
           (result (clime-parse app '("run" "myscript" "arg1" "-" "arg3"))))
      (should (equal (plist-get (clime-parse-result-params result) 'rest-args)
                     '("arg1" "rest stdin" "arg3"))))))

;;; ─── Type coercion ─────────────────────────────────────────────────────

(ert-deftest clime-test-stdin/type-coercion ()
  "Stdin value is coerced through the option's type."
  (clime-test-with-stdin "42"
    (let* ((opt (clime-make-option :name 'count :flags '("--count")
                                   :type 'integer))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd))))
           (result (clime-parse app '("cmd" "--count" "-"))))
      (should (equal (plist-get (clime-parse-result-params result) 'count)
                     42)))))

;;; ─── Error cases ───────────────────────────────────────────────────────

(ert-deftest clime-test-stdin/empty-stdin-error ()
  "Empty stdin signals usage error."
  (clime-test-with-stdin "   "
    (let* ((opt (clime-make-option :name 'desc :flags '("--desc")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd)))))
      (should-error (clime-parse app '("cmd" "--desc" "-"))
                    :type 'clime-usage-error))))

(ert-deftest clime-test-stdin/no-stdin-file-error ()
  "Missing stdin file signals usage error."
  (let ((clime--stdin-content nil))
    (setenv "CLIME_STDIN_FILE" nil)
    (let* ((opt (clime-make-option :name 'desc :flags '("--desc")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd)))))
      (should-error (clime-parse app '("cmd" "--desc" "-"))
                    :type 'clime-usage-error))))

;;; ─── App-specific env var ──────────────────────────────────────────────

(ert-deftest clime-test-stdin/app-prefix-env-var ()
  "Stdin reads from {ENV_PREFIX}_STDIN_FILE when available."
  (let* ((file (make-temp-file "clime-stdin-test"))
         (old-app-env (getenv "MYAPP_STDIN_FILE"))
         (old-clime-env (getenv "CLIME_STDIN_FILE"))
         (clime--stdin-content nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "app-specific stdin"))
          (setenv "MYAPP_STDIN_FILE" file)
          (setenv "CLIME_STDIN_FILE" nil)
          (let* ((opt (clime-make-option :name 'desc :flags '("--desc")))
                 (cmd (clime-make-command :name "cmd" :handler #'ignore
                                          :options (list opt)))
                 (app (clime-make-app :name "t" :version "1"
                                      :env-prefix "MYAPP"
                                      :children (list (cons "cmd" cmd))))
                 (result (clime-parse app '("cmd" "--desc" "-"))))
            (should (equal (plist-get (clime-parse-result-params result) 'desc)
                           "app-specific stdin"))))
      (setenv "MYAPP_STDIN_FILE" old-app-env)
      (setenv "CLIME_STDIN_FILE" old-clime-env)
      (when (file-exists-p file)
        (delete-file file)))))

;;; ─── Reset per invocation ──────────────────────────────────────────────

(ert-deftest clime-test-stdin/reset-per-run ()
  "`clime--stdin-content' is reset at start of `clime-run'."
  (clime-test-with-stdin "first call"
    (let* ((opt (clime-make-option :name 'desc :flags '("--desc")))
           (cmd (clime-make-command :name "cmd" :handler #'ignore
                                    :options (list opt)))
           (app (clime-make-app :name "t" :version "1"
                                :children (list (cons "cmd" cmd)))))
      ;; Pre-populate cache
      (setq clime--stdin-content "stale")
      ;; clime-run should reset it
      (with-output-to-string
        (clime-run app '("cmd" "--desc" "-")))
      ;; After run, the value should have been read fresh
      (should (equal clime--stdin-content "first call")))))

(provide 'clime-stdin-tests)
;;; clime-stdin-tests.el ends here
