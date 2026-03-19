;;; clime-collision-tests.el --- Tests for ancestor collision checks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for definition-time ancestor flag collision detection.
;; Collisions are caught at construction time in clime-make-app.

;;; Code:

(require 'ert)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-test-helpers)

;;; ─── No Collision (Clean Trees) ───────────────────────────────────────

(ert-deftest clime-test-collision/clean-tree ()
  "No error when there are no flag collisions."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
         (cmd-opt (clime-make-option :name 'json :flags '("--json")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt))))
    ;; Should not signal
    (clime-make-app :name "t" :version "1"
                    :options (list root-opt)
                    :children (list (cons "show" cmd)))))

(ert-deftest clime-test-collision/siblings-allowed ()
  "Sibling commands may share the same flag names."
  (let* ((opt-a (clime-make-option :name 'to :flags '("--to")))
         (opt-b (clime-make-option :name 'to :flags '("--to")))
         (cmd-a (clime-make-command :name "send" :handler #'ignore
                                    :options (list opt-a)))
         (cmd-b (clime-make-command :name "move" :handler #'ignore
                                    :options (list opt-b))))
    ;; Should not signal — siblings don't collide
    (clime-make-app :name "t" :version "1"
                    :children (list (cons "send" cmd-a)
                                    (cons "move" cmd-b)))))

;;; ─── Collision Detected ───────────────────────────────────────────────

(ert-deftest clime-test-collision/child-vs-root ()
  "Error when child command option collides with root option."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
         (cmd-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt))))
    (should-error (clime-make-app :name "t" :version "1"
                                  :options (list root-opt)
                                  :children (list (cons "show" cmd)))
                  :type 'error)))

(ert-deftest clime-test-collision/child-vs-root-short-flag ()
  "Error when child short flag collides with root short flag."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose" "-v")))
         (cmd-opt (clime-make-option :name 'very :flags '("-v")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt))))
    (should-error (clime-make-app :name "t" :version "1"
                                  :options (list root-opt)
                                  :children (list (cons "show" cmd)))
                  :type 'error)))

(ert-deftest clime-test-collision/grandchild-vs-root ()
  "Error when grandchild option collides with root option."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd (clime-make-command :name "add" :handler #'ignore
                                  :options (list cmd-opt)))
         (grp (clime-make-group :name "dep"
                                :children (list (cons "add" cmd)))))
    (should-error (clime-make-app :name "t" :version "1"
                                  :options (list root-opt)
                                  :children (list (cons "dep" grp)))
                  :type 'error)))

(ert-deftest clime-test-collision/child-vs-group ()
  "Error when command option collides with parent group option."
  (let* ((grp-opt (clime-make-option :name 'json :flags '("--json")))
         (cmd-opt (clime-make-option :name 'json :flags '("--json")))
         (cmd (clime-make-command :name "add" :handler #'ignore
                                  :options (list cmd-opt)))
         (grp (clime-make-group :name "dep"
                                :options (list grp-opt)
                                :children (list (cons "add" cmd)))))
    (should-error (clime-make-app :name "t" :version "1"
                                  :children (list (cons "dep" grp)))
                  :type 'error)))

;;; ─── Error Message Content ────────────────────────────────────────────

(ert-deftest clime-test-collision/error-message-content ()
  "Error message includes the colliding flag and node names."
  (let* ((root-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd-opt (clime-make-option :name 'verbose :flags '("--verbose")))
         (cmd (clime-make-command :name "show" :handler #'ignore
                                  :options (list cmd-opt))))
    (condition-case err
        (progn (clime-make-app :name "t" :version "1"
                               :options (list root-opt)
                               :children (list (cons "show" cmd)))
               (ert-fail "Expected error"))
      (error
       (let ((msg (error-message-string err)))
         (should (string-match-p "--verbose" msg))
         (should (string-match-p "show" msg)))))))

(provide 'clime-collision-tests)
;;; clime-collision-tests.el ends here
