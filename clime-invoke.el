;;; clime-invoke.el --- Interactive transient UI for clime apps  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Auto-generates transient.el menus from `clime-app' definitions.
;; Groups become nested transient prefixes, options become infixes
;; (switches / value inputs), commands get an execute action that
;; collects args and calls `clime-run'.
;;
;; Requires the `transient' package (not bundled with clime).

;;; Code:

(require 'cl-lib)
(require 'clime-core)
(require 'clime-run)
(require 'transient)

;;; ─── Internal State ────────────────────────────────────────────────────

(defvar clime-invoke--cache (make-hash-table :test #'eq)
  "Cache of generated transient prefix symbols, keyed by app symbol.")

;;; ─── Pure Helpers ──────────────────────────────────────────────────────

(defun clime-invoke--preferred-key (option)
  "Return the preferred single-char key string for OPTION.
Prefers the short flag letter (e.g. \"-v\" → \"v\"), falls back to
the first letter of the long flag (e.g. \"--output\" → \"o\")."
  (let ((flags (clime-option-flags option)))
    (or (cl-loop for flag in flags
                 when (and (= (length flag) 2)
                           (string-prefix-p "-" flag)
                           (not (string-prefix-p "--" flag)))
                 return (substring flag 1 2))
        ;; Fallback: first letter after "--"
        (let ((long (cl-find-if (lambda (f) (string-prefix-p "--" f)) flags)))
          (when long
            (substring long 2 3))))))

(defun clime-invoke--assign-keys (options)
  "Assign unique transient keys to OPTIONS.
Return an alist of (OPTION-NAME . KEY-STRING).
Uses preferred keys when available, deduplicates collisions by
trying subsequent letters of the flag name, then fallback pool."
  (let ((used (make-hash-table :test #'equal))
        (result '()))
    (dolist (opt options)
      (let* ((preferred (clime-invoke--preferred-key opt))
             (name (clime-option-name opt))
             (long-flag (cl-find-if (lambda (f) (string-prefix-p "--" f))
                                    (clime-option-flags opt)))
             (long-name (and long-flag (substring long-flag 2)))
             (key nil))
        ;; Try preferred key
        (when (and preferred (not (gethash preferred used)))
          (setq key preferred))
        ;; Try letters from long flag name
        (unless key
          (when long-name
            (cl-loop for i from 0 below (length long-name)
                     for ch = (substring long-name i (1+ i))
                     when (and (string-match-p "[a-z]" ch)
                               (not (gethash ch used)))
                     return (setq key ch))))
        ;; Fallback: first unused letter
        (unless key
          (cl-loop for code from ?a to ?z
                   for ch = (char-to-string code)
                   when (not (gethash ch used))
                   return (setq key ch)))
        (when key
          (puthash key t used)
          (push (cons name key) result))))
    (nreverse result)))

(defun clime-invoke--assign-child-keys (children)
  "Assign unique transient keys to CHILDREN alist entries.
CHILDREN is an alist of (NAME-STRING . NODE).
Return an alist of (NAME-STRING . KEY-STRING)."
  (let ((used (make-hash-table :test #'equal))
        (result '()))
    (dolist (entry children)
      (let* ((name (car entry))
             (key nil))
        ;; Try first letter
        (let ((first (substring name 0 1)))
          (when (not (gethash first used))
            (setq key first)))
        ;; Try subsequent letters
        (unless key
          (cl-loop for i from 1 below (length name)
                   for ch = (substring name i (1+ i))
                   when (and (string-match-p "[a-z]" ch)
                             (not (gethash ch used)))
                   return (setq key ch)))
        ;; Fallback
        (unless key
          (cl-loop for code from ?a to ?z
                   for ch = (char-to-string code)
                   when (not (gethash ch used))
                   return (setq key ch)))
        (when key
          (puthash key t used)
          (push (cons name key) result))))
    (nreverse result)))

(defun clime-invoke--option-to-argv (option value)
  "Convert OPTION with VALUE to an argv fragment (list of strings).
Returns nil if VALUE is nil/empty."
  (cond
   ((null value) nil)
   ;; Boolean/count flag
   ((clime-option-boolean-p option)
    (if value
        (list (car (clime-option-flags option)))
      nil))
   ;; Multiple values
   ((and (clime-option-multiple option) (listp value))
    (let ((flag (car (clime-option-flags option))))
      (cl-mapcan (lambda (v) (list flag v)) value)))
   ;; Single value
   (t
    (let ((flag (car (clime-option-flags option))))
      (list flag (if (stringp value) value (format "%s" value)))))))

(defun clime-invoke--build-argv (path option-fragments positional-args)
  "Build a complete argv list.
PATH is the command path (list of strings).
OPTION-FRAGMENTS is a list of argv fragments from `clime-invoke--option-to-argv'.
POSITIONAL-ARGS is a list of positional arg value strings."
  (append path
          (apply #'append option-fragments)
          positional-args))

(defun clime-invoke--collect-options (node)
  "Collect all applicable options for NODE.
Includes NODE's own options plus options from all ancestors.
Excludes hidden options."
  (let ((opts '()))
    ;; Own options
    (dolist (opt (clime-node-options node))
      (unless (clime-option-hidden opt)
        (push opt opts)))
    ;; Ancestor options
    (dolist (ancestor (clime-node-ancestors node))
      (dolist (opt (clime-node-options ancestor))
        (unless (clime-option-hidden opt)
          (push opt opts))))
    (nreverse opts)))

(defun clime-invoke--visible-children (group)
  "Return visible children of GROUP as an alist.
Excludes hidden nodes.  Promotes inline group children."
  (let ((result '()))
    (dolist (entry (clime-group-children group))
      (let ((name (car entry))
            (child (cdr entry)))
        (cond
         ;; Inline group: promote its children
         ((and (clime-group-p child) (clime-node-inline child))
          (dolist (sub-entry (clime-invoke--visible-children child))
            (push sub-entry result)))
         ;; Hidden: skip
         ((clime-node-hidden child) nil)
         ;; Normal: include
         (t (push (cons name child) result)))))
    (nreverse result)))

(defun clime-invoke--group-by-category (options)
  "Group OPTIONS by their :category slot.
Return an alist of (CATEGORY . OPTIONS-LIST).
nil category is listed first."
  (let ((groups (make-hash-table :test #'equal))
        (order '()))
    (dolist (opt options)
      (let ((cat (clime-option-category opt)))
        (unless (gethash (or cat "") groups)
          (push cat order))
        (push opt (gethash (or cat "") groups))))
    (let ((result '()))
      (dolist (cat (nreverse order))
        (push (cons cat (nreverse (gethash (or cat "") groups)))
              result))
      (nreverse result))))

(defun clime-invoke--prefix-symbol (app-name path)
  "Generate a transient prefix symbol for APP-NAME and PATH.
PATH is a list of command name strings, or nil for root."
  (intern (concat "clime-invoke/"
                  app-name
                  (if path
                      (concat "/" (string-join path "/"))
                    ""))))

;;; ─── Spec Builders ─────────────────────────────────────────────────────

(defun clime-invoke--option-to-spec (option key _prefix-sym)
  "Build a transient suffix spec for OPTION with KEY under PREFIX-SYM.
Returns a shorthand spec list consumable by `transient-parse-suffixes'."
  (let* ((name (clime-option-name option))
         (flags (clime-option-flags option))
         (primary-flag (car flags))
         (help (or (clime-option-help option)
                   (symbol-name name))))
    (cond
     ;; Boolean/count flag → switch (argument without "=")
     ((clime-option-boolean-p option)
      `(,key ,help ,(concat primary-flag " ")))
     ;; Option with choices
     ((clime-option-choices option)
      (let ((choices (clime--resolve-value (clime-option-choices option))))
        `(,key ,help ,(concat primary-flag "=")
               :choices ,choices)))
     ;; Plain value option (argument with "=")
     (t
      `(,key ,help ,(concat primary-flag "="))))))

;;; ─── Arg Prompting ─────────────────────────────────────────────────────

(defun clime-invoke--prompt-args (args)
  "Prompt for positional ARGS via minibuffer.
Return a list of value strings.  Required args must be non-empty.
Optional args can be skipped with empty input."
  (let ((values '()))
    (dolist (arg args)
      (let* ((name (symbol-name (clime-arg-name arg)))
             (required (clime-arg-required arg))
             (choices (and (clime-arg-choices arg)
                           (clime--resolve-value (clime-arg-choices arg))))
             (prompt (format "%s%s: "
                             (capitalize name)
                             (if required "" " (optional, RET to skip)")))
             (value (if choices
                        (completing-read prompt choices nil t)
                      (read-string prompt))))
        (cond
         ((and required (string-empty-p value))
          (user-error "Required argument `%s' cannot be empty" name))
         ((not (string-empty-p value))
          ;; For :rest args, split on spaces
          (if (eq (clime-arg-nargs arg) :rest)
              (setq values (append values (split-string value)))
            (push value values))))))
    (nreverse values)))

;;; ─── Output Display ────────────────────────────────────────────────────

(defun clime-invoke--display-output (output &optional exit-code)
  "Display OUTPUT string in the *clime-output* buffer.
EXIT-CODE is shown in the mode-line."
  (if (string-empty-p output)
      (message "Command completed (exit %s)" (or exit-code 0))
    (let ((buf (get-buffer-create "*clime-output*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (goto-char (point-min)))
        (special-mode)
        (setq-local mode-line-process
                    (format " [exit %s]" (or exit-code 0))))
      (display-buffer buf)
      ;; Short output also in echo area
      (with-current-buffer buf
        (when (<= (count-lines (point-min) (point-max)) 3)
          (message "%s" (string-trim output)))))))

;;; ─── Run Action ────────────────────────────────────────────────────────

(defun clime-invoke--make-run-action (app command path prefix-sym)
  "Create the \"Run\" suffix action for COMMAND at PATH under PREFIX-SYM.
APP is the root `clime-app'."
  (let ((run-sym (intern (format "%s--run" prefix-sym))))
    (defalias run-sym
      (lambda ()
        (interactive)
        (let* ((trans-args (transient-args prefix-sym))
               ;; Collect option values from transient args
               (option-fragments '())
               (opts (clime-invoke--collect-options command)))
          (dolist (opt opts)
            (let* ((primary-flag (car (clime-option-flags opt)))
                   (frag nil))
              (if (clime-option-boolean-p opt)
                  ;; Switch: check if flag is present
                  (let ((switch-arg (concat primary-flag " ")))
                    (when (member switch-arg trans-args)
                      (setq frag (list primary-flag))))
                ;; Option: extract value from "flag=value" format
                (let ((prefix (concat primary-flag "=")))
                  (dolist (a trans-args)
                    (when (string-prefix-p prefix a)
                      (let ((val (substring a (length prefix))))
                        (unless (string-empty-p val)
                          (setq frag (list primary-flag val))))))))
              (when frag
                (push frag option-fragments))))
          ;; Prompt for positional args
          (let* ((cmd-args (clime-node-args command))
                 (pos-args (condition-case err
                               (clime-invoke--prompt-args cmd-args)
                             (user-error
                              (message "%s" (error-message-string err))
                              'abort))))
            (unless (eq pos-args 'abort)
              (let* ((argv (clime-invoke--build-argv
                            path
                            (nreverse option-fragments)
                            pos-args))
                     (exit-code nil)
                     (output (with-output-to-string
                               (setq exit-code (clime-run app argv)))))
                (clime-invoke--display-output output exit-code)))))))
    (put run-sym 'interactive-only t)
    run-sym))

;;; ─── Prefix Generators ─────────────────────────────────────────────────

(defun clime-invoke--make-group-vector (prefix-sym description specs)
  "Build a transient group vector in the internal layout format.
PREFIX-SYM is the owning prefix.  DESCRIPTION labels the column.
SPECS is a list of shorthand suffix specs to parse.
Returns a vector [LEVEL CLASS PROPS CHILDREN]."
  (vector 1 'transient-column
          (list :description description)
          (transient-parse-suffixes prefix-sym specs)))

(defun clime-invoke--make-command-prefix (app command path)
  "Generate a transient prefix for leaf COMMAND at PATH.
APP is the root `clime-app'.  Returns the prefix symbol."
  (let* ((prefix-sym (clime-invoke--prefix-symbol
                      (clime-node-name app) path))
         (opts (clime-invoke--collect-options command))
         (key-map (clime-invoke--assign-keys opts))
         (grouped (clime-invoke--group-by-category opts))
         (run-sym (clime-invoke--make-run-action app command path prefix-sym))
         (layout '()))
    ;; Build option groups
    (dolist (cat-entry grouped)
      (let* ((cat (car cat-entry))
             (cat-opts (cdr cat-entry))
             (specs '()))
        (dolist (opt cat-opts)
          (let ((key (cdr (assq (clime-option-name opt) key-map))))
            (when key
              (push (clime-invoke--option-to-spec opt key prefix-sym)
                    specs))))
        (push (clime-invoke--make-group-vector
               prefix-sym (or cat "Options") (nreverse specs))
              layout)))
    ;; Add "Run" action group
    (push (clime-invoke--make-group-vector
           prefix-sym "Actions" `(("RET" "Run" ,run-sym)))
          layout)
    ;; Register the prefix
    (clime-invoke--register-prefix prefix-sym (clime-node-name app)
                                   path (nreverse layout))))

(defun clime-invoke--make-group-prefix (app group path)
  "Generate a transient prefix for GROUP at PATH.
APP is the root `clime-app'.  Returns the prefix symbol."
  (let* ((prefix-sym (clime-invoke--prefix-symbol
                      (clime-node-name app) path))
         (children (clime-invoke--visible-children group))
         (child-keys (clime-invoke--assign-child-keys children))
         (child-specs '()))
    ;; Build suffix specs for each child
    (dolist (entry children)
      (let* ((name (car entry))
             (child (cdr entry))
             (key (cdr (assoc name child-keys)))
             (help (or (clime-node-help child) name))
             (child-path (append path (list name))))
        (when key
          (let ((child-sym
                 (if (clime-branch-p child)
                     (clime-invoke--make-group-prefix app child child-path)
                   (clime-invoke--make-command-prefix app child child-path))))
            (push `(,key ,help ,child-sym
                         :transient transient--do-stack)
                  child-specs)))))
    ;; Build layout with the correct 4-element vector format
    (let ((layout
           (list (clime-invoke--make-group-vector
                  prefix-sym
                  (or (clime-node-help group)
                      (clime-node-name group)
                      "Commands")
                  (nreverse child-specs)))))
      (clime-invoke--register-prefix prefix-sym (clime-node-name app)
                                     path layout))))

(defun clime-invoke--register-prefix (prefix-sym app-name path layout)
  "Register PREFIX-SYM as a transient prefix with LAYOUT.
APP-NAME and PATH are used for documentation."
  (defalias prefix-sym
    (lambda () (interactive) (transient-setup prefix-sym)))
  (put prefix-sym 'interactive-only t)
  (put prefix-sym 'function-documentation
       (format "Invoke: %s"
               (string-join (cons app-name (or path '())) " ")))
  (put prefix-sym 'transient--prefix
       (transient-prefix :command prefix-sym))
  (put prefix-sym 'transient--layout layout)
  prefix-sym)

;;; ─── Public API ────────────────────────────────────────────────────────

;;;###autoload
(defun clime-invoke (app &optional refresh)
  "Open a transient menu for clime APP.
APP is a `clime-app' struct.  With prefix arg REFRESH, regenerate
the cached transient layout."
  (interactive (list nil current-prefix-arg))

  (unless app
    (user-error "clime-invoke requires a clime-app struct"))
  (when refresh
    (remhash app clime-invoke--cache))
  (let ((prefix-sym (or (and (not refresh)
                             (gethash app clime-invoke--cache))
                        (let ((sym (clime-invoke--make-group-prefix
                                    app app nil)))
                          (puthash app sym clime-invoke--cache)
                          sym))))
    (transient-setup prefix-sym)))

;;;###autoload
(defun clime-invoke-command (app command-path)
  "Open a transient menu for a specific command in APP.
COMMAND-PATH is a list of strings naming the path to the command."

  (let ((node app))
    (dolist (step command-path)
      (let ((child (and (clime-group-p node)
                        (clime-group-find-child node step))))
        (unless child
          (user-error "Command not found: %s" step))
        (setq node child)))
    (let ((prefix-sym
           (if (clime-branch-p node)
               (clime-invoke--make-group-prefix app node command-path)
             (clime-invoke--make-command-prefix app node command-path))))
      (transient-setup prefix-sym))))

(provide 'clime-invoke)
;;; clime-invoke.el ends here
