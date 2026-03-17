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

(defvar clime-invoke--cache (make-hash-table :test #'equal)
  "Cache of generated transient prefix symbols, keyed by (APP . PATH).")

(defvar clime-invoke--registry (make-hash-table :test #'equal)
  "Registry of named clime apps, keyed by name string.")

;;; ─── Registry ────────────────────────────────────────────────────────

(defun clime-register-app (name app)
  "Register APP under NAME for interactive discovery.
NAME is a string.  APP is a `clime-app' struct."
  (puthash name app clime-invoke--registry))

(defun clime-invoke--registry-keys ()
  "Return a list of registered app name strings."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) clime-invoke--registry)
    (sort keys #'string<)))

(defun clime-invoke--refresh (&optional app)
  "Clear cached transient layout for APP, or all apps when nil."
  (if app
      (remhash app clime-invoke--cache)
    (clrhash clime-invoke--cache)))

;;; ─── Custom Infix Classes ─────────────────────────────────────────────

(defclass clime-invoke-count-infix (transient-argument)
  ()
  "Infix for :count options.  Each key press increments the count.")

(cl-defmethod transient-init-value ((obj clime-invoke-count-infix))
  "Initialize count to 0."
  (oset obj value 0))

(cl-defmethod transient-infix-read ((obj clime-invoke-count-infix))
  "Increment the count, cycling back to 0 after 5."
  (let ((n (or (oref obj value) 0)))
    (if (>= n 5) 0 (1+ n))))

(cl-defmethod transient-format-value ((obj clime-invoke-count-infix))
  "Display the count."
  (let ((n (or (oref obj value) 0)))
    (if (> n 0)
        (propertize (format "×%d" n) 'face 'transient-value)
      (propertize "off" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-value ((obj clime-invoke-count-infix))
  "Return the flag string when count > 0, for `transient-args'."
  (let ((n (or (oref obj value) 0)))
    (when (> n 0)
      (oref obj argument))))

(defclass clime-invoke-multi-infix (transient-option)
  ()
  "Infix for :multiple options.  Each key press adds a value.")

(cl-defmethod transient-init-value ((obj clime-invoke-multi-infix))
  "Initialize to empty list."
  (oset obj value nil))

(cl-defmethod transient-infix-read ((obj clime-invoke-multi-infix))
  "Read a single value to append.  Empty input clears the list."
  (let ((choices (and (slot-boundp obj 'choices) (oref obj choices))))
    (if choices
        (completing-read "Add value: " choices nil t)
      (read-string "Add value (empty to clear): "))))

(cl-defmethod transient-infix-set ((obj clime-invoke-multi-infix) value)
  "Append VALUE to the list, or clear if empty."
  (if (string-empty-p value)
      (oset obj value nil)
    (oset obj value (append (or (oref obj value) '()) (list value)))))

(cl-defmethod transient-format-value ((obj clime-invoke-multi-infix))
  "Display comma-separated values."
  (let ((values (oref obj value)))
    (if values
        (propertize (string-join values ", ") 'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-value ((obj clime-invoke-multi-infix))
  "Return list of flag=val strings for `transient-args'."
  (let ((values (oref obj value))
        (arg (oref obj argument)))
    (when values
      (mapcar (lambda (v) (concat arg v)) values))))

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

(defun clime-invoke--option-key-item (option)
  "Build an assign-keys item for OPTION.
Returns (NAME PREFERRED FALLBACK-STRING)."
  (let* ((preferred (clime-invoke--preferred-key option))
         (long-flag (cl-find-if (lambda (f) (string-prefix-p "--" f))
                                (clime-option-flags option)))
         (fallback (and long-flag (substring long-flag 2))))
    (list (clime-option-name option) preferred fallback)))

(defun clime-invoke--assign-keys (items)
  "Assign unique transient keys to ITEMS.
Each element of ITEMS is (NAME PREFERRED FALLBACK-STRING) where:
  NAME identifies the item (symbol or string),
  PREFERRED is the preferred single-char key string (or nil),
  FALLBACK-STRING is a string whose characters are tried in order.
Return an alist of (NAME . KEY-STRING)."
  (let ((used (make-hash-table :test #'equal))
        (result '()))
    (dolist (item items)
      (let* ((name (nth 0 item))
             (preferred (nth 1 item))
             (fallback (nth 2 item))
             (key nil))
        ;; Try preferred key
        (when (and preferred (not (gethash preferred used)))
          (setq key preferred))
        ;; Try letters from fallback string
        (unless key
          (when fallback
            (cl-loop for i from 0 below (length fallback)
                     for ch = (substring fallback i (1+ i))
                     when (and (string-match-p "[a-z]" ch)
                               (not (gethash ch used)))
                     return (setq key ch))))
        ;; Last resort: first unused a-z
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
   ;; Count flag: emit flag N times
   ((clime-option-count option)
    (let ((flag (car (clime-option-flags option)))
          (n (if (integerp value) value (if value 1 0))))
      (when (> n 0)
        (make-list n flag))))
   ;; Boolean flag
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
OPTION-FRAGMENTS is a list of argv fragments from
`clime-invoke--option-to-argv'.
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

(defun clime-invoke--collect-options-grouped (node)
  "Collect options for NODE, grouped by category with ancestor labeling.
Own options keep their category.  Ancestor options without a category
are tagged under \"Global Options\".
Returns an alist of (CATEGORY . OPTIONS-LIST)."
  (let ((own-opts '())
        (ancestor-opts '()))
    ;; Own options (preserve category)
    (dolist (opt (clime-node-options node))
      (unless (clime-option-hidden opt)
        (push opt own-opts)))
    ;; Ancestor options
    (dolist (ancestor (clime-node-ancestors node))
      (dolist (opt (clime-node-options ancestor))
        (unless (clime-option-hidden opt)
          (push opt ancestor-opts))))
    ;; Group own opts by their category
    (let ((result (clime-invoke--group-by-category (nreverse own-opts))))
      ;; Add ancestor opts under "Global Options"
      (when ancestor-opts
        (setq result (append result
                             (list (cons "Global Options"
                                         (nreverse ancestor-opts))))))
      result)))

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

(defun clime-invoke--arg-to-spec (arg key _prefix-sym)
  "Build a transient suffix spec for positional ARG with KEY.
Returns a shorthand spec consumable by `transient-parse-suffixes'."
  (let* ((name (clime-arg-name arg))
         (help-str (or (clime-arg-help arg)
                       (symbol-name name)))
         (required (clime-arg-required arg))
         (desc-kw (when required
                    (list :description
                          (clime-invoke--required-description help-str nil))))
         (arg-str (concat "<" (symbol-name name) ">="))
         (extra '()))
    (when (clime-arg-choices arg)
      (setq extra (list :choices
                        (clime--resolve-value (clime-arg-choices arg)))))
    `(,key ,help-str ,arg-str ,@extra ,@desc-kw)))

;;; ─── Header Formatter ──────────────────────────────────────────────────

(defun clime-invoke--format-header (node path)
  "Format a header string for NODE at PATH.
Includes the command path, help text, and examples."
  (let ((parts '()))
    ;; Command path
    (when path
      (push (propertize (string-join path " ") 'face 'transient-heading)
            parts))
    ;; Help text
    (when-let ((help (clime-node-help node)))
      (push help parts))
    ;; Examples
    (when-let ((examples (clime-node-examples node)))
      (push "" parts)
      (push "Examples:" parts)
      (dolist (ex examples)
        (cond
         ((and (consp ex) (cdr ex))
          (push (format "  $ %s  — %s" (car ex) (cdr ex)) parts))
         ((consp ex)
          (push (format "  $ %s" (car ex)) parts))
         ((stringp ex)
          (push (format "  $ %s" ex) parts)))))
    (string-join (nreverse parts) "\n")))

;;; ─── Spec Builders ─────────────────────────────────────────────────────

(defun clime-invoke--required-description (help option)
  "Return a function-valued description for a required OPTION.
Shows HELP with a \"(required)\" marker when the infix value is unset."
  (lambda (obj)
    (let ((value (oref obj value)))
      (if (or (null value)
              (and (integerp value) (zerop value))
              (and (stringp value) (string-empty-p value)))
          (concat help " " (propertize "(required)" 'face 'warning))
        help))))

(defun clime-invoke--option-to-spec (option key _prefix-sym)
  "Build a transient suffix spec for OPTION with KEY under PREFIX-SYM.
Returns a shorthand spec list consumable by `transient-parse-suffixes'."
  (let* ((name (clime-option-name option))
         (flags (clime-option-flags option))
         (primary-flag (car flags))
         (help-str (or (clime-option-help option)
                       (symbol-name name)))
         (required (and (clime-option-required option)
                        (not (clime-option-default option))))
         ;; Shorthand requires string in position 2; use :description
         ;; keyword for function-valued dynamic descriptions.
         (desc-kw (when required
                    (list :description
                          (clime-invoke--required-description
                           help-str option)))))
    (cond
     ;; Count flag → custom infix
     ((clime-option-count option)
      `(,key ,help-str ,(concat primary-flag " ")
             :class clime-invoke-count-infix ,@desc-kw))
     ;; Boolean flag → switch
     ((clime-option-boolean-p option)
      `(,key ,help-str ,(concat primary-flag " ") ,@desc-kw))
     ;; Multiple values → custom infix
     ((clime-option-multiple option)
      (let ((extra (when (clime-option-choices option)
                     (list :choices (clime--resolve-value
                                    (clime-option-choices option))))))
        `(,key ,help-str ,(concat primary-flag "=")
               :class clime-invoke-multi-infix ,@extra ,@desc-kw)))
     ;; Option with choices
     ((clime-option-choices option)
      (let ((choices (clime--resolve-value (clime-option-choices option))))
        `(,key ,help-str ,(concat primary-flag "=")
               :choices ,choices ,@desc-kw)))
     ;; Plain value option
     (t
      `(,key ,help-str ,(concat primary-flag "=") ,@desc-kw)))))

;;; ─── Validation ───────────────────────────────────────────────────────

(defun clime-invoke--alist-to-plist (alist)
  "Convert ALIST of (NAME . VALUE) to a plist."
  (let ((plist '()))
    (dolist (pair alist)
      (setq plist (plist-put plist (car pair) (cdr pair))))
    plist))

(defun clime-invoke--validate-pre-run (node params-alist)
  "Validate PARAMS-ALIST against NODE before running.
PARAMS-ALIST is an alist of (NAME . VALUE).
Returns nil on success or an error message string."
  (or
   ;; Check required options
   (let ((opts (clime-invoke--collect-options node)))
     (cl-loop for opt in opts
              when (and (clime-option-required opt)
                        (not (clime-option-default opt))
                        (not (assq (clime-option-name opt) params-alist)))
              return (format "Missing required option: --%s"
                             (clime-option-name opt))))
   ;; Run node conformers
   (when-let ((conform (clime-node-conform node)))
     (let ((plist (clime-invoke--alist-to-plist params-alist)))
       (condition-case err
           (progn
             (if (functionp conform)
                 (funcall conform plist node)
               (dolist (fn conform)
                 (setq plist (funcall fn plist node))))
             nil)
         (clime-usage-error
          (error-message-string err)))))))

(defun clime-invoke--extract-incompatible (node)
  "Extract transient :incompatible groups from NODE's conformers.
Finds `clime-check-exclusive' conformers (identified by their
`clime-exclusive-members' property) and maps member names to
transient argument strings.
Returns a list of lists of argument strings, or nil."
  (let ((conform (clime-node-conform node))
        (groups '()))
    (let ((fns (cond
                ((null conform) nil)
                ((functionp conform) (list conform))
                ((listp conform) conform))))
      (dolist (fn fns)
        (when-let ((members (and (symbolp fn)
                                 (get fn 'clime-exclusive-members))))
          (let ((args '()))
            (dolist (name members)
              (let ((opt (cl-find-if (lambda (o) (eq (clime-option-name o) name))
                                     (clime-node-options node))))
                (when opt
                  (let ((flag (car (clime-option-flags opt))))
                    (push (if (clime-option-boolean-p opt)
                              (concat flag " ")
                            (concat flag "="))
                          args)))))
            (when (>= (length args) 2)
              (push (nreverse args) groups))))))
    (nreverse groups)))

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

(defun clime-invoke--collect-infix-values (opts cmd-args)
  "Collect typed values from transient infix objects.
OPTS is a list of `clime-option' structs.
CMD-ARGS is a list of `clime-arg' structs.
Returns (PARAMS-ALIST OPTION-FRAGMENTS POS-ARGS)."
  (let ((params-alist '())
        (option-fragments '())
        (pos-args '()))
    ;; Walk all active infix suffixes
    (dolist (obj transient--suffixes)
      (when (cl-typep obj 'transient-infix)
        (let ((arg-str (and (slot-boundp obj 'argument)
                            (oref obj argument)))
              (value (oref obj value)))
          (when (and arg-str value)
            ;; Match against options
            (cl-block nil
              (dolist (opt opts)
                (let ((flag (car (clime-option-flags opt))))
                  (when (or (equal arg-str (concat flag " "))
                            (equal arg-str (concat flag "=")))
                    (push (cons (clime-option-name opt) value)
                          params-alist)
                    (push (clime-invoke--option-to-argv opt value)
                          option-fragments)
                    (cl-return))))
              ;; Match against positional args
              (dolist (arg cmd-args)
                (let ((name (clime-arg-name arg)))
                  (when (equal arg-str (concat "<" (symbol-name name) ">="))
                    (push (cons name value) params-alist)
                    (if (eq (clime-arg-nargs arg) :rest)
                        (setq pos-args
                              (append pos-args (split-string value)))
                      (setq pos-args (append pos-args (list value))))
                    (cl-return)))))))))
    (list params-alist
          (nreverse option-fragments)
          pos-args)))

(defun clime-invoke--make-run-action (app command path prefix-sym)
  "Create the \"Run\" suffix action for COMMAND at PATH under PREFIX-SYM.
APP is the root `clime-app'.
The action validates before running and stays in the transient on failure."
  (let ((run-sym (intern (format "%s--run" prefix-sym))))
    (defalias run-sym
      (lambda ()
        (interactive)
        (let* ((opts (clime-invoke--collect-options command))
               (cmd-args (clime-node-args command))
               (collected (clime-invoke--collect-infix-values opts cmd-args))
               (params-alist (nth 0 collected))
               (option-fragments (nth 1 collected))
               (pos-args (nth 2 collected))
               (err (clime-invoke--validate-pre-run command params-alist)))
          (if err
              (message "Error: %s" err)
            (let* ((argv (clime-invoke--build-argv
                          path option-fragments pos-args))
                   (exit-code nil)
                   (output (with-output-to-string
                             (setq exit-code (clime-run app argv)))))
              (transient-quit-all)
              (clime-invoke--display-output output exit-code))))))
    (put run-sym 'interactive-only t)
    run-sym))

;;; ─── Prefix Generators ─────────────────────────────────────────────────

(defun clime-invoke--make-group-vector (prefix-sym description specs)
  "Build a transient group vector in the internal layout format.
PREFIX-SYM is the owning prefix.  DESCRIPTION labels the column.
SPECS is a list of shorthand suffix specs to parse, or nil for
a description-only group (e.g. headers).
Returns a vector [LEVEL CLASS PROPS CHILDREN]."
  (vector 1 'transient-column
          (list :description description)
          (if specs
              (transient-parse-suffixes prefix-sym specs)
            '())))

(defun clime-invoke--make-option-layout (prefix-sym grouped key-map)
  "Build transient layout groups for GROUPED options using KEY-MAP.
GROUPED is an alist of (CATEGORY . OPTIONS-LIST).
KEY-MAP is an alist of (OPTION-NAME . KEY-STRING).
Returns a list of group vectors."
  (let ((layout '()))
    (dolist (cat-entry grouped)
      (let* ((cat (car cat-entry))
             (cat-opts (cdr cat-entry))
             (specs '()))
        (dolist (opt cat-opts)
          (let ((key (cdr (assq (clime-option-name opt) key-map))))
            (when key
              (push (clime-invoke--option-to-spec opt key prefix-sym)
                    specs))))
        (when specs
          (push (clime-invoke--make-group-vector
                 prefix-sym (or cat "Options") (nreverse specs))
                layout))))
    (nreverse layout)))

(defun clime-invoke--make-arg-layout (prefix-sym args)
  "Build transient layout for positional ARGS.
ARGS is a list of `clime-arg' structs.
Returns a list containing one group vector, or nil if no args."
  (when args
    (let* ((key-map (clime-invoke--assign-keys
                     (mapcar (lambda (a)
                               (let ((name (symbol-name (clime-arg-name a))))
                                 (list (clime-arg-name a) nil name)))
                             args)))
           (specs '()))
      (dolist (arg args)
        (let ((key (cdr (assq (clime-arg-name arg) key-map))))
          (when key
            (push (clime-invoke--arg-to-spec arg key prefix-sym) specs))))
      (when specs
        (list (clime-invoke--make-group-vector
               prefix-sym "Arguments" (nreverse specs)))))))

(defun clime-invoke--make-command-prefix (app command path)
  "Generate a transient prefix for leaf COMMAND at PATH.
APP is the root `clime-app'.  Returns the prefix symbol."
  (let* ((prefix-sym (clime-invoke--prefix-symbol
                      (clime-node-name app) path))
         (grouped (clime-invoke--collect-options-grouped command))
         (all-opts (mapcan (lambda (entry) (copy-sequence (cdr entry))) grouped))
         (key-map (clime-invoke--assign-keys
                   (mapcar #'clime-invoke--option-key-item all-opts)))
         (run-sym (clime-invoke--make-run-action app command path prefix-sym))
         (layout '()))
    ;; Header
    (let ((header (clime-invoke--format-header command path)))
      (unless (string-empty-p header)
        (push (clime-invoke--make-group-vector
               prefix-sym header nil)
              layout)))
    ;; Option groups
    (setq layout (append layout
                         (clime-invoke--make-option-layout
                          prefix-sym grouped key-map)))
    ;; Positional args
    (setq layout (append layout
                         (clime-invoke--make-arg-layout
                          prefix-sym (clime-node-args command))))
    ;; Run action (stays in transient on failure)
    (setq layout (append layout
                         (list (clime-invoke--make-group-vector
                                prefix-sym "Actions"
                                `(("RET" "Run" ,run-sym
                                         :transient t))))))
    (clime-invoke--register-prefix prefix-sym command path layout)))

(defun clime-invoke--make-group-prefix (app group path)
  "Generate a transient prefix for GROUP at PATH.
APP is the root `clime-app'.  Returns the prefix symbol.
Shows options (own + ancestor) alongside child navigation."
  (let* ((prefix-sym (clime-invoke--prefix-symbol
                      (clime-node-name app) path))
         ;; Options (own + ancestor)
         (grouped (clime-invoke--collect-options-grouped group))
         (all-opts (mapcan (lambda (entry) (copy-sequence (cdr entry))) grouped))
         (key-map (clime-invoke--assign-keys
                   (mapcar #'clime-invoke--option-key-item all-opts)))
         (opt-layout (clime-invoke--make-option-layout
                      prefix-sym grouped key-map))
         ;; Children
         (children (clime-invoke--visible-children group))
         (child-keys (clime-invoke--assign-keys
                      (mapcar (lambda (entry)
                                (let ((name (car entry)))
                                  (list name (substring name 0 1) name)))
                              children)))
         (child-specs '())
         (layout '()))
    ;; Header
    (let ((header (clime-invoke--format-header group path)))
      (unless (string-empty-p header)
        (push (clime-invoke--make-group-vector prefix-sym header nil)
              layout)))
    ;; Option groups
    (setq layout (append layout opt-layout))
    ;; Build child navigation specs
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
    ;; Commands group
    (when child-specs
      (setq layout (append layout
                           (list (clime-invoke--make-group-vector
                                  prefix-sym "Commands"
                                  (nreverse child-specs))))))
    (clime-invoke--register-prefix prefix-sym group path layout)))

(defun clime-invoke--register-prefix (prefix-sym node path layout)
  "Register PREFIX-SYM as a transient prefix with LAYOUT.
NODE is the clime node.  PATH is the command path list."
  (defalias prefix-sym
    (lambda () (interactive) (transient-setup prefix-sym)))
  (put prefix-sym 'interactive-only t)
  (put prefix-sym 'function-documentation
       (format "Invoke: %s"
               (string-join (cons (clime-node-name node) (or path '())) " ")))
  (let* ((incompatible (clime-invoke--extract-incompatible node))
         (prefix-obj (apply #'transient-prefix
                            :command prefix-sym
                            (when incompatible
                              (list :incompatible incompatible)))))
    (put prefix-sym 'transient--prefix prefix-obj))
  (put prefix-sym 'transient--layout layout)
  prefix-sym)

(defun clime-invoke--params-to-transient-value (node params)
  "Convert PARAMS alist to transient prefix value format for NODE.
PARAMS is an alist of (NAME . VALUE).
Returns a list of argument strings suitable for `transient--prefix' value."
  (let ((result '()))
    ;; Options
    (dolist (opt (clime-invoke--collect-options node))
      (let ((pair (assq (clime-option-name opt) params)))
        (when (and pair (cdr pair))
          (let ((flag (car (clime-option-flags opt)))
                (val (cdr pair)))
            (cond
             ((clime-option-boolean-p opt)
              (push (concat flag " ") result))
             (t
              (push (concat flag "="
                            (if (stringp val) val (format "%s" val)))
                    result)))))))
    ;; Positional args
    (dolist (arg (clime-node-args node))
      (let ((pair (assq (clime-arg-name arg) params)))
        (when (and pair (cdr pair))
          (let ((val (cdr pair)))
            (push (concat (symbol-name (clime-arg-name arg)) "="
                          (if (stringp val) val (format "%s" val)))
                  result)))))
    (nreverse result)))

;;; ─── Public API ────────────────────────────────────────────────────────

;;;###autoload
(defun clime-invoke (app &optional path params)
  "Open a transient menu for clime APP.
APP is a `clime-app' struct, or nil to select from registered apps.
PATH is an optional list of strings naming a node to navigate to.
PARAMS is an optional alist of (NAME . VALUE) to pre-populate infixes."
  (interactive (list nil))
  ;; Resolve app from registry when nil
  (unless app
    (let ((keys (clime-invoke--registry-keys)))
      (unless keys
        (user-error "No apps registered; pass an app struct directly"))
      (let ((name (completing-read "App: " keys nil t)))
        (setq app (gethash name clime-invoke--registry)))))
  ;; Navigate to path if given
  (let ((node app))
    (when path
      (dolist (step path)
        (let ((child (and (clime-group-p node)
                          (clime-group-find-child node step))))
          (unless child
            (user-error "Command not found: %s" step))
          (setq node child))))
    ;; Generate or retrieve cached prefix
    (let* ((cache-key (cons app path))
           (prefix-sym (or (gethash cache-key clime-invoke--cache)
                           (let ((sym (if (and path (not (clime-branch-p node)))
                                          (clime-invoke--make-command-prefix
                                           app node path)
                                        (clime-invoke--make-group-prefix
                                         app node path))))
                             (puthash cache-key sym clime-invoke--cache)
                             sym))))
      ;; Pre-populate infixes from PARAMS
      (when params
        (let ((prefix-obj (get prefix-sym 'transient--prefix)))
          (oset prefix-obj value
                (clime-invoke--params-to-transient-value node params))))
      (transient-setup prefix-sym))))

;;;###autoload
(defun clime-invoke-command (app command-path)
  "Open a transient menu for a specific command in APP.
COMMAND-PATH is a list of strings naming the path to the command.
Deprecated: use `clime-invoke' with PATH instead."
  (declare (obsolete clime-invoke "0.3.0"))
  (clime-invoke app command-path))

(provide 'clime-invoke)
;;; clime-invoke.el ends here
