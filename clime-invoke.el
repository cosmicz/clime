;;; clime-invoke.el --- Interactive menu for clime apps  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Lightweight interactive menu driven by `clime-app' structs.
;; The app tree IS the menu — no separate menu definition needed.
;; A `read-key' loop renders the current node and dispatches input.

;;; Code:

(require 'cl-lib)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-help)
(require 'clime-run)

;;; ─── Faces ───────────────────────────────────────────────────────────

(defface clime-invoke-key
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for key bindings in the menu (generic fallback)."
  :group 'clime)

(defface clime-invoke-option-key
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for option key bindings (-X)."
  :group 'clime)

(defface clime-invoke-command-key
  '((t :inherit font-lock-type-face))
  "Face for command key bindings."
  :group 'clime)

(defface clime-invoke-arg-key
  '((t :inherit font-lock-constant-face))
  "Face for argument key bindings."
  :group 'clime)

(defface clime-invoke-action-key
  '((t :inherit font-lock-builtin-face))
  "Face for action key bindings (RET, q)."
  :group 'clime)

(defface clime-invoke-active
  '((t :inherit font-lock-string-face :weight bold))
  "Face for actively set values."
  :group 'clime)

(defface clime-invoke-value
  '((t :inherit font-lock-string-face))
  "Face for set option values."
  :group 'clime)

(defface clime-invoke-default
  '((t :inherit shadow :slant italic))
  "Face for default (non-active) values."
  :group 'clime)

(defface clime-invoke-unset
  '((t :inherit shadow))
  "Face for unset option values."
  :group 'clime)

(defface clime-invoke-heading
  '((t :weight bold))
  "Face for section headings."
  :group 'clime)

(defface clime-invoke-error
  '((t :inherit error))
  "Face for error messages."
  :group 'clime)

(defface clime-invoke-invalid
  '((t :inherit error))
  "Face for inline validation error text."
  :group 'clime)

(defface clime-invoke-locked
  '((t :inherit warning))
  "Face for locked option values (from alias :vals)."
  :group 'clime)

(defface clime-invoke-dimmed
  '((t :inherit shadow))
  "Face for inactive elements during prefix key input."
  :group 'clime)

(defface clime-invoke-hidden
  '((t :inherit (shadow italic)))
  "Face for hidden items when revealed via visibility toggle."
  :group 'clime)

;;; ─── Customization ──────────────────────────────────────────────────

(defcustom clime-invoke-display-buffer-action
  '(display-buffer-in-side-window
    (side . bottom)
    (slot . 0)
    (dedicated . t)
    (inhibit-same-window . t)
    (preserve-size . (nil . t)))
  "Display buffer action for the menu window."
  :type '(choice (const :tag "Default" nil)
                 (sexp :tag "Custom display-buffer action"))
  :group 'clime)

(defcustom clime-invoke-output-display-action nil
  "Display buffer action for the command output window.
When nil, use default `display-buffer' behavior.
See Info node `(elisp)Choosing Window' for action format."
  :type '(choice (const :tag "Default" nil)
                 (sexp :tag "Custom display-buffer action"))
  :group 'clime)

;;; ─── Internal State ─────────────────────────────────────────────────

(defvar clime-invoke--show-mode 'normal
  "Visibility mode for the invoke menu.
One of `normal' (default), `all' (show hidden), or `clean' (hide deprecated).")

(defvar clime-invoke--registry (make-hash-table :test #'equal)
  "Registry of named clime apps, keyed by name string.")

(defconst clime-invoke--buffer-name " *clime-menu*"
  "Buffer name for the menu display.")

;;; ─── Registry ───────────────────────────────────────────────────────

(defun clime-register-app (name app)
  "Register APP under NAME for interactive discovery.
NAME is a string.  APP is a `clime-app' struct."
  (puthash name app clime-invoke--registry))

(defun clime-invoke--registry-keys ()
  "Return a list of registered app name strings."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) clime-invoke--registry)
    (sort keys #'string<)))

;;; ─── Pure Helpers: Key Assignment ───────────────────────────────────

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
        (let ((long (cl-find-if (lambda (f) (string-prefix-p "--" f)) flags)))
          (when long
            (substring long 2 3))))))

(defun clime-invoke--option-key-item (option)
  "Build an assign-keys item for OPTION.
Returns (NAME PREFERRED FALLBACK-STRING).
Preferred key is the short flag letter (e.g. \"v\" from \"-v\")."
  (let* ((short-letter (cl-loop for flag in (clime-option-flags option)
                                when (and (= (length flag) 2)
                                          (string-prefix-p "-" flag)
                                          (not (string-prefix-p "--" flag)))
                                return (substring flag 1 2)))
         (long-flag (cl-find-if (lambda (f) (string-prefix-p "--" f))
                                (clime-option-flags option)))
         (fallback (and long-flag (substring long-flag 2))))
    (list (clime-option-name option) short-letter fallback)))

(defun clime-invoke--assign-keys (items &optional used)
  "Assign unique keys to ITEMS.
Each element of ITEMS is (NAME PREFERRED FALLBACK-STRING &optional PREFIX)
where:
  NAME identifies the item (symbol or string),
  PREFERRED is the preferred key string (or nil),
  FALLBACK-STRING is a string whose characters are tried in order,
  PREFIX is prepended to each fallback candidate (default \"\").
USED, when non-nil, is a pre-populated hash table of taken keys.
Return an alist of (NAME . KEY-STRING)."
  (let ((used (or used (make-hash-table :test #'equal)))
        (result '()))
    (dolist (item items)
      (let* ((name (nth 0 item))
             (preferred (nth 1 item))
             (fallback (nth 2 item))
             (prefix (or (nth 3 item) ""))
             (key nil))
        (when (and preferred (not (gethash preferred used)))
          (setq key preferred))
        (unless key
          (when fallback
            (cl-loop for i from 0 below (length fallback)
                     for ch = (substring fallback i (1+ i))
                     for candidate = (concat prefix ch)
                     when (and (string-match-p "[a-z]" ch)
                               (not (gethash candidate used)))
                     return (setq key candidate))))
        (unless key
          (cl-loop for code from ?a to ?z
                   for ch = (char-to-string code)
                   for candidate = (concat prefix ch)
                   when (not (gethash candidate used))
                   return (setq key candidate)))
        (when key
          (puthash key t used)
          (push (cons name key) result))))
    (nreverse result)))

;;; ─── Pure Helpers: Option Cycling ───────────────────────────────────

(defun clime-invoke--cycle-choice (current choices)
  "Return the next choice after CURRENT in CHOICES, cycling to nil after last.
Returns the first choice when CURRENT is nil or not in CHOICES."
  (let ((tail (and current (cdr (member current choices)))))
    (cond
     (tail (car tail))
     ((and current (member current choices)) nil)
     (t (car choices)))))

(defun clime-invoke--cycle-choice-backward (current choices)
  "Return the previous choice before CURRENT in CHOICES.
Cycles: nil → last, first → nil."
  (cond
   ((null current) (car (last choices)))
   (t (let ((pos (cl-position current choices :test #'equal)))
        (cond
         ((or (null pos) (= pos 0)) nil)
         (t (nth (1- pos) choices)))))))

(defun clime-invoke--cycle-ternary (current pos-flag neg-flag)
  "Cycle ternary state: nil → POS-FLAG → NEG-FLAG → nil."
  (cond
   ((null current) pos-flag)
   ((equal current pos-flag) neg-flag)
   (t nil)))

(defun clime-invoke--read-count (n pfx)
  "Compute next count value from current N and PFX (prefix arg).
nil → increment (wraps at 5).  Integer → set directly.  Other → decrement."
  (cond
   ((integerp pfx) (max 0 pfx))
   (pfx (max 0 (1- n)))
   (t (if (>= n 5) 0 (1+ n)))))

;;; ─── Tree Helpers ───────────────────────────────────────────────────

(defun clime-invoke--visible-children (group)
  "Return visible children of GROUP as an alist.
Filters based on `clime-invoke--show-mode':
  normal — excludes hidden, shows deprecated
  all    — shows everything including hidden
  clean  — excludes hidden and deprecated
Promotes inline group children."
  (let ((result '()))
    (dolist (entry (clime-group-children group))
      (let ((name (car entry))
            (child (cdr entry)))
        (cond
         ((and (clime-group-p child) (clime-node-inline child))
          (dolist (sub-entry (clime-invoke--visible-children child))
            (push sub-entry result)))
         ((and (not (eq clime-invoke--show-mode 'all))
               (clime-node-hidden child))
          nil)
         ((and (eq clime-invoke--show-mode 'clean)
               (clime-node-deprecated child))
          nil)
         (t (push (cons name child) result)))))
    (nreverse result)))

;;; ─── Key Map Building ───────────────────────────────────────────────

(defun clime-invoke--build-child-actions (node shared-used)
  "Build (:child NAME NODE) actions for visible children of NODE.
SHARED-USED is a hash table of taken keys.  Returns alist of (KEY . ACTION)."
  (when (clime-branch-p node)
    (let* ((children (clime-invoke--visible-children node))
           (child-keys (clime-invoke--assign-keys
                        (mapcar (lambda (entry)
                                  (let ((name (car entry)))
                                    (list name (substring name 0 1) name)))
                                children)
                        shared-used))
           (actions '()))
      (dolist (entry children)
        (let* ((name (car entry))
               (child (cdr entry))
               (key (cdr (assoc name child-keys))))
          (when key
            (push (cons key (list :child name child)) actions))))
      (nreverse actions))))

(defun clime-invoke--build-arg-actions (node shared-used)
  "Build (:arg ARG) actions for positional args of NODE.
SHARED-USED is a hash table of taken keys.  Returns alist of (KEY . ACTION)."
  (let* ((args (clime-node-args node))
         (arg-keys (clime-invoke--assign-keys
                    (mapcar (lambda (a)
                              (let ((name (symbol-name (clime-arg-name a))))
                                (list (clime-arg-name a) nil name)))
                            args)
                    shared-used))
         (actions '()))
    (dolist (arg args)
      (let ((key (cdr (assq (clime-arg-name arg) arg-keys))))
        (when key
          (push (cons key (list :arg arg)) actions))))
    (nreverse actions)))

(defun clime-invoke--build-option-actions (node)
  "Build (:option OPTION) actions for own + ancestor options of NODE.
Options use the \"- X\" two-key namespace.  Returns alist of (KEY . ACTION)."
  (let* ((own (cl-remove-if (lambda (o) (or (and (clime-option-hidden o)
                                                (not (eq clime-invoke--show-mode 'all)))
                                            (clime-option-locked o)
                                            (and (eq clime-invoke--show-mode 'clean)
                                                 (clime-option-deprecated o))))
                            (clime-node-all-options node)))
         (ancestor (clime-help--collect-ancestor-options node))
         (opts (append own ancestor))
         (opt-used (make-hash-table :test #'equal))
         (key-map (clime-invoke--assign-keys
                   (mapcar #'clime-invoke--option-key-item opts)
                   opt-used))
         (actions '()))
    (dolist (opt opts)
      (let ((letter (cdr (assq (clime-option-name opt) key-map))))
        (when letter
          (push (cons (concat "- " letter) (list :option opt)) actions))))
    (nreverse actions)))

(defun clime-invoke--build-key-map (node)
  "Build a key dispatch map for NODE.
Returns an alist of (KEY . ACTION) where ACTION is one of:
  (:option OPTION)   — toggle/cycle/prompt for option
  (:arg ARG)         — prompt for positional arg value
  (:child NAME NODE) — enter child node
  (:run NODE)        — run handler"
  (let ((shared-used (make-hash-table :test #'equal)))
    (puthash "?" t shared-used)
    (append (clime-invoke--build-child-actions node shared-used)
            (clime-invoke--build-arg-actions node shared-used)
            (clime-invoke--build-option-actions node)
            (when (clime-node-handler node)
              (list (cons "RET" (list :run node)))))))

;;; ─── Value Formatting ───────────────────────────────────────────────

(defun clime-invoke--format-choices (choices selected default)
  "Format CHOICES list with SELECTED highlighted, DEFAULT in parens.
When more than 5 choices, truncate around SELECTED with ellipsis."
  (let* ((active (or selected default))
         (idx (and active (cl-position active choices :test #'equal)))
         (max-visible 5)
         (truncate-p (> (length choices) max-visible))
         (visible choices)
         (prefix-dots nil)
         (suffix-dots nil))
    (when truncate-p
      (let* ((center (or idx 0))
             (half (/ (1- max-visible) 2))
             (start (max 0 (- center half)))
             (end (min (length choices) (+ start max-visible))))
        (when (< (- end start) max-visible)
          (setq start (max 0 (- end max-visible))))
        (setq visible (cl-subseq choices start end)
              prefix-dots (> start 0)
              suffix-dots (< end (length choices)))))
    (let ((parts (mapcar
                  (lambda (c)
                    (cond
                     ((and selected (equal c selected))
                      (propertize (format "%s" c) 'face 'clime-invoke-active))
                     ((and (null selected) default (equal c default))
                      (propertize (format "(%s)" c) 'face 'clime-invoke-default))
                     (t (format "%s" c))))
                  visible)))
      (concat (if prefix-dots "... | " "")
              (string-join parts " | ")
              (if suffix-dots " | ..." "")))))

(defun clime-invoke--format-value (option)
  "Format the display value for OPTION from its :value slot."
  (let* ((val (clime-param-value option))
         (default (clime-option-default option)))
    (cond
     ;; Count option
     ((clime-option-count option)
      (let ((n (or val 0)))
        (if (> n 0)
            (propertize (format "×%d" n) 'face 'clime-invoke-active)
          (propertize "off" 'face 'clime-invoke-unset))))
     ;; Negatable (ternary)
     ((clime-option-negatable option)
      (let* ((long (cl-find-if (lambda (f) (string-prefix-p "--" f))
                               (clime-option-flags option)))
             (neg-flag (and long (concat "--no-" (substring long 2)))))
        (cond
         ((equal val long)
          (propertize "on" 'face 'clime-invoke-active))
         ((equal val neg-flag)
          (propertize "off" 'face 'clime-invoke-active))
         (t (propertize "auto" 'face 'clime-invoke-unset)))))
     ;; Boolean flag
     ((clime-option-boolean-p option)
      (if val
          (propertize "on" 'face 'clime-invoke-active)
        (propertize "off" 'face 'clime-invoke-unset)))
     ;; Choices — show all inline
     ((clime-option-choices option)
      (let ((choices (clime--resolve-value (clime-option-choices option)))
            (d (and default (clime--resolve-value default))))
        (clime-invoke--format-choices choices val d)))
     ;; Multiple
     ((clime-option-multiple option)
      (if (and val (listp val) val)
          (string-join (mapcar (lambda (v) (propertize (format "%s" v) 'face 'clime-invoke-active)) val)
                      ", ")
        (propertize "(unset)" 'face 'clime-invoke-unset)))
     ;; Plain value
     (t
      (cond
       (val (propertize (format "%s" val) 'face 'clime-invoke-active))
       (default
        (let ((d (clime--resolve-value default)))
          (propertize (format "(%s)" d) 'face 'clime-invoke-default)))
       (t (propertize "(unset)" 'face 'clime-invoke-unset)))))))

(defun clime-invoke--format-desc (option-or-arg)
  "Format the desc column for OPTION-OR-ARG.
Returns help text with long flag in parens and annotations."
  (let* ((help (clime-param-help option-or-arg))
         (parts '()))
    ;; Help text or long flag as fallback
    (when help
      (push help parts))
    ;; Long flag in parens (options only)
    (when (clime-option-p option-or-arg)
      (let ((long (cl-find-if (lambda (f) (string-prefix-p "--" f))
                              (clime-option-flags option-or-arg))))
        (when long
          (push (propertize (format "(%s)" long) 'face 'shadow) parts))))
    ;; When no help and no long flag pushed, use name
    (unless parts
      (push (symbol-name name) parts))
    ;; Annotations
    (let ((annotations '()))
      ;; Required — always show, dimmed when satisfied
      (when (and (clime-param-required option-or-arg)
                 (not (clime-param-default option-or-arg)))
        (if (clime-param-source option-or-arg)
            (push (propertize "(required)" 'face 'shadow) annotations)
          (push (propertize "(required)" 'face 'warning) annotations)))
      ;; Deprecated
      (when (and (clime-option-p option-or-arg)
                 (clime-option-deprecated option-or-arg))
        (push (propertize "(deprecated)" 'face 'warning) annotations))
      ;; Hidden (only visible in all mode)
      (when (and (clime-option-p option-or-arg)
                 (clime-option-hidden option-or-arg))
        (push (propertize "(hidden)" 'face 'clime-invoke-hidden) annotations))
      ;; Type hint (non-string)
      (when (clime-option-p option-or-arg)
        (let ((type (clime-option-type option-or-arg)))
          (when (and type (not (eq type 'string)))
            (push (propertize (format "(%s)" type) 'face 'shadow) annotations))))
      ;; Multiple hint
      (when (and (clime-option-p option-or-arg)
                 (clime-option-multiple option-or-arg))
        (push (propertize "(multi)" 'face 'shadow) annotations))
      (when annotations
        (setq parts (append parts (nreverse annotations)))))
    (string-join (nreverse parts) " ")))

(defun clime-invoke--format-env (option &optional app)
  "Format the env column for OPTION, or return nil.
APP is the root app (for :env-prefix resolution).
Shows [$VAR=resolved] with active face on value when env is source."
  (when (clime-option-p option)
    (let ((env-name (clime--env-var-for-option option app)))
      (when env-name
        (let* ((env-val (getenv env-name))
               (explicit (and (clime-param-source option)
                              (not (eq (clime-param-source option) 'env))))
               (has-val (and env-val (not (string-empty-p env-val)))))
          (if has-val
              (let ((prefix (propertize (format "[$%s=" env-name) 'face 'shadow))
                    (value (if explicit
                               (propertize env-val 'face 'shadow)
                             (propertize env-val 'face 'clime-invoke-active)))
                    (suffix (propertize "]" 'face 'shadow)))
                (concat prefix value suffix))
            (propertize (format "[$%s]" env-name) 'face 'shadow)))))))

;;; ─── Validation ─────────────────────────────────────────────────────

(defun clime-invoke--validate-param (param)
  "Validate a single PARAM, returning nil or error string.
PARAM is a `clime-option' or `clime-arg'.  Skips unset values.
Reuses `clime--transform-value' for type coercion and choices validation,
then runs the param's `:conform' function (if any) on the coerced value."
  (let* ((name (clime-param-name param))
         (val (clime-param-value param)))
    (when (and val (clime-param-source param))
      (let* ((type (if (clime-option-p param)
                       (clime-option-type param)
                     (clime-arg-type param)))
             (choices (if (clime-option-p param)
                          (clime-option-choices param)
                        (clime-arg-choices param)))
             (coerce (if (clime-option-p param)
                         (clime-option-coerce param)
                       (clime-arg-coerce param)))
             (cfn (if (clime-option-p param)
                      (clime-option-conform param)
                    (clime-arg-conform param)))
             (flag-or-name (if (clime-option-p param)
                               (car (clime-option-flags param))
                             (symbol-name name))))
        ;; Resolve dynamic choices eagerly (safe in invoke context)
        (let ((resolved-choices (and choices (clime--resolve-value choices))))
          (if (stringp val)
              ;; String values: type-coerce first, then conform
              (condition-case err
                  (let ((coerced (clime--transform-value
                                  val type resolved-choices coerce flag-or-name)))
                    (when cfn
                      (condition-case err
                          (progn (clime--call-conform cfn coerced param) nil)
                        (error (error-message-string err)))))
                (clime-usage-error (cadr err)))
            ;; Non-string values (pre-filled already coerced): run conform only
            (when cfn
              (condition-case err
                  (progn (clime--call-conform cfn val param) nil)
                (error (error-message-string err))))))))))

(defun clime-invoke--run-conformer-checks (node params)
  "Run NODE's conformers on PARAMS, returning attributed errors.
PARAMS is a plist; it is converted to a values map for conformers.
Also walks inline group children (postorder) to collect their errors.
Returns a list of (MESSAGE . PARAM-NAMES) cons cells, where PARAM-NAMES
is a list of symbols or nil if the conformer didn't attribute params."
  (let ((errors nil)
        ;; Convert params plist to values map for conformers
        (values (cl-loop for (k v) on params by #'cddr
                         collect (list k :value v :source 'user))))
    ;; Walk inline group children first (postorder)
    (when (clime-group-p node)
      (dolist (entry (clime-group-children node))
        (let ((child (cdr entry)))
          (when (and (clime-group-p child) (clime-node-inline child))
            (setq errors (nconc errors
                                (clime-invoke--run-conformer-checks child params)))))))
    ;; Then this node's own conformers
    (let ((fns (clime-node-conform node)))
      (when (functionp fns) (setq fns (list fns)))
      (dolist (conform fns)
        (condition-case err
            (funcall conform (copy-sequence values) node)
          (clime-usage-error
           (let* ((data (cdr err))
                  (msg (car data))
                  (plist (cdr data))
                  (params-attr (plist-get plist :params)))
             (push (cons msg params-attr) errors))))))
    (nreverse errors)))

(defun clime-invoke--validate-all (node)
  "Validate NODE's params, returning (PARAM-ERRORS . GENERAL-ERRORS).
PARAM-ERRORS is an alist of (NAME . error-string) for per-param issues.
GENERAL-ERRORS is a list of strings from conformers and requires checks.
Derives a params plist from struct values for conformer/requires checks."
  (let ((param-errors nil)
        (general-errors nil))
    ;; Per-param validation: own options (including inline groups) + ancestor options + args
    ;; Skip locked options — their values are not user-settable
    (dolist (opt (clime-node-all-options node))
      (unless (clime-option-locked opt)
        (when-let ((err (clime-invoke--validate-param opt)))
          (push (cons (clime-option-name opt) err) param-errors))))
    (dolist (opt (clime-help--collect-ancestor-options node))
      (when-let ((err (clime-invoke--validate-param opt)))
        (push (cons (clime-option-name opt) err) param-errors)))
    (dolist (arg (clime-node-args node))
      (when-let ((err (clime-invoke--validate-param arg)))
        (push (cons (clime-arg-name arg) err) param-errors)))
    ;; Derive params plist for conformer/requires checks
    (let ((params (clime-app-params node)))
      ;; Requires checks
      (let* ((scope (cons node (clime-node-ancestors node))))
        (dolist (err (clime--find-unsatisfied-requires scope params))
          (push err general-errors)))
      ;; Conformer checks — inject locked vals so mutex/zip checks see them
      ;; Skip locked options with nil value (excluded siblings, not set values)
      (let ((check-params (copy-sequence params)))
        (dolist (opt (clime-node-all-options node))
          (when (and (clime-option-locked opt)
                     (not (null (clime-param-value opt))))
            (let ((name (clime-option-name opt)))
              (unless (plist-member check-params name)
                (setq check-params (plist-put check-params name
                                              (clime-param-value opt)))))))
        ;; Unpack (MESSAGE . PARAM-NAMES) pairs
        (dolist (entry (clime-invoke--run-conformer-checks node check-params))
          (let ((msg (car entry))
                (attr-params (cdr entry)))
            (if attr-params
                ;; Attributed: inline only (like type errors)
                (dolist (name attr-params)
                  (push (cons name msg) param-errors))
              ;; Unattributed: header
              (push msg general-errors))))))
    (cons (nreverse param-errors) (nreverse general-errors))))

;;; ─── Rendering ──────────────────────────────────────────────────────

(defun clime-invoke--find-root (node)
  "Walk up NODE's parent chain to find the root app."
  (let ((n node))
    (while (clime-node-parent n)
      (setq n (clime-node-parent n)))
    n))

(defun clime-invoke--format-header (node)
  "Format a breadcrumb header for NODE.
Shows appname > group > command — Help text.
At root, shows appname vVERSION."
  (let* ((root (clime-invoke--find-root node))
         (root-name (clime-node-name root))
         (parts '()))
    ;; Build breadcrumb path from root to node
    (let ((chain '())
          (n node))
      (while (and n (not (eq n root)))
        (push (clime-node-name n) chain)
        (setq n (clime-node-parent n)))
      (if chain
          ;; Nested: appname > path > segments
          (let ((sep (propertize " > " 'face 'shadow)))
            (push (concat (propertize root-name 'face 'clime-invoke-heading)
                          sep
                          (propertize (string-join chain (concat sep))
                                      'face 'clime-invoke-heading))
                  parts))
        ;; Root: appname vVERSION
        (let ((version (and (clime-app-p root) (clime-app-version root))))
          (push (concat (propertize root-name 'face 'clime-invoke-heading)
                        (if version
                            (propertize (format " v%s" version) 'face 'shadow)
                          ""))
                parts))))
    ;; Help text after separator
    (when-let ((help (clime-node-help node)))
      (push (concat (propertize " — " 'face 'shadow) help) parts))
    ;; Examples on subsequent lines
    (when-let ((examples (clime-node-examples node)))
      (push "\n" parts)
      (dolist (ex examples)
        (cond
         ((and (consp ex) (cdr ex))
          (push (format "\n  $ %s  — %s" (car ex) (cdr ex)) parts))
         ((consp ex)
          (push (format "\n  $ %s" (car ex)) parts))
         ((stringp ex)
          (push (format "\n  $ %s" ex) parts)))))
    (string-join (nreverse parts))))

(defun clime-invoke--render-to-string (node key-map error-msg
                                            &optional at-root validation-result
                                            prefix-state)
  "Render the menu for NODE with KEY-MAP and ERROR-MSG.
When KEY-MAP is nil, builds one automatically from NODE.
AT-ROOT non-nil means this is the entry-point node (q exits entirely).
VALIDATION-RESULT is (PARAM-ERRORS . GENERAL-ERRORS) from `clime-invoke--validate-all'.
PREFIX-STATE is nil, \"-\", or \"=\" when a prefix key is active.
Uses 4-column layout: Key | Desc | Value | Env."
  (unless key-map
    (setq key-map (clime-invoke--build-key-map node)))
  (let ((lines '())
        (param-errors (car validation-result))
        (general-errors (cdr validation-result))
        (dimmed prefix-state)
        (root (let ((n node)) (while (clime-node-parent n) (setq n (clime-node-parent n))) n)))
    ;; Header (breadcrumb)
    (let ((header (clime-invoke--format-header node)))
      (unless (string-empty-p header)
        (push header lines)
        (push "" lines)))
    ;; Error (user errors + general validation errors)
    (let ((all-errors
           (append (when error-msg (list error-msg))
                   general-errors)))
      (when all-errors
        (push (propertize (string-join all-errors "; ")
                          'face 'clime-invoke-error)
              lines)
        (push "" lines)))
    ;; Options grouped: own options by category, then ancestor as "Global Options"
    (let* ((own (cl-remove-if (lambda (o)
                                (or (and (clime-option-hidden o)
                                         (not (eq clime-invoke--show-mode 'all)))
                                    (and (eq clime-invoke--show-mode 'clean)
                                         (clime-option-deprecated o))))
                              (clime-node-all-options node)))
           (ancestor (clime-help--collect-ancestor-options node))
           (grouped (append
                     (when own (list (cons nil own)))
                     (when ancestor (list (cons "Global Options" ancestor))))))
      (dolist (cat-entry grouped)
        (let ((cat (or (car cat-entry) "Options"))
              (opts (cdr cat-entry)))
          (push (propertize cat 'face 'clime-invoke-heading) lines)
          (dolist (opt opts)
            (if (clime-option-locked opt)
                ;; Locked option: show full flags dimmed, value in locked face
                (let* ((desc (clime-invoke--format-desc opt))
                       (locked-val (clime-param-value opt))
                       (has-val (not (null locked-val)))
                       (val-str (if has-val
                                    (propertize (format "%s" locked-val)
                                                'face 'clime-invoke-locked)
                                  (propertize "(excluded)" 'face 'clime-invoke-dimmed))))
                  (push (format "     %s  %s  %s"
                                (clime-invoke--pad-to
                                 (propertize desc 'face 'clime-invoke-dimmed)
                                 30)
                                val-str
                                (if has-val
                                    (propertize "(locked)" 'face 'clime-invoke-dimmed)
                                  ""))
                        lines))
              ;; Normal option: interactive with key binding
              (let* ((name (clime-option-name opt))
                     (key (car (cl-find-if
                                (lambda (entry)
                                  (and (eq (cadr entry) :option)
                                       (eq (clime-option-name (caddr entry)) name)))
                                key-map)))
                     (desc (clime-invoke--format-desc opt))
                     (val-str (clime-invoke--format-value opt))
                     (env-str (clime-invoke--format-env opt root))
                     (param-err (cdr (assq name param-errors))))
                (when key
                  (let ((display-key (if prefix-state
                                         (clime-invoke--prefix-display-key key prefix-state)
                                       (clime-invoke--display-key key))))
                    (push (concat
                           (format " %s %s  %s"
                                   (propertize (format "%3s" display-key)
                                               'face 'clime-invoke-option-key)
                                   (clime-invoke--pad-to desc 30)
                                   val-str)
                           (when param-err
                             (concat "  " (propertize (concat "← " param-err)
                                                      'face 'clime-invoke-invalid)))
                           (when env-str (concat "  " env-str)))
                          lines)))))))
        (push "" lines)))
    ;; Positional args
    (let ((args (clime-node-args node)))
      (when args
        (push (propertize "Arguments" 'face (if dimmed 'clime-invoke-dimmed
                                              'clime-invoke-heading))
              lines)
        (dolist (arg args)
          (let* ((name (clime-arg-name arg))
                 (key (car (cl-find-if
                            (lambda (entry)
                              (and (eq (cadr entry) :arg)
                                   (eq (clime-arg-name (caddr entry)) name)))
                            key-map)))
                 (desc (clime-invoke--format-desc arg))
                 (val (clime-param-value arg))
                 (val-str (if val
                              (propertize (format "%s" val) 'face 'clime-invoke-active)
                            (propertize "(unset)" 'face 'clime-invoke-unset)))
                 (param-err (cdr (assq name param-errors))))
            (when key
              (push (concat
                     (format " %s %s  %s"
                             (propertize (format "%3s" key)
                                         'face (if dimmed 'clime-invoke-dimmed
                                                 'clime-invoke-arg-key))
                             (clime-invoke--pad-to desc 30)
                             val-str)
                     (when param-err
                       (concat "  " (propertize (concat "← " param-err)
                                                'face 'clime-invoke-invalid))))
                    lines))))
        (push "" lines)))
    ;; Children (only on branch nodes, not commands)
    (when (clime-branch-p node)
      (let ((children (clime-invoke--visible-children node)))
        (when children
          (push (propertize "Commands" 'face (if dimmed 'clime-invoke-dimmed
                                               'clime-invoke-heading))
                lines)
          (dolist (entry children)
            (let* ((name (car entry))
                   (child (cdr entry))
                   (key (car (cl-find-if
                              (lambda (e)
                                (and (eq (cadr e) :child)
                                     (equal (caddr e) name)))
                              key-map)))
                   (help (or (clime-node-help child) name))
                   (ann (concat
                         (if (clime-node-hidden child)
                             (concat " " (propertize "(hidden)"
                                                     'face 'clime-invoke-hidden))
                           "")
                         (if (clime-node-deprecated child)
                             (concat " " (propertize "(deprecated)"
                                                     'face 'warning))
                           ""))))
              (when key
                (push (format " %s %s%s"
                              (propertize (format "%3s" key)
                                          'face (if dimmed 'clime-invoke-dimmed
                                                  'clime-invoke-command-key))
                              help ann)
                      lines))))
          (push "" lines))))
    ;; Actions footer
    (push (propertize "Actions" 'face (if dimmed 'clime-invoke-dimmed
                                        'clime-invoke-heading))
          lines)
    (let ((actions '())
          (act-face (if dimmed 'clime-invoke-dimmed 'clime-invoke-action-key)))
      (when (clime-node-handler node)
        (push (format "%s %s"
                      (propertize "RET" 'face act-face)
                      "Run")
              actions))
      (unless at-root
        (push (format "%s %s"
                      (propertize "DEL" 'face act-face)
                      "Back")
              actions))
      (push (format "%s %s"
                    (propertize "ESC" 'face act-face)
                    "Quit")
            actions)
      (push (format "%s %s"
                    (propertize "?" 'face act-face)
                    (symbol-name clime-invoke--show-mode))
            actions)
      (push (concat " " (string-join (nreverse actions) "    ")) lines))
    (string-join (nreverse lines) "\n")))


(defun clime-invoke--display-key (key)
  "Format KEY for display.  Converts \"- v\" to \"-v\"."
  (if (string-match "\\`- \\(.\\)\\'" key)
      (concat "-" (match-string 1 key))
    key))

(defun clime-invoke--prefix-display-key (key prefix)
  "Format option KEY with PREFIX for display during prefix state.
Extracts the letter from \"- v\" and prepends PREFIX (e.g. \"=v\")."
  (if (string-match "\\`- \\(.\\)\\'" key)
      (concat prefix (match-string 1 key))
    key))

(defun clime-invoke--pad-to (str width)
  "Pad STR with spaces to WIDTH characters."
  (if (>= (length str) width)
      str
    (concat str (make-string (- width (length str)) ?\s))))

(defun clime-invoke--render (node key-map error-msg buffer
                                  &optional at-root validation-result
                                  prefix-state)
  "Render menu for NODE into BUFFER.
VALIDATION-RESULT is (PARAM-ERRORS . GENERAL-ERRORS) or nil.
PREFIX-STATE is nil, \"-\", or \"=\" when a prefix key is active."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (clime-invoke--render-to-string
               node key-map error-msg at-root validation-result
               prefix-state))
      (goto-char (point-min))))
  ;; Fit window to content
  (let ((win (get-buffer-window buffer)))
    (when win
      (fit-window-to-buffer win))))

;;; ─── Option Interaction ─────────────────────────────────────────────

(defun clime-invoke--seed-params (tree params)
  "Seed initial PARAMS plist onto TREE's option/arg structs.
Walks all nodes setting :value/:source for matching params."
  (let ((work (list tree)))
    (while work
      (let ((node (pop work)))
        (dolist (opt (clime-node-options node))
          (let ((val (plist-member params (clime-param-name opt))))
            (when val
              (setf (clime-param-value opt) (cadr val)
                    (clime-param-source opt) 'user))))
        (dolist (arg (clime-node-args node))
          (let ((val (plist-member params (clime-param-name arg))))
            (when val
              (setf (clime-param-value arg) (cadr val)
                    (clime-param-source arg) 'user))))
        (when (clime-group-p node)
          (dolist (entry (clime-group-children node))
            (push (cdr entry) work)))))))

(defun clime-invoke--set-param (param value)
  "Set PARAM's :value to VALUE and :source to 'user."
  (setf (clime-param-value param) value
        (clime-param-source param) 'user))

(defun clime-invoke--clear-param (param)
  "Clear PARAM's :value and :source (unset)."
  (setf (clime-param-value param) nil
        (clime-param-source param) nil))

(defun clime-invoke--handle-option (option)
  "Handle user interaction for OPTION, mutating its :value/:source slots."
  (let ((current (clime-param-value option)))
    (cond
     ;; Count
     ((clime-option-count option)
      (let ((n (clime-invoke--read-count (or current 0) nil)))
        (if (zerop n)
            (clime-invoke--clear-param option)
          (clime-invoke--set-param option n))))
     ;; Negatable (ternary)
     ((clime-option-negatable option)
      (let* ((pos-flag (cl-find-if (lambda (f) (string-prefix-p "--" f))
                                   (clime-option-flags option)))
             (neg-flag (and pos-flag (concat "--no-" (substring pos-flag 2))))
             (next (clime-invoke--cycle-ternary current pos-flag neg-flag)))
        (if next
            (clime-invoke--set-param option next)
          (clime-invoke--clear-param option))))
     ;; Boolean flag
     ((clime-option-boolean-p option)
      (if current
          (clime-invoke--clear-param option)
        (clime-invoke--set-param option t)))
     ;; Choices
     ((clime-option-choices option)
      (let* ((choices (clime--resolve-value (clime-option-choices option)))
             (next (clime-invoke--cycle-choice current choices)))
        (if next
            (clime-invoke--set-param option next)
          (clime-invoke--clear-param option))))
     ;; Multiple
     ((clime-option-multiple option)
      (let* ((choices (and (clime-option-choices option)
                           (clime--resolve-value (clime-option-choices option))))
             (val (if choices
                      (completing-read "Add value: " choices nil t)
                    (read-string "Add value (empty to clear): "))))
        (if (string-empty-p val)
            (clime-invoke--clear-param option)
          (clime-invoke--set-param option (append (or current '()) (list val))))))
     ;; Plain value
     (t
      (let ((val (read-string (format "%s: " (or (clime-option-help option)
                                                 (symbol-name (clime-option-name option))))
                              (and current (format "%s" current)))))
        (if (string-empty-p val)
            (clime-invoke--clear-param option)
          (clime-invoke--set-param option val)))))))


(defun clime-invoke--handle-arg (arg)
  "Handle user interaction for positional ARG, mutating its :value/:source slots."
  (let* ((name (clime-arg-name arg))
         (current (clime-param-value arg))
         (choices (and (clime-arg-choices arg)
                       (clime--resolve-value (clime-arg-choices arg))))
         (val (if choices
                  (completing-read
                   (format "%s: " (or (clime-arg-help arg) (symbol-name name)))
                   choices nil t current)
                (read-string
                 (format "%s: " (or (clime-arg-help arg) (symbol-name name)))
                 current))))
    (if (string-empty-p val)
        (clime-invoke--clear-param arg)
      (clime-invoke--set-param arg val))))

(defun clime-invoke--handle-option-direct (option)
  "Handle direct-input interaction for OPTION, mutating its :value/:source slots.
Unlike cycling, this prompts the user for an explicit value."
  (let* ((name (clime-option-name option))
         (current (clime-param-value option)))
    (cond
     ;; Choices: completing-read
     ((clime-option-choices option)
      (let* ((choices (clime--resolve-value (clime-option-choices option)))
             (val (completing-read
                   (format "%s: " (or (clime-option-help option) (symbol-name name)))
                   choices nil t (and current (format "%s" current)))))
        (if (string-empty-p val)
            (clime-invoke--clear-param option)
          (clime-invoke--set-param option val))))
     ;; Count: prompt for number
     ((clime-option-count option)
      (let* ((prompt (format "%s (0-5, current %s): "
                             (or (clime-option-help option) (symbol-name name))
                             (or current 0)))
             (input (read-string prompt))
             (n (and (string-match-p "\\`[0-9]+\\'" input)
                     (string-to-number input))))
        (unless (and n (<= 0 n) (<= n 5))
          (error "Invalid count: %s (expected 0-5)" input))
        (if (zerop n)
            (clime-invoke--clear-param option)
          (clime-invoke--set-param option n))))
     ;; Boolean/negatable/plain: delegate to cycling handler
     (t (clime-invoke--handle-option option)))))

;;; ─── Run Handler ────────────────────────────────────────────────────
;;;

(defun clime-invoke--run-handler (app node path params)
  "Run NODE's handler with PARAMS, returning (EXIT-CODE . OUTPUT).
APP is the root app.  PATH is the command path list.
Runs the full parse finalization pipeline (defaults, env vars,
conformers, required checks) before calling the handler."
  (let* ((values (cl-loop for (k v) on params by #'cddr
                          collect (list k :value v :source 'user)))
         (result (clime-parse-result--create
                  :command node
                  :node node
                  :path path
                  :display-path path
                  :params (copy-sequence params)
                  :values values
                  :tree app))
         (exit-code nil)
         (output (with-output-to-string
                   (setq exit-code
                         (condition-case err
                             (progn
                               (clime-parse-finalize result)
                               (let ((ctx (clime--build-context app result)))
                                 (clime-run--execute
                                  (clime-node-handler node) ctx)))
                           (clime-usage-error
                            (princ (cadr err))
                            2)
                           (clime-help-requested
                            (clime--print-help (cdr err))
                            0)
                           (error
                            (princ (error-message-string err))
                            1))))))
    (cons (or exit-code 0) output)))

;;; ─── Output Display ─────────────────────────────────────────────────

(defun clime-invoke--display-output (output &optional exit-code display)
  "Display OUTPUT string in the *clime-output* buffer.
EXIT-CODE is shown in the mode-line.

DISPLAY controls how the output is shown:
  t (default)  — short output (≤3 lines) is shown via `message' only;
                  longer output opens a window via `display-buffer'.
  `message'    — always use `message', never open a window.
  `silent'     — suppress all display (buffer is still populated)."
  (let ((code (or exit-code 0))
        (display (or display t))
        (buf (get-buffer-create "*clime-output*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (string-empty-p output)
            (insert (format "Command completed (exit %d)\n" code))
          (insert output))
        (goto-char (point-min)))
      (special-mode)
      (setq-local mode-line-process (format " [exit %s]" code)))
    (unless (eq display 'silent)
      (let ((short (and (not (string-empty-p output))
                        (with-current-buffer buf
                          (<= (count-lines (point-min) (point-max)) 3)))))
        (when (and (eq display t) (not short))
          (display-buffer buf clime-invoke-output-display-action))
        (when (and (not (string-empty-p output))
                   (or short (eq display 'message)))
          (message "%s" (string-trim output)))))))

;;; ─── Event Loop ─────────────────────────────────────────────────────

(defvar clime-invoke-prefix-timeout 5
  "Seconds to wait for second key after a prefix key (- or =).")

(defun clime-invoke--read-prefixed-key (prefix)
  "Read one more key after PREFIX with timeout.
Returns the full key string (e.g. \"- v\") or nil on timeout/C-g."
  (let ((ch (read-event (format "%s " prefix) nil clime-invoke-prefix-timeout)))
    (message nil)
    (cond
     ((null ch) nil)
     ((memq ch '(?\C-g 7)) nil)
     (t (concat prefix " " (key-description (vector ch)))))))

(defun clime-invoke--loop (app node path &optional at-root)
  "Run the menu event loop for NODE.
APP is the root app.  PATH is the current command path.
AT-ROOT non-nil means this is the entry-point node.
Returns (LAST-OUTPUT QUIT-ALL) where LAST-OUTPUT is
\(EXIT-CODE . STRING) or nil, and QUIT-ALL non-nil means ESC was pressed."
  (let ((buf (get-buffer-create clime-invoke--buffer-name))
        (key-map (clime-invoke--build-key-map node))
        (error-msg nil)
        (last-output nil)
        (validation-result nil)
        (quit-all nil)
        (running t))
    (display-buffer buf clime-invoke-display-buffer-action)
    ;; Initial validation
    (setq validation-result (clime-invoke--validate-all node))
    (while running
      (clime-invoke--render node key-map error-msg buf
                            at-root validation-result)
      (let* ((ch (read-key))
             (key (key-description (vector ch)))
             (key (if (member key '("-" "="))
                      (progn
                        ;; Re-render with prefix state visible, show hint in echo area
                        (clime-invoke--render node key-map error-msg buf
                                              at-root validation-result key)
                        (message (if (equal key "-") "- cycle" "= direct"))
                        (let ((ch2 (read-event nil nil clime-invoke-prefix-timeout)))
                          (message nil)
                          (cond
                           ((null ch2) :prefix-cancelled)
                           ((memq ch2 '(?\C-g 7)) :prefix-cancelled)
                           (t (concat key " " (key-description (vector ch2)))))))
                    key)))
        (setq error-msg nil)
        (when (eq key :prefix-cancelled)
          (setq key nil))
        (cond
         ;; Prefix cancelled (timeout or C-g during prefix read)
         ((null key) nil)
         ;; Quit (ESC = quit all, DEL/C-g = back one level)
         ((equal key "ESC")
          (setq running nil quit-all t))
         ((or (equal key "DEL") (equal key "C-g"))
          (setq running nil))
         ;; Visibility toggle
         ((equal key "?")
          (setq clime-invoke--show-mode
                (pcase clime-invoke--show-mode
                  ('normal 'all)
                  ('all 'clean)
                  (_ 'normal)))
          (setq key-map (clime-invoke--build-key-map node)))
         ;; Dispatch from key-map
         (t
          (let ((action (cdr (assoc key key-map))))
            ;; "= X" direct-input: look up corresponding "- X" option
            (when (and (null action) (string-prefix-p "= " key))
              (let* ((opt-key (concat "- " (substring key 2)))
                     (opt-action (cdr (assoc opt-key key-map))))
                (when (and opt-action (eq (car opt-action) :option))
                  (setq action (list :option-direct (cadr opt-action))))))
            (if (null action)
                (setq error-msg (format "Unknown key: %s" key))
              (pcase (car action)
                (:option
                 (let ((opt (cadr action)))
                   (condition-case err
                       (progn
                         (clime-invoke--handle-option opt)
                         (setq validation-result
                               (clime-invoke--validate-all node)))
                     (quit nil)
                     (error
                      (setq error-msg (error-message-string err))))))
                (:option-direct
                 (let ((opt (cadr action)))
                   (condition-case err
                       (progn
                         (clime-invoke--handle-option-direct opt)
                         (setq validation-result
                               (clime-invoke--validate-all node)))
                     (quit nil)
                     (error
                      (setq error-msg (error-message-string err))))))
                (:arg
                 (let ((arg (cadr action)))
                   (condition-case err
                       (progn
                         (clime-invoke--handle-arg arg)
                         (setq validation-result
                               (clime-invoke--validate-all node)))
                     (quit nil)
                     (error
                      (setq error-msg (error-message-string err))))))
                (:child
                 (let* ((child-name (cadr action))
                        (child-node (caddr action))
                        (child-path (append path (list child-name))))
                   (let ((child-result
                          (clime-invoke--loop app child-node child-path)))
                     (setq validation-result
                           (clime-invoke--validate-all node))
                     (when (cadr child-result)
                       ;; Propagate quit-all from child
                       (setq running nil quit-all t))
                     (when-let ((child-output (car child-result)))
                       ;; Propagate: close parent loop too, show output
                       (setq last-output child-output
                             running nil)))))
                (:run
                 (let* ((params (clime-app-params node))
                        (run-result (clime-invoke--run-handler app node path params))
                        (exit-code (car run-result))
                        (cmd-output (cdr run-result)))
                   (if (zerop exit-code)
                       (setq running nil
                             last-output (cons exit-code cmd-output))
                     (setq error-msg
                           (format "Exit %d: %s"
                                   exit-code (string-trim cmd-output))))))
                (_
                 (setq error-msg (format "Unknown action: %s" (car action)))))))))))
    ;; Re-display parent after returning from child
    (when (buffer-live-p buf)
      (display-buffer buf clime-invoke-display-buffer-action))
    (list last-output quit-all)))

;;; ─── Public API ─────────────────────────────────────────────────────

;;;###autoload
(cl-defun clime-invoke (app &optional path params &key display)
  "Open an interactive menu for clime APP.
APP is a `clime-app' struct, or nil to select from registered apps.
PATH is an optional list of strings naming a node to navigate to.
PARAMS is an optional plist of initial param values.

DISPLAY controls how command output is shown (see
`clime-invoke--display-output' for values).  Default is t.

Return a plist (:params PLIST :exit EXIT :output OUTPUT) where:
  :params  — final parameter values
  :exit    — handler exit code (integer), or nil if user quit
  :output  — handler output (string), or nil if user quit"
  (interactive (list nil))
  (when app
    (clime-register-app (clime-node-name app) app))
  (unless app
    (let ((keys (clime-invoke--registry-keys)))
      (unless keys
        (user-error "No apps registered; pass an app struct directly"))
      (let ((name (completing-read "App: " keys nil t)))
        (setq app (gethash name clime-invoke--registry)))))
  ;; Run setup hook once
  (let ((setup (clime-app-setup app)))
    (when setup
      (let ((result (clime-parse app '() t)))
        (funcall setup app result)
        (clime-parse-finalize result))))
  ;; Deep-copy tree so struct mutations don't persist on the registered app
  (clime--set-parent-refs app)
  (let ((tree (clime--deep-copy-tree app)))
    ;; Seed initial params onto tree structs
    (when params
      (clime-invoke--seed-params tree params))
    ;; Navigate to starting node
    (let ((node tree)
          (nav-path '()))
      (when path
        (dolist (step path)
          (let ((child (and (clime-group-p node)
                            (clime-group-find-child node step))))
            (unless child
              (user-error "Command not found: %s" step))
            (setq node child)
            (push step nav-path)))
        (setq nav-path (nreverse nav-path)))
      ;; Enter the loop
      (let ((clime-invoke--show-mode 'normal)
            loop-result)
        (unwind-protect
            (setq loop-result
                  (clime-invoke--loop tree node (or nav-path '()) t))
          ;; Clean up invoke buffer regardless of how we exit
          (when-let ((buf (get-buffer clime-invoke--buffer-name)))
            (let ((win (get-buffer-window buf)))
              (when win (delete-window win)))
            (kill-buffer buf)))
        ;; Show output after invoke buffer is gone
        (let ((last-output (car loop-result)))
          (when last-output
            (clime-invoke--display-output (cdr last-output) (car last-output) display))
          (list :params (clime-app-params node)
                :exit (and last-output (car last-output))
                :output (and last-output (cdr last-output))))))))

;;;###autoload
(defun clime-invoke-command (app command-path)
  "Open an interactive menu for a specific command in APP.
COMMAND-PATH is a list of strings naming the path to the command.
Deprecated: use `clime-invoke' with PATH instead."
  (declare (obsolete clime-invoke "0.3.0"))
  (clime-invoke app command-path))

(provide 'clime-invoke)
;;; clime-invoke.el ends here
