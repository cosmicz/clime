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
(require 'clime-help)
(require 'clime-run)

;;; ─── Faces ───────────────────────────────────────────────────────────

(defface clime-invoke-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for key bindings in the menu."
  :group 'clime)

(defface clime-invoke-value
  '((t :inherit font-lock-string-face))
  "Face for set option values."
  :group 'clime)

(defface clime-invoke-unset
  '((t :inherit shadow))
  "Face for unset option values."
  :group 'clime)

(defface clime-invoke-heading
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for section headings."
  :group 'clime)

(defface clime-invoke-error
  '((t :inherit error))
  "Face for error messages."
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

;;; ─── Internal State ─────────────────────────────────────────────────

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

(defun clime-invoke--read-count (n prefix-arg)
  "Compute next count value from current N and PREFIX-ARG.
nil → increment (wraps at 5).  Integer → set directly.  Other → decrement."
  (cond
   ((integerp prefix-arg) (max 0 prefix-arg))
   (prefix-arg (max 0 (1- n)))
   (t (if (>= n 5) 0 (1+ n)))))

;;; ─── Tree Helpers ───────────────────────────────────────────────────

(defun clime-invoke--visible-children (group)
  "Return visible children of GROUP as an alist.
Excludes hidden nodes.  Promotes inline group children."
  (let ((result '()))
    (dolist (entry (clime-group-children group))
      (let ((name (car entry))
            (child (cdr entry)))
        (cond
         ((and (clime-group-p child) (clime-node-inline child))
          (dolist (sub-entry (clime-invoke--visible-children child))
            (push sub-entry result)))
         ((clime-node-hidden child) nil)
         (t (push (cons name child) result)))))
    (nreverse result)))

;;; ─── Key Map Building ───────────────────────────────────────────────

(defun clime-invoke--build-child-actions (node shared-used)
  "Build (:child NAME NODE) actions for visible children of NODE.
SHARED-USED is a hash table of taken keys.  Returns alist of (KEY . ACTION)."
  (when (clime-group-p node)
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
  (let* ((own (cl-remove-if #'clime-option-hidden (clime-node-options node)))
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
    (puthash "q" t shared-used)
    (puthash "?" t shared-used)
    (append (clime-invoke--build-child-actions node shared-used)
            (clime-invoke--build-arg-actions node shared-used)
            (clime-invoke--build-option-actions node)
            (when (clime-node-handler node)
              (list (cons "RET" (list :run node)))))))

;;; ─── Value Formatting ───────────────────────────────────────────────

(defun clime-invoke--format-value (option params)
  "Format the display value for OPTION given PARAMS plist."
  (let* ((name (clime-option-name option))
         (val (plist-get params name))
         (default (clime-option-default option)))
    (cond
     ;; Count option
     ((clime-option-count option)
      (let ((n (or val 0)))
        (if (> n 0)
            (propertize (format "×%d" n) 'face 'clime-invoke-value)
          (propertize "off" 'face 'clime-invoke-unset))))
     ;; Negatable (ternary)
     ((clime-option-negatable option)
      (let* ((long (cl-find-if (lambda (f) (string-prefix-p "--" f))
                               (clime-option-flags option)))
             (neg-flag (and long (concat "--no-" (substring long 2)))))
        (cond
         ((equal val long)
          (propertize "on" 'face 'clime-invoke-value))
         ((equal val neg-flag)
          (propertize "off" 'face 'clime-invoke-value))
         (t (propertize "auto" 'face 'clime-invoke-unset)))))
     ;; Boolean flag
     ((clime-option-boolean-p option)
      (if val
          (propertize "on" 'face 'clime-invoke-value)
        (propertize "off" 'face 'clime-invoke-unset)))
     ;; Choices
     ((clime-option-choices option)
      (if val
          (propertize (format "%s" val) 'face 'clime-invoke-value)
        (let ((d (and default (clime--resolve-value default))))
          (if d
              (propertize (format "%s" d) 'face 'clime-invoke-unset)
            (propertize "unset" 'face 'clime-invoke-unset)))))
     ;; Multiple
     ((clime-option-multiple option)
      (if (and val (listp val) val)
          (propertize (string-join (mapcar (lambda (v) (format "%s" v)) val) ", ")
                      'face 'clime-invoke-value)
        (propertize "unset" 'face 'clime-invoke-unset)))
     ;; Plain value
     (t
      (if val
          (propertize (format "%s" val) 'face 'clime-invoke-value)
        (let ((d (and default (clime--resolve-value default))))
          (if d
              (propertize (format "%s" d) 'face 'clime-invoke-unset)
            (propertize "unset" 'face 'clime-invoke-unset))))))))

;;; ─── Rendering ──────────────────────────────────────────────────────

(defun clime-invoke--format-header (node path)
  "Format a header string for NODE at PATH."
  (let ((parts '()))
    (when path
      (push (propertize (string-join path " ") 'face 'clime-invoke-heading)
            parts))
    (when-let ((help (clime-node-help node)))
      (push help parts))
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

(defun clime-invoke--render-to-string (node params key-map error-msg &optional at-root)
  "Render the menu for NODE with PARAMS, KEY-MAP, and ERROR-MSG.
When KEY-MAP is nil, builds one automatically from NODE.
AT-ROOT non-nil means this is the entry-point node (q exits entirely)."
  (unless key-map
    (setq key-map (clime-invoke--build-key-map node)))
  (let ((lines '())
        (path (let ((p '()) (n node))
                (while (clime-node-parent n)
                  (push (clime-node-name n) p)
                  (setq n (clime-node-parent n)))
                p)))
    ;; Header
    (let ((header (clime-invoke--format-header node path)))
      (unless (string-empty-p header)
        (push header lines)
        (push "" lines)))
    ;; Error
    (when error-msg
      (push (propertize error-msg 'face 'clime-invoke-error) lines)
      (push "" lines))
    ;; Options grouped: own options by category, then ancestor as "Global Options"
    (let* ((own (cl-remove-if #'clime-option-hidden (clime-node-options node)))
           (ancestor (clime-help--collect-ancestor-options node))
           (grouped (append
                     (when own (list (cons nil own)))
                     (when ancestor (list (cons "Global Options" ancestor))))))
      (dolist (cat-entry grouped)
        (let ((cat (or (car cat-entry) "Options"))
              (opts (cdr cat-entry)))
          (push (propertize cat 'face 'clime-invoke-heading) lines)
          (dolist (opt opts)
            (let* ((name (clime-option-name opt))
                   (key (car (cl-find-if
                              (lambda (entry)
                                (and (eq (cadr entry) :option)
                                     (eq (clime-option-name (caddr entry)) name)))
                              key-map)))
                   (help-base (or (clime-option-help opt) (symbol-name name)))
                   (help (if (and (clime-option-required opt)
                                  (not (clime-option-default opt))
                                  (not (plist-get params name)))
                             (concat help-base " "
                                     (propertize "(required)" 'face 'warning))
                           help-base))
                   (val-str (clime-invoke--format-value opt params)))
              (when key
                (push (format " %s %s  %s"
                              (propertize (format "%3s" (clime-invoke--display-key key))
                                          'face 'clime-invoke-key)
                              (clime-invoke--pad-to help 30)
                              val-str)
                      lines))))
          (push "" lines))))
    ;; Positional args
    (let ((args (clime-node-args node)))
      (when args
        (push (propertize "Arguments" 'face 'clime-invoke-heading) lines)
        (dolist (arg args)
          (let* ((name (clime-arg-name arg))
                 (key (car (cl-find-if
                            (lambda (entry)
                              (and (eq (cadr entry) :arg)
                                   (eq (clime-arg-name (caddr entry)) name)))
                            key-map)))
                 (help-base (or (clime-arg-help arg) (symbol-name name)))
                 (help (if (and (clime-arg-required arg)
                                (not (plist-get params name)))
                           (concat help-base " "
                                   (propertize "(required)" 'face 'warning))
                         help-base))
                 (val (plist-get params name))
                 (val-str (if val
                              (propertize (format "%s" val) 'face 'clime-invoke-value)
                            (propertize "unset" 'face 'clime-invoke-unset))))
            (when key
              (push (format " %s %s  %s"
                            (propertize (format "%3s" key) 'face 'clime-invoke-key)
                            (clime-invoke--pad-to help 30)
                            val-str)
                    lines))))
        (push "" lines)))
    ;; Children
    (when (clime-group-p node)
      (let ((children (clime-invoke--visible-children node)))
        (when children
          (push (propertize "Commands" 'face 'clime-invoke-heading) lines)
          (dolist (entry children)
            (let* ((name (car entry))
                   (child (cdr entry))
                   (key (car (cl-find-if
                              (lambda (e)
                                (and (eq (cadr e) :child)
                                     (equal (caddr e) name)))
                              key-map)))
                   (help (or (clime-node-help child) name)))
              (when key
                (push (format " %s %s"
                              (propertize (format "%3s" key) 'face 'clime-invoke-key)
                              help)
                      lines))))
          (push "" lines))))
    ;; Run action
    (when (clime-node-handler node)
      (push (format " %s %s"
                    (propertize "RET" 'face 'clime-invoke-key)
                    "Run")
            lines))
    ;; Quit / Return
    (push (format " %s %s"
                  (propertize "  q" 'face 'clime-invoke-key)
                  (if at-root "Quit" "Return"))
          lines)
    (string-join (nreverse lines) "\n")))

(defun clime-invoke--display-key (key)
  "Format KEY for display.  Converts \"- v\" to \"-v\"."
  (if (string-match "\\`- \\(.\\)\\'" key)
      (concat "-" (match-string 1 key))
    key))

(defun clime-invoke--pad-to (str width)
  "Pad STR with spaces to WIDTH characters."
  (if (>= (length str) width)
      str
    (concat str (make-string (- width (length str)) ?\s))))

(defun clime-invoke--render (node params key-map error-msg buffer &optional at-root)
  "Render menu for NODE into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (clime-invoke--render-to-string node params key-map error-msg at-root))
      (goto-char (point-min))))
  ;; Fit window to content
  (let ((win (get-buffer-window buffer)))
    (when win
      (fit-window-to-buffer win))))

;;; ─── Option Interaction ─────────────────────────────────────────────

(defun clime-invoke--handle-option (option params)
  "Handle user interaction for OPTION, updating PARAMS.
Returns the updated params plist."
  (let* ((name (clime-option-name option))
         (current (plist-get params name)))
    (cond
     ;; Count
     ((clime-option-count option)
      (let ((n (clime-invoke--read-count (or current 0) nil)))
        (if (zerop n)
            (clime-invoke--plist-remove params name)
          (plist-put params name n))))
     ;; Negatable (ternary)
     ((clime-option-negatable option)
      (let* ((pos-flag (cl-find-if (lambda (f) (string-prefix-p "--" f))
                                   (clime-option-flags option)))
             (neg-flag (and pos-flag (concat "--no-" (substring pos-flag 2))))
             (next (clime-invoke--cycle-ternary current pos-flag neg-flag)))
        (if next
            (plist-put params name next)
          (clime-invoke--plist-remove params name))))
     ;; Boolean flag
     ((clime-option-boolean-p option)
      (if current
          (clime-invoke--plist-remove params name)
        (plist-put params name t)))
     ;; Choices
     ((clime-option-choices option)
      (let* ((choices (clime--resolve-value (clime-option-choices option)))
             (next (clime-invoke--cycle-choice current choices)))
        (if next
            (plist-put params name next)
          (clime-invoke--plist-remove params name))))
     ;; Multiple
     ((clime-option-multiple option)
      (let* ((choices (and (clime-option-choices option)
                           (clime--resolve-value (clime-option-choices option))))
             (val (if choices
                      (completing-read "Add value: " choices nil t)
                    (read-string "Add value (empty to clear): "))))
        (if (string-empty-p val)
            (clime-invoke--plist-remove params name)
          (plist-put params name (append (or current '()) (list val))))))
     ;; Plain value
     (t
      (let ((val (read-string (format "%s: " (or (clime-option-help option)
                                                 (symbol-name name)))
                              (and current (format "%s" current)))))
        (if (string-empty-p val)
            (clime-invoke--plist-remove params name)
          (plist-put params name val)))))))

(defun clime-invoke--plist-remove (plist key)
  "Return PLIST with KEY removed."
  (let ((result '()))
    (cl-loop for (k v) on plist by #'cddr
             unless (eq k key)
             do (push k result) (push v result))
    (nreverse result)))

(defun clime-invoke--handle-arg (arg params)
  "Handle user interaction for positional ARG, updating PARAMS.
Returns the updated params plist."
  (let* ((name (clime-arg-name arg))
         (current (plist-get params name))
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
        (clime-invoke--plist-remove params name)
      (plist-put params name val))))

;;; ─── Run Handler ────────────────────────────────────────────────────
;;;

(defun clime-invoke--run-handler (app node path params)
  "Run NODE's handler with PARAMS, returning (EXIT-CODE . OUTPUT).
APP is the root app.  PATH is the command path list.
Runs the full parse finalization pipeline (defaults, env vars,
conformers, required checks) before calling the handler."
  (let* ((visited-nodes (cons node (reverse (clime-node-ancestors node))))
         (result (clime-parse-result--create
                  :command node
                  :node node
                  :path path
                  :display-path path
                  :params (copy-sequence params)
                  :visited-nodes visited-nodes))
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

(defun clime-invoke--display-output (output &optional exit-code)
  "Display OUTPUT string in the *clime-output* buffer.
EXIT-CODE is shown in the mode-line."
  (let ((code (or exit-code 0))
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
    (display-buffer buf)
    (when (not (string-empty-p output))
      (with-current-buffer buf
        (when (<= (count-lines (point-min) (point-max)) 3)
          (message "%s" (string-trim output)))))))

;;; ─── Event Loop ─────────────────────────────────────────────────────

(defvar clime-invoke-prefix-timeout 1.5
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

(defun clime-invoke--loop (app node path params &optional at-root)
  "Run the menu event loop for NODE.
APP is the root app.  PATH is the current command path.
PARAMS is the shared params plist.
AT-ROOT non-nil means this is the entry-point node.
Returns (PARAMS LAST-OUTPUT) where LAST-OUTPUT is (EXIT-CODE . STRING) or nil."
  (let ((buf (get-buffer-create clime-invoke--buffer-name))
        (key-map (clime-invoke--build-key-map node))
        (error-msg nil)
        (last-output nil)
        (running t))
    (display-buffer buf clime-invoke-display-buffer-action)
    (while running
      (clime-invoke--render node params key-map error-msg buf at-root)
      (let* ((ch (read-key))
             (key (key-description (vector ch)))
             (key (if (member key '("-" "="))
                      (or (clime-invoke--read-prefixed-key key)
                          :prefix-cancelled)
                    key)))
        (setq error-msg nil)
        (when (eq key :prefix-cancelled)
          (setq key nil))
        (cond
         ;; Prefix cancelled (timeout or C-g during prefix read)
         ((null key) nil)
         ;; Quit
         ((or (equal key "q") (equal key "C-g"))
          (setq running nil))
         ;; Help
         ((equal key "?")
          (setq error-msg
                (format "Keys: %s"
                        (mapconcat (lambda (e) (car e)) key-map ", "))))
         ;; Dispatch from key-map
         (t
          (let ((action (cdr (assoc key key-map))))
            (if (null action)
                (setq error-msg (format "Unknown key: %s" key))
              (pcase (car action)
                (:option
                 (let ((opt (cadr action)))
                   (condition-case err
                       (setq params (clime-invoke--handle-option opt params))
                     (quit nil)
                     (error
                      (setq error-msg (error-message-string err))))))
                (:arg
                 (let ((arg (cadr action)))
                   (condition-case err
                       (setq params (clime-invoke--handle-arg arg params))
                     (quit nil)
                     (error
                      (setq error-msg (error-message-string err))))))
                (:child
                 (let* ((child-name (cadr action))
                        (child-node (caddr action))
                        (child-path (append path (list child-name))))
                   (let ((child-result
                          (clime-invoke--loop app child-node child-path params)))
                     (setq params (car child-result))
                     (when-let ((child-output (cadr child-result)))
                       ;; Propagate: close parent loop too, show output
                       (setq last-output child-output
                             running nil)))))
                (:run
                 (let* ((run-result (clime-invoke--run-handler app node path params))
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
    (list params last-output)))

;;; ─── Public API ─────────────────────────────────────────────────────

;;;###autoload
(defun clime-invoke (app &optional path params)
  "Open an interactive menu for clime APP.
APP is a `clime-app' struct, or nil to select from registered apps.
PATH is an optional list of strings naming a node to navigate to.
PARAMS is an optional plist of initial param values."
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
  ;; Navigate to starting node
  (let ((node app)
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
    (let (loop-result)
      (unwind-protect
          (setq loop-result
                (clime-invoke--loop app node (or nav-path '())
                                    (or params '()) t))
        ;; Clean up invoke buffer regardless of how we exit
        (when-let ((buf (get-buffer clime-invoke--buffer-name)))
          (let ((win (get-buffer-window buf)))
            (when win (delete-window win)))
          (kill-buffer buf)))
      ;; Show output after invoke buffer is gone
      (let ((final-params (car loop-result))
            (last-output (cadr loop-result)))
        (when last-output
          (clime-invoke--display-output (cdr last-output) (car last-output)))
        final-params))))

;;;###autoload
(defun clime-invoke-command (app command-path)
  "Open an interactive menu for a specific command in APP.
COMMAND-PATH is a list of strings naming the path to the command.
Deprecated: use `clime-invoke' with PATH instead."
  (declare (obsolete clime-invoke "0.3.0"))
  (clime-invoke app command-path))

(provide 'clime-invoke)
;;; clime-invoke.el ends here
