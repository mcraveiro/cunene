;;; yasnippet-bundle.el --- Yet another snippet extension (Auto compiled bundle)
;;; Yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid

;; Authors: pluskid <pluskid@gmail.com>, joaotavora <joaotavora@gmail.com>
;; Version: 0.6.0
;; Package-version: 0.6.0b
;; X-URL: http://code.google.com/p/yasnippet/
;; Keywords: snippet, textmate
;; URL: http://code.google.com/p/yasnippet/
;; EmacsWiki: YaSnippetMode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;; 
;;   1. In your .emacs file:
;;	  (add-to-list 'load-path "/dir/to/yasnippet.el")
;;        (require 'yasnippet)
;;   2. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   3. In your .emacs file
;;        (setq yas/root-directory "~/.emacs/snippets")
;;        (yas/load-directory yas/root-directory)
;;   4. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   5. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 4. and 5. are optional, you don't have to use the minor
;;   mode to use YASnippet.
;;
;;
;;   Major commands are:
;;
;;       M-x yas/expand
;;
;;           Try to expand snippets before point.  In `yas/minor-mode',
;;           this is bound to `yas/trigger-key' which you can customize.
;;
;;       M-x yas/load-directory
;;
;;           Prompts you for a directory hierarchy of snippets to load.
;;
;;       M-x yas/insert-snippet
;;
;;	     Prompts you for possible snippet expansion if that is
;;	     possible according to buffer-local and snippet-local
;;	     expansion conditions.  With prefix argument, ignore these
;;	     conditions.
;;
;;       M-x yas/find-snippets
;;
;;           Lets you find the snippet file in the directory the
;;           snippet was loaded from (if it exists) like
;;           `find-file-other-window'.
;;
;;       M-x yas/visit-snippet-file
;;
;;           Prompts you for possible snippet expansions like
;;           `yas/insert-snippet', but instead of expanding it, takes
;;           you directly to the snippet definition's file, if it
;;           exists.
;;
;;       M-x yas/load-snippet-buffer
;;
;;           When editing a snippet, this loads the snippet.  This is
;;           bound to "C-c C-c" while in the `snippet-mode' editing
;;           mode.
;;
;;       M-x yas/tryout-snippet
;;
;;	     When editing a snippet, this opens a new empty buffer,
;;	     sets it to the appropriate major mode and inserts the
;;	     snippet there, so you can see what it looks like.  This is
;;	     bound to "C-c C-t" while in `snippet-mode'.
;;
;;   The `dropdown-list.el' extension is bundled with YASnippet, you
;;   can optionally use it the preferred "prompting method", puting in
;;   your .emacs file, for example:
;;
;;       (require 'dropdown-list)
;;       (setq 'yas/prompt-functions '(yas/dropdown-prompt
;;				       yas/ido-prompt
;;				       yas/completing-prompt))
;;
;;   Also check out the customization group
;;   
;;        M-x customize-group RET yasnippet RET
;;
;;   For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(require 'cl)
(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;

(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :group 'editing)

(defcustom yas/root-directory nil
  "Root directory that stores the snippets for each major mode.

Can also be a list of strings, for multiple root directories."
  :type '(string)
  :group 'yasnippet)

(defcustom yas/prompt-functions '(yas/x-prompt
				  yas/dropdown-prompt
				  yas/completing-prompt
				  yas/ido-prompt
				  yas/no-prompt)
  "Functions to prompt for keys, templates, etc interactively."
  :type 'list
  :group 'yasnippet)

(defcustom yas/indent-line 'auto
  "Controls indenting applied to a recent snippet expansion.

The following values are possible:

`fixed' Indent the snippet to the current column;

`auto' Indent each line of the snippet with `indent-according-to-mode'

Every other value means don't apply any snippet-side indendation
after expansion (the manual per-line \"$>\" indentation still
applies)."
  :type '(choice (const :tag "Nothing"  nothing)
                 (const :tag "Fixed"    fixed)
                 (const :tag "Auto"     auto))
  :group 'yasnippet)

(defcustom yas/snippet-revival t
  "Non-nil means re-activate snippet fields after undo/redo."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-key "TAB"
  "The key bound to `yas/expand' when function `yas/minor-mode' is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/next-field-key "TAB"
  "The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/prev-field-key '("<backtab>" "<S-tab>")
  "The key to navigate to previous field when a snippet is active.

Can also be a list of keys.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/skip-and-clear-key "C-d"
  "The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet)

(defcustom yas/triggers-in-field nil
  "If non-nil, `yas/next-field-key' can trigger stacked expansions.

Otherwise, `yas/next-field-key' just tries to move on to the next
field"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/fallback-behavior 'call-other-command
  "How to act when `yas/trigger-key' does *not* expand a snippet.

The fall back behavior of YASnippet when it can't find a snippet
to expand.

`call-other-command' means try to temporarily disable
    YASnippet and call other command bound to `yas/trigger-key'.

`return-nil' means return do nothing."
  :type '(choice (const :tag "Call previous command"  'call-other-command)
                 (const :tag "Do nothing"    'return-nil))
  :group 'yasnippet)

(defcustom yas/choose-keys-first t
  "If non-nil, `yas/insert-snippet' prompts for key, then for template.

Otherwise `yas/insert-snippet' prompts for all possible
templates and inserts the selected one."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/use-menu t
  "Display a YASnippet menu in the menu bar.

If this is set to t, all snippet template of the current
mode will be listed under the menu \"yasnippet\"."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger."
  :type 'string
  :group 'yasnippet)

(defcustom yas/show-all-modes-in-menu nil
  "Display \"artificial\" major modes in menu bar as well.

Currently, YASnippet only all \"real modes\" to menubar.  For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'.  There's really
no such mode like \"cc-mode\".  So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes.  The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/wrap-around-region nil
  "If non-nil, snippet expansion wraps around selected region.

The wrapping occurs just before the snippet's exit marker.  This
can be overriden on a per-snippet basis."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/good-grace t
  "If non-nil, don't raise errors in inline elisp evaluation.

An error string \"[yas] error\" is returned instead."


  :type 'boolean
  :group 'yasnippet)

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight the currently active field of a snippet"
  :group 'yasnippet)

(defface yas/field-debug-face
  '()
  "The face used for debugging some overlays normally hidden"
  :group 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User semi-customizable variables
;;

(defvar yas/keymap (make-sparse-keymap)
  "The keymap active while a snippet expansion is in progress.")

(defun yas/define-some-keys (keys keymap definition)
  "Bind KEYS to DEFINITION in KEYMAP, read with `read-kbd-macro'."
  (let ((keys (or (and (listp keys) keys)
		  (list keys))))
    (dolist (key keys)
      (define-key keymap (read-kbd-macro key) definition))))

(eval-when-compile
  (yas/define-some-keys yas/next-field-key yas/keymap 'yas/next-field-or-maybe-expand)
  (yas/define-some-keys yas/prev-field-key yas/keymap 'yas/prev-field)
  (yas/define-some-keys yas/skip-and-clear-key yas/keymap 'yas/skip-and-clear-or-delete-char))

(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.

The hooks will be run in an environment where some variables bound to
proper values:

`yas/snippet-beg' : The beginning of the region of the snippet.

`yas/snippet-end' : Similar to beg.

Attention: These hooks are not run when exiting nested/stackd snippet expansion!")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run just before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (not (bobp))
            (or (equal 'font-lock-comment-face
                       (get-char-property (1- (point))
                                          'face))
                (equal 'font-lock-string-face
                       (get-char-property (1- (point))
                                          'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Condition to yasnippet local to each buffer.

The default value helps filtering out potential snippet
expansions inside comments and string literals, unless the
snippet itself contains a condition that returns the symbol
`force-in-comment'.

    * If yas/buffer-local-condition evaluate to nil, snippet
      won't be expanded.

    * If it evaluate to the a cons cell where the car is the
      symbol `require-snippet-condition' and the cdr is a
      symbol (let's call it \"requirement\"):
       * If the snippet has no condition, then it won't be
         expanded.
       * If the snippet has a condition but it evaluates to nil or
         error occured during evaluation, it won't be expanded.
       * If the snippet has a condition that evaluate to
         non-nil (let's call it \"result\"):
          * If \"requirement\" is t, the snippet is ready to be
            expanded.
          * If \"requirement\" is eq to \"result\", the snippet is ready
            to be expanded.
          * Otherwise the snippet won't be expanded.

    * If it evaluates to `always', snippet is unconditionally
      expanded.

    * If it evaluates to other non-nil value:
       * If the snippet has no condition, or has a condition that
         evaluate to non-nil, it is ready to be expanded.
       * Otherwise, it won't be expanded.

Here's an example:

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))")
(make-variable-buffer-local 'yas/buffer-local-condition)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;; 

(defvar yas/version "0.6.0b")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major mode.")

(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major mode.")

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")

(defvar yas/escaped-characters
  '(?\\ ?` ?' ?$ ?} )
  "List of characters which *might* need to be escaped.")

(defconst yas/field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field.")

(defconst yas/multi-dollar-lisp-expression-regexp
  "$+[ \t\n]*\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression.")

(defconst yas/backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"`lisp-expression`\" expression." )

(defconst yas/transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform.")

(defconst yas/simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror.")

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode stuff
;;
;; TODO: XXX: This is somehow needed in Carbon Emacs for MacOSX 
(defvar last-buffer-undo-list nil)

(defvar yas/minor-mode-map (make-sparse-keymap)
  "The keymap used when function `yas/minor-mode' is active.")

(defun yas/init-keymap-and-menu ()
  (easy-menu-define yas/minor-mode-menu
    yas/minor-mode-map
    "Menu used when YAS/minor-mode is active."
    (cons "YASnippet"
	  (mapcar #'(lambda (ent)
		      (when (third ent)
			(define-key yas/minor-mode-map (third ent) (second ent)))
		      (vector (first ent) (second ent) t))
		  (list (list "--")
			(list "Expand trigger" 'yas/expand (read-kbd-macro yas/trigger-key))
			(list "Insert at point..." 'yas/insert-snippet "\C-c&\C-s")
			(list "Visit snippet file..." 'yas/visit-snippet-file "\C-c&\C-v")
			(list "Find snippets..." 'yas/find-snippets "\C-c&\C-f")
			(list "About" 'yas/about)
			(list "Reload-all-snippets" 'yas/reload-all)
			(list "Load snippets..." 'yas/load-directory))))))

(eval-when-compile
  (yas/init-keymap-and-menu))

(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}"
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'yasnippet)

(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode."
  (interactive)
  (yas/minor-mode 1))

(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))

(define-globalized-minor-mode yas/global-mode yas/minor-mode yas/minor-mode-on
  :group 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode stuff
;;
(defvar yas/font-lock-keywords
  (append '(("^#.*$" . font-lock-comment-face))
	  lisp-font-lock-keywords
	  lisp-font-lock-keywords-1
	  lisp-font-lock-keywords-2
	  '(("$\\([0-9]+\\)"
	     (0 font-lock-keyword-face)
	     (1 font-lock-string-face t))
	    ("${\\([0-9]+\\):?"
	     (0 font-lock-keyword-face)
	     (1 font-lock-warning-face t))
	    ("${" font-lock-keyword-face)
	    ("$[0-9]+?" font-lock-preprocessor-face)
	    ("\\(\\$(\\)" 1 font-lock-preprocessor-face)
	    ("}"
	     (0 font-lock-keyword-face)))))

(defvar snippet-mode-map (make-sparse-keymap))
(define-key snippet-mode-map "\C-c\C-c" 'yas/load-snippet-buffer)
(define-key snippet-mode-map "\C-c\C-t" 'yas/tryout-snippet)


(define-derived-mode snippet-mode text-mode "YASnippet"
  "A mode for editing yasnippets"
  (setq font-lock-defaults '(yas/font-lock-keywords))
  (set (make-local-variable 'require-final-newline) nil)
  (use-local-map snippet-mode-map))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal structs for template management
;; 

(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition env file)))
  "A template for a snippet."
  content
  name
  condition
  env
  file)

(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (default-directory nil)
  (parent nil))

(defun yas/template-condition-predicate (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition))))
    (error (progn
             (message (format "[yas]error in condition evaluation: %s"
                              (error-message-string err)))
             nil))))


(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the applicable condition.

TEMPLATES is a list of cons (KEY . TEMPLATE) where KEY is a
string and TEMPLATE is a `yas/template' structure.

This function implements the rules described in
`yas/buffer-local-condition'.  See that variables documentation."
  (let ((requirement (yas/require-template-specific-condition-p)))
    (if (eq requirement 'always)
	templates
      (remove-if-not #'(lambda (pair)
			 (let* ((condition (yas/template-condition (cdr pair)))
				(result (and condition
					     (yas/template-condition-predicate condition))))
			   (cond ((eq requirement t)
				  result)
				 (t
				  (eq requirement result)))))
		     templates))))

(defun yas/snippet-table-fetch (table key)
  "Fetch a snippet binding to KEY from TABLE. If not found,
fetch from parent if any."
  (when table
    (let* ((unfiltered (gethash key (yas/snippet-table-hash table)))
	   (templates  (yas/filter-templates-by-condition unfiltered)))
      (when (and (null templates)
		 (not (null (yas/snippet-table-parent table))))
	(setq templates (yas/snippet-table-fetch
			 (yas/snippet-table-parent table)
			 key)))
      templates)))

(defun yas/snippet-table-all-templates (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
		   (setq acc (append acc templates)))
	       (yas/snippet-table-hash table))
      (append (yas/filter-templates-by-condition acc)
	      (yas/snippet-table-all-templates (yas/snippet-table-parent table))))))

(defun yas/snippet-table-all-keys (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
		   (when (yas/filter-templates-by-condition templates)
		     (push key acc)))
	       (yas/snippet-table-hash table))
      (append acc
	      (yas/snippet-table-all-keys (yas/snippet-table-parent table))))))

(defun yas/snippet-table-store (table full-key key template)
  "Store a snippet template in the TABLE."
  (puthash key
           (yas/modify-alist (gethash key
                                      (yas/snippet-table-hash table))
                             full-key
                             template)
           (yas/snippet-table-hash table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;; 

(defun yas/ensure-minor-mode-priority ()
  "Ensure that the key binding of yas/minor-mode takes priority."
  (unless (eq 'yas/minor-mode
              (caar minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons
           (cons 'yas/minor-mode yas/minor-mode-map)
           (assq-delete-all 'yas/minor-mode
                            minor-mode-map-alist)))))

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/eval-string (string)
  ;; TODO: This is a possible optimization point, the expression could
  ;; be stored in cons format instead of string,
  "Evaluate STRING and convert the result to string."
  (let ((retval (catch 'yas/exception
		  (condition-case err
		      (save-excursion
			(save-restriction
			  (save-match-data
			    (widen)
			    (let ((result (eval (read string))))
			      (when result
				(format "%s" result))))))
		    (error (if yas/good-grace
			       "[yas] elisp error!"
			     (error (format "[yas] elisp error: %s"
					    (error-message-string err)))))))))
    (when (and (consp retval)
	       (eq 'yas/exception (car retval)))
      (error (cdr retval)))
    retval))

(defun yas/snippet-table-get-create (mode &optional directory)
  "Get the snippet table corresponding to MODE.

Optional DIRECTORY gets recorded as the default directory to
search for snippet files if the retrieved/created table didn't
already have such a property."
  (let ((table (gethash mode
			yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table))
      (puthash mode table yas/snippet-tables))
    (unless (or (not directory) (yas/snippet-table-default-directory table))
      (setf (yas/snippet-table-default-directory table)
	    directory))
    table))

(defun yas/current-snippet-table (&optional mode-symbol dont-search-parents)
  "Get the snippet table for current major-mode."
  (let ((mode (or mode-symbol
		  major-mode)))
    (or (gethash mode
		 yas/snippet-tables)
	(and (not dont-search-parents)
	     (get mode 'derived-mode-parent)
	     (yas/current-snippet-table (get mode 'derived-mode-parent))))))

(defun yas/menu-keymap-for-mode (mode)
  "Get the menu keymap correspondong to MODE."
  (let ((keymap (gethash mode yas/menu-table)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (puthash mode
	       keymap yas/menu-table))
    keymap))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end (point))
        (syntaxes yas/key-syntaxes)
        syntax done templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point)))
      (setq templates
            (yas/snippet-table-fetch
             (yas/current-snippet-table)
             (buffer-substring-no-properties start end)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template-related and snippet loading functions

(defun yas/parse-template (&optional file)
  "Parse the template in the current buffer.

Optional FILE is the absolute file name of the file being
parsed.

Return a snippet-definition, i.e. a list

 (KEY TEMPLATE NAME CONDITION GROUP ENV)

If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Here's a list of currently recognized variables:

 * name
 * contributor
 * condition
 * key
 * group
 * env

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let* ((name (and file (file-name-nondirectory file)))
	 (key name)
	 template
	 bound
	 condition
	 group
	 env)
    (if (re-search-forward "^# --\n" nil t)
	(progn (setq template
		     (buffer-substring-no-properties (point)
						     (point-max)))
	       (setq bound (point))
	       (goto-char (point-min))
	       (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
		 (when (string= "name" (match-string-no-properties 1))
		   (setq name (match-string-no-properties 2)))
		 (when (string= "condition" (match-string-no-properties 1))
		   (setq condition (read (match-string-no-properties 2))))
		 (when (string= "group" (match-string-no-properties 1))
		   (setq group (match-string-no-properties 2)))
		 (when (string= "env" (match-string-no-properties 1))
		   (setq env (match-string-no-properties 2)))
		 (when (string= "key" (match-string-no-properties 1))
		   (setq key (match-string-no-properties 2)))))
      (setq template
	    (buffer-substring-no-properties (point-min) (point-max))))
    (list key template name condition group env file)))

(defun yas/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
		   (string-match "~$"
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  `(lambda () (interactive) (yas/expand-from-menu ,template)))

(defun yas/expand-from-menu (template)
  (let ((where (if mark-active
		   (cons (region-beginning) (region-end))
		 (cons (point) (point)))))
    (yas/expand-snippet (car where)
			(cdr where)
			(yas/template-content template))))

(defun yas/modify-alist (alist key value)
  "Modify ALIST to map KEY to VALUE. return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
        (cons (cons key value)
              alist)
      (setcdr pair value)
      alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popping up for keys and templates
;; 
(defun yas/prompt-for-template (templates &optional prompt)
  "Interactively choose a template from the list TEMPLATES.

TEMPLATES is a list of `yas/template'."
  (let ((template (some #'(lambda (fn)
			    (funcall fn (or prompt "Choose a snippet: ")
				     templates #'(lambda (template)
						   (yas/template-name template))))
			yas/prompt-functions)))
    template))

(defun yas/prompt-for-keys (keys &optional prompt)
  "Interactively choose a template key from the list KEYS."
  (if keys
      (some #'(lambda (fn)
		(funcall fn (or prompt "Choose a snippet key: ") keys))
	    yas/prompt-functions)))

(defun yas/x-prompt (prompt choices &optional display-fn)
  (when (and window-system choices)
    (let ((keymap (cons 'keymap
			(cons
			 prompt
			 (mapcar (lambda (choice)
				   (list choice
					 'menu-item
					 (if display-fn
					     (funcall display-fn choice)
					   choice)
					 t))
				 choices)))))
      (when (cdr keymap)
	(car (x-popup-menu (if (fboundp 'posn-at-point)
			       (let ((x-y (posn-x-y (posn-at-point (point)))))
				 (list (list (+ (car x-y) 10)
					     (+ (cdr x-y) 20))
				       (selected-window)))
			     t)
			   keymap))))))

(defun yas/ido-prompt (prompt choices &optional display-fn)
  (when (and (featurep 'ido)
	     ido-mode)
    (let* ((formatted-choices (or (and display-fn
				       (mapcar display-fn choices))
				  choices))
	   (chosen (and formatted-choices
			(ido-completing-read prompt
					     formatted-choices
					     nil
					     'require-match
					     nil
					     nil))))
      (when chosen
	(nth (position chosen formatted-choices) choices)))))

(defun yas/dropdown-prompt (prompt choices &optional display-fn)
  (when (featurep 'dropdown-list)
    (let* ((formatted-choices (or (and display-fn
				       (mapcar display-fn choices))
				  choices))
	   (chosen (and formatted-choices
			(nth (dropdown-list formatted-choices)
			     choices))))
      chosen)))

(defun yas/completing-prompt (prompt choices &optional display-fn)
  (let* ((formatted-choices (or (and display-fn
				     (mapcar display-fn choices))
				choices))
	 (chosen (and formatted-choices
		      (completing-read prompt
				       formatted-choices
				       nil
				       'require-match
				       nil
				       nil))))
    (when chosen
      (nth (position chosen formatted-choices) choices))))

(defun yas/no-prompt (prompt choices &optional display-fn)
  (first choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading snippets from files
;; 
(defun yas/load-directory-1 (directory &optional parent)
  
  "Recursively load snippet templates from DIRECTORY."

  (let ((mode-sym (intern (file-name-nondirectory directory)))
        (snippet-defs nil))
    (with-temp-buffer
      (dolist (file (yas/subdirs directory 'no-subdirs-just-files))
        (when (file-readable-p file)
          (insert-file-contents file nil nil nil t)
          (push (yas/parse-template file)
		snippet-defs))))
    (yas/define-snippets mode-sym
                         snippet-defs
                         parent
			 directory)
    (dolist (subdir (yas/subdirs directory))
      (yas/load-directory-1 subdir mode-sym))))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.

Below the top-level directory, each directory is a mode
name.  And under each subdirectory, each file is a definition
of a snippet.  The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless (file-directory-p directory)
    (error "Error %s not a directory" directory))
  (add-to-list 'yas/root-directory directory)
  (dolist (dir (yas/subdirs directory))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "done.")))

(defun yas/reload-all ()
  "Reload all snippets and rebuild the YASnippet menu. "

  (interactive)
  (let ((restore-global-mode nil)
	(restore-minor-mode nil))
    (setq yas/snippet-tables (make-hash-table))
    (setq yas/menu-table (make-hash-table))
    (setf (cdr yas/minor-mode-menu) nil)
    (setf (cdr yas/minor-mode-map) nil)
    (when yas/global-mode
      (yas/global-mode -1)
      (setq restore-global-mode t))

    (when yas/minor-mode
      (yas/minor-mode -1)
      (setq restore-minor-mode t))

    (yas/init-keymap-and-menu)

    (if yas/root-directory
	(if (listp yas/root-directory)
	    (dolist (directory yas/root-directory)
	      (yas/load-directory directory))
	  (yas/load-directory yas/root-directory))
      (call-interactively 'yas/load-directory))


    (when restore-minor-mode
      (yas/minor-mode 1))

    (when restore-global-mode
      (yas/global-mode 1))

    (message "done.")))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnipept Bundle

(defun yas/initialize ()
  "For backward compatibility, enable `yas/minor-mode' globally"
  (yas/global-mode 1))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code dropdown)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.
SNIPPET-ROOTS is a list of root directories that contains the
snippets definition. YASNIPPET is the yasnippet.el file
path. YASNIPPET-BUNDLE is the output file of the compile
result. CODE is the code you would like to used to initialize
yasnippet. Last optional argument DROPDOWN is the filename of the
dropdown-list.el library.

Here's the default value for all the parameters:

 (yas/compile-bundle \"yasnippet.el\"
                     \"./yasnippet-bundle.el\"
                     '(\"snippets\")
                     \"(yas/initialize)\")

..

"
  (when (null yasnippet)
    (setq yasnippet "yasnippet.el"))
  (when (null yasnippet-bundle)
    (setq yasnippet-bundle "./yasnippet-bundle.el"))
  (when (null snippet-roots)
    (setq snippet-roots '("snippets")))
  (when (null dropdown)
    (setq dropdown "dropdown-list.el"))
  (when (null code)
    (setq code (concat "(yas/initialize-bundle)"
		       "\n;;;###autoload" ; break through so that won't
		       "(require 'yasnippet-bundle)"))) ; be treated as magic comment

  (let ((dirs (or (and (listp snippet-roots) snippet-roots)
                  (list snippet-roots)))
        (bundle-buffer nil))
    (with-temp-buffer
      (setq bundle-buffer (current-buffer))
      (insert ";;; yasnippet-bundle.el --- "
              "Yet another snippet extension (Auto compiled bundle)\n")
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert "\n")
      (when dropdown
	(insert-file-contents dropdown))
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\""
              "  (yas/global-mode 1)\n")
      (flet ((yas/define-snippets
              (mode snippets &optional parent directory)
              (with-current-buffer bundle-buffer
                (insert ";;; snippets for " (symbol-name mode) "\n")
                (insert "(yas/define-snippets '" (symbol-name mode) "\n")
                (insert "'(\n")
                (dolist (snippet snippets)
                  (insert "  ("
                          (yas/quote-string (car snippet))
                          " "
                          (yas/quote-string (nth 1 snippet))
                          " "
                          (if (nth 2 snippet)
                              (yas/quote-string (nth 2 snippet))
                            "nil")
                          " "
                          (if (nth 3 snippet)
                              (format "'%s" (nth 3 snippet))
                            "nil")
                          " "
                          (if (nth 4 snippet)
                              (yas/quote-string (nth 4 snippet))
                            "nil")
                          ")\n"))
                (insert "  )\n")
                (insert (if parent
                            (concat "'" (symbol-name parent))
                          "nil")
			;; (if directory
                        ;;     (concat "\"" directory "\"")
                        ;;   "nil")
                        ")\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/subdirs dir))
            (yas/load-directory-1 subdir nil))))

      (insert ")\n\n" code "\n")
      (insert "(provide '"
              (file-name-nondirectory
               (file-name-sans-extension
                yasnippet-bundle))
              ")\n")
      (insert ";;; "
              (file-name-nondirectory yasnippet-bundle)
              " ends here\n")
      (setq buffer-file-name yasnippet-bundle)
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User level functions
;;; 

(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>/joaotavora <joaotavora@gmail.com>")))

(defun yas/define-snippets (mode snippets &optional parent-mode directory)
  "Define snippets for MODE.  SNIPPETS is a list of
snippet definitions, each taking the following form:

 (KEY TEMPLATE NAME CONDITION GROUP)

NAME, CONDITION or GROUP may be omitted.  Optional PARENT-MODE
can be used to specify the parent mode of MODE.  That is, when
looking a snippet in MODE failed, it can refer to its parent
mode.  The PARENT-MODE does not need to be a real mode.

Optional DIRECTORY is recorded in the `yas/snippet-table' if it
is created for the first time. Then, it becomes the default
directory to find snippet files.


"
  (let ((snippet-table (yas/snippet-table-get-create mode directory))
        (parent-table (if parent-mode
                          (yas/snippet-table-get-create parent-mode)
                        nil))
        (keymap (if yas/use-menu
                    (yas/menu-keymap-for-mode mode)
                  nil)))
    (when parent-table
      (setf (yas/snippet-table-parent snippet-table)
            parent-table)
      (when yas/use-menu
        (define-key keymap (vector 'parent-mode)
          `(menu-item "parent mode"
                      ,(yas/menu-keymap-for-mode parent-mode)))))
    (when (and yas/use-menu
               (yas/real-mode? mode))
      (define-key yas/minor-mode-menu (vector mode)
        `(menu-item ,(symbol-name mode) ,keymap)))
    (dolist (snippet snippets)
      (let* ((full-key (car snippet))
             (key (file-name-sans-extension full-key))
             (name (or (nth 2 snippet) (file-name-extension full-key)))
             (condition (nth 3 snippet))
             (group (nth 4 snippet))
	     (template (yas/make-template (nth 1 snippet)
                                          (or name key)
                                          condition
					  (nth 5 snippet)
					  (nth 6 snippet))))
        (yas/snippet-table-store snippet-table
                                 full-key
                                 key
                                 template)
        (when yas/use-menu
          (let ((group-keymap keymap))
	    ;; delete this entry from another group if already exists
	    ;; in some other group. An entry is considered as existing
	    ;; in another group if its name string-matches.
	    (yas/delete-from-keymap group-keymap name)
  
	    ;; ... then add this entry to the correct group
            (when (and (not (null group))
                       (not (string= "" group)))
              (dolist (subgroup (mapcar #'make-symbol
                                        (split-string group "\\.")))
                (let ((subgroup-keymap (lookup-key group-keymap
                                                   (vector subgroup))))
                  (when (null subgroup-keymap)
                    (setq subgroup-keymap (make-sparse-keymap))
                    (define-key group-keymap (vector subgroup)
                      `(menu-item ,(symbol-name subgroup)
                                  ,subgroup-keymap)))
                  (setq group-keymap subgroup-keymap))))
            (define-key group-keymap (vector (make-symbol full-key))
              `(menu-item ,(yas/template-name template)
                          ,(yas/make-menu-binding template)
                          :keys ,(concat key yas/trigger-symbol)))))))))

(defun yas/delete-from-keymap (keymap name)
  "Recursively delete items name NAME from KEYMAP and its submenus.

Skip any submenus named \"parent mode\""
  ;; First of all, r ecursively enter submenus, i.e. the tree is
  ;; searched depth first so that stale submenus can be found in the
  ;; higher passes.
  ;; 
  (mapc #'(lambda (item)
	    (when (and (keymapp (fourth item))
		       (stringp (third item))
		       (not (string= (third item)
				     "parent mode")))
	      (yas/delete-from-keymap (fourth item) name)))
	(rest keymap))
  ;;
  (when (keymapp keymap)
    (let ((pos-in-keymap))
      (while (setq pos-in-keymap (position-if #'(lambda (item)
						  (and (listp item)
						       (or
							;; the menu item we want to delete
							(and (eq 'menu-item (second item))
							     (third item)
							     (and (string= (third item) name)))
							;; a stale subgroup
							(and (keymapp (fourth item))
							     (null (rest (fourth item)))))))
					      keymap))
	(setf (nthcdr pos-in-keymap keymap)
	      (nthcdr (+ 1 pos-in-keymap) keymap))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
         (yas/snippet-table-get-create mode))
        (yas/snippet-table-get-create parent))
  (when yas/use-menu
    (define-key (yas/menu-keymap-for-mode mode) (vector 'parent-mode)
      `(menu-item "parent mode"
                  ,(yas/menu-keymap-for-mode parent)))))

(defun yas/define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.
NAME is a description to this template.  Also update
the menu if `yas/use-menu' is `t'.  CONDITION is the
condition attached to this snippet.  If you attach a
condition to a snippet, then it will only be expanded
when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition group))))

(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (undo 1)
    nil))

(defun yas/require-template-specific-condition-p ()
  "Decides if this buffer requests/requires snippet-specific
conditions to filter out potential expansions."
  (if (eq 'always yas/buffer-local-condition)
      'always
    (let ((local-condition (yas/template-condition-predicate
			    yas/buffer-local-condition)))
      (and local-condition
	   (consp local-condition)
	   (eq 'require-snippet-condition (car local-condition))
	   (symbolp (cdr local-condition))
	   (cdr local-condition)))))

(defun yas/expand (&optional field)
  "Expand a snippet."
  (interactive)
  (multiple-value-bind (templates start end) (if field
						 (save-restriction
						   (narrow-to-region (yas/field-start field) (yas/field-end field))
						   (yas/current-key))
					       (yas/current-key))
    (if templates
	(let ((template (or (and (rest templates) ;; more than one
				 (yas/prompt-for-template (mapcar #'cdr templates)))
			    (cdar templates))))
	  (when template
	    (yas/expand-snippet start
				end
				(yas/template-content template)
				(yas/template-env template))))
      (if (eq yas/fallback-behavior 'return-nil)
	  nil				; return nil
	(let* ((yas/minor-mode nil)
	       (command (key-binding (read-kbd-macro yas/trigger-key))))
	  (when (commandp command)
	    (call-interactively command)))))))

(defun yas/insert-snippet (&optional no-condition)
  "Choose a snippet to expand, pop-up a list of choices according
to `yas/prompt-function'.

With prefix argument NO-CONDITION, bypass filtering of snippets
by condition."
  (interactive "P")
  (let* ((yas/buffer-local-condition (or (and no-condition
					      'always)
					 yas/buffer-local-condition))
	 (templates (mapcar #'cdr
			    (if yas/choose-keys-first
				(let ((key (yas/prompt-for-keys (yas/snippet-table-all-keys (yas/current-snippet-table)))))
				  (when key
				    (yas/snippet-table-fetch (yas/current-snippet-table) key)))
			      (yas/snippet-table-all-templates (yas/current-snippet-table)))))
	 (template (and templates
			(or (and (rest templates) ;; more than one template for same key
				 (yas/prompt-for-template templates))
			    (car templates))))
	 (where (if mark-active
		    (cons (region-beginning) (region-end))
		  (cons (point) (point)))))
    (if template
	(yas/expand-snippet (car where)
			    (cdr where)
			    (yas/template-content template)
			    (yas/template-env template))
      (message "[yas] No snippets can be inserted here!"))))

(defun yas/visit-snippet-file ()
  "Choose a snippet to edit, selection like `yas/insert-snippet'.

Only success if selected snippet was loaded from a file.  Put the
visited file in `snippet-mode'."
  (interactive)
  (let* ((yas/buffer-local-condition 'always)
	 (templates (mapcar #'cdr
			    (if yas/choose-keys-first
				(let ((key (yas/prompt-for-keys (yas/snippet-table-all-keys (yas/current-snippet-table))
								"Choose a snippet key to edit: ")))
				  (when key
				    (yas/snippet-table-fetch (yas/current-snippet-table) key)))
			      (yas/snippet-table-all-templates (yas/current-snippet-table)))))
	 (template (and templates
			(or (and (rest templates) ;; more than one template for same key
				 (yas/prompt-for-template templates
							  "Choose a snippet template to edit: "))
			    (car templates)))))

    (when template
      (let ((file (yas/template-file template)))
	(cond ((and file (file-exists-p file))
	       (find-file-other-window file)
	       (snippet-mode))
	      (file
	       (message "Original file %s no longer exists!" file))
	      (t
	       (message "This snippet was not loaded from a file!")))))))

(defun yas/guess-snippet-directory ()
  "Try to guess the suitable yassnippet based on `major-mode'"
  (let ((loaded-root (or (and (listp yas/root-directory)
			      (first yas/root-directory))
			 yas/root-directory))
	(mode major-mode)
	(path))
    (when loaded-root
      (while mode
	(setq path (format "%s/%s"
			   mode
			   (or path
			       "")))
	(setq mode (get mode 'derived-mode-parent)))
      (concat loaded-root
	      (unless (string-match "/$" loaded-root) "/")
	      path))))


(defun yas/find-snippets (&optional same-window)
  "Looks for snippets file in the current mode's directory.

This can be used to create new snippets for the currently active
major mode."
  (interactive "P")
  (let* ((current-table (yas/current-snippet-table major-mode 'dont-search-parents))
	 (parents-table (yas/current-snippet-table major-mode))
	 (parents-directory (and parents-table
				 (yas/snippet-table-default-directory parents-table)))
	 (guessed-directory (or (and current-table
				     (yas/snippet-table-default-directory current-table))
				(yas/guess-snippet-directory)
				default-directory))
	 (buffer))
    (unless (file-exists-p guessed-directory)
      (if (y-or-n-p (format "Guessed directory (%s) does not exist! Create? " guessed-directory))
	  (make-directory guessed-directory 'also-make-parents)
	(if parents-directory
	    (setq guessed-directory parents-directory)
	  (setq guessed-directory default-directory))))
    (let ((default-directory guessed-directory))
      (setq buffer (call-interactively (if same-window
					   'find-file
					 'find-file-other-window)))
      (when buffer
	(save-excursion
	  (set-buffer buffer)
	  (when (eq major-mode 'fundamental-mode)
	    (snippet-mode)))))))


(defun yas/compute-major-mode-and-parent (file &optional prompt-if-failed)
  (let* ((file-dir (and file
			(directory-file-name (file-name-directory file))))
	 (major-mode-name (and file-dir
			       (file-name-nondirectory file-dir)))
	 (parent-file-dir (and file-dir
			       (directory-file-name (file-name-directory file-dir))))
	 (parent-mode-name (and parent-file-dir
				(file-name-nondirectory parent-file-dir)))
	 (major-mode-sym (or (and major-mode-name
				  (intern major-mode-name))
			     (when prompt-if-failed
			       (read-from-minibuffer "[yas] Cannot auto-detect major mode! Enter a major mode: "))))
	 (parent-mode-sym (and parent-mode-name
			       (intern parent-mode-name))))
    (if (fboundp major-mode-sym)
	(cons major-mode-sym
	      (when (fboundp parent-mode-sym)
		parent-mode-sym)))))

(defun yas/load-snippet-buffer (&optional kill)
  "Parse and load current buffer's snippet definition.

With optional prefix argument KILL quit the window and buffer."
  (interactive "P")
  (if buffer-file-name
      (let ((major-mode-and-parent (yas/compute-major-mode-and-parent buffer-file-name)))
	(if major-mode-and-parent
	    (let* ((parsed (yas/parse-template buffer-file-name))
		   (name (and parsed
			      (third parsed))))
	      (when name
		(yas/define-snippets (car major-mode-and-parent)
				     (list parsed)
				     (cdr major-mode-and-parent))
		(when (and (buffer-modified-p)
			   (y-or-n-p "Save snippet? "))
		  (save-buffer))
		(if kill
		    (quit-window kill)
		  (message "[yas] Snippet \"%s\" loaded for %s." name (car major-mode-and-parent)))))
	  (message "[yas] Cannot load snippet for unknown major mode")))
    (message "Save the buffer as a file first!")))

(defun yas/tryout-snippet (&optional debug)
  "Test current buffers's snippet template in other buffer."
  (interactive "P")
  (let* ((major-mode-and-parent (yas/compute-major-mode-and-parent buffer-file-name))
	 (parsed (and major-mode-and-parent
		      (fboundp (car major-mode-and-parent))
		      (yas/parse-template (symbol-name (car major-mode-and-parent)))))
	 (template (and parsed
			(yas/make-template (second parsed) (third parsed) nil (sixth parsed) nil))))
    (cond (template
	   (let ((buffer-name (format "*YAS TEST: %s*" (yas/template-name template))))
	     (set-buffer (switch-to-buffer buffer-name))
	     (erase-buffer)
	     (setq buffer-undo-list nil)
	     (funcall (car major-mode-and-parent))
	     (yas/expand-snippet (point-min) (point-max) (yas/template-content template) (yas/template-env template))
	     (when debug
	       (add-hook 'post-command-hook 'yas/debug-some-vars 't 'local))))
	  (t
	   (message "[yas] Coulnd not parse template!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User convenience functions, for using in snippet definitions
;;;

(defun yas/substr (str pattern &optional subexp)
  "Search PATTERN in STR and return SUBEXPth match.

If found, the content of subexp group SUBEXP (default 0) is
  returned, or else the original STR will be returned."
  (let ((grp (or subexp 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

(defun yas/choose-value (possibilities)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
	      yas/modified-p)
    (some #'(lambda (fn)
	      (funcall fn "Choose: " possibilities))
	  yas/prompt-functions)))

(defun yas/key-to-value (alist)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
	      yas/modified-p)
    (let ((key (read-key-sequence "")))
      (when (stringp key)
	(or (cdr (find key alist :key #'car :test #'string=))
	    key)))))

(defun yas/throw (text)
  "Throw a yas/exception with TEXT as the reason."
  (throw 'yas/exception (cons 'yas/exception text)))

(defun yas/verify-value (possibilities)
  "Verify that the current field value is in POSSIBILITIES

Otherwise throw exception."
  (when (and yas/moving-away-p (notany #'(lambda (pos) (string= pos yas/text)) possibilities))
    (yas/throw (format "[yas] field only allows %s" possibilities))))

(defun yas/field-value (number)
  (let* ((snippet (car (yas/snippets-at-point)))
	 (field (and snippet
		     (yas/snippet-find-field snippet number))))
    (when field
      (yas/field-text-for-display field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippet expansion and field management

(defvar yas/active-field-overlay nil
  "Overlays the currently active field.")

(defvar yas/field-protection-overlays nil
  "Two overlays protect the current active field ")

(defvar yas/deleted-text nil
  "The text deleted in the last snippet expansion.")

(defvar yas/selected-text nil
  "The selected region deleted on the last snippet expansion.")

(defvar yas/start-column nil
  "The column where the snippet expansion started.")

(make-variable-buffer-local 'yas/active-field-overlay)
(make-variable-buffer-local 'yas/field-protection-overlays)
(make-variable-buffer-local 'yas/deleted-text)

(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

..."
  (fields '())
  (exit nil)
  (id (yas/snippet-next-id) :read-only t)
  (control-overlay nil)
  active-field
  ;; stacked expansion: the `previous-active-field' slot saves the
  ;; active field where the child expansion took place
  previous-active-field
  force-exit)

(defstruct (yas/field (:constructor yas/make-field (number start end parent-field)))
  "A field."
  number
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  (back-adjacent-fields nil)
  (back-adjacent-mirrors nil))

(defstruct (yas/mirror (:constructor yas/make-mirror (start end transform)))
  "A mirror."
  start end
  (transform nil))

(defun yas/apply-transform (field-or-mirror field)
  "Calculate the value of the field/mirror. If there's a transform
for this field, apply it. Otherwise, returned nil."
  (let* ((yas/text (yas/field-text-for-display field))
	 (text yas/text)
	 (yas/modified-p (yas/field-modified-p field))
	 (yas/moving-away-p nil)
	 (transform (if (yas/mirror-p field-or-mirror)
			(yas/mirror-transform field-or-mirror)
		      (yas/field-transform field-or-mirror)))
	 (start-point (if (yas/mirror-p field-or-mirror)
			  (yas/mirror-start field-or-mirror)
			(yas/field-start field-or-mirror)))
	 (transformed (and transform
			   (save-excursion
			     (goto-char start-point)
			     (yas/eval-string transform)))))
    transformed))

(defsubst yas/replace-all (from to &optional text)
  "Replace all occurance from FROM to TO.

With optional string TEXT do it in that string."
  (goto-char (point-min))
  (if text
      (replace-regexp-in-string from to text t t)
    (while (search-forward from nil t)
      (replace-match to t t text))))

(defun yas/snippet-find-field (snippet number)
  (find-if #'(lambda (field)
	       (eq number (yas/field-number field)))
	   (yas/snippet-fields snippet)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the field's start point"
  (let ((n1 (yas/field-number field1))
        (n2 (yas/field-number field2)))
    (if n1
        (if n2
            (< n1 n2)
          t)
      (if n2
          nil
        (< (yas/field-start field1)
           (yas/field-start field2))))))

(defun yas/field-probably-deleted-p (field)
  "Guess if FIELD was deleted because of his parent-field"
  (and (zerop (- (yas/field-start field) (yas/field-end field)))
       (yas/field-parent-field field)))

(defun yas/snippets-at-point (&optional all-snippets)
  "Return a sorted list of snippets at point, most recently
inserted first."
  (sort
   (remove nil (remove-duplicates (mapcar #'(lambda (ov)
					      (overlay-get ov 'yas/snippet))
					  (if all-snippets
					      (overlays-in (point-min) (point-max))
					    (overlays-at (point))))))
   #'(lambda (s1 s2)
       (<= (yas/snippet-id s2) (yas/snippet-id s1)))))

(defun yas/next-field-or-maybe-expand ()
  "Try to expand a snippet at a key before point, otherwise
delegate to `yas/next-field'."
  (interactive)
  (if yas/triggers-in-field
      (let ((yas/fallback-behavior 'return-nil)
	    (active-field (overlay-get yas/active-field-overlay 'yas/field)))
	(when active-field
	  (unless (yas/expand active-field)
	    (yas/next-field))))
    (yas/next-field)))

(defun yas/next-field (&optional arg)
  "Navigate to next field.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (first (yas/snippets-at-point)))
	 (active-field (overlay-get yas/active-field-overlay 'yas/field))
         (live-fields (remove-if #'(lambda (field)
				     (and (not (eq field active-field))
					  (yas/field-probably-deleted-p field)))
				 (yas/snippet-fields snippet)))
	 (active-field-pos (position active-field live-fields))
	 (target-pos (and active-field-pos (+ arg active-field-pos)))
	 (target-field (nth target-pos live-fields)))
    ;; First check if we're moving out of a field with a transform
    ;; 
    (when (and active-field
	       (yas/field-transform active-field))
      (let* ((yas/moving-away-p t)
	     (yas/text (yas/field-text-for-display active-field))
	     (text yas/text)
	     (yas/modified-p (yas/field-modified-p active-field)))
	;;; primary field transform: exit call to field-transform
	(yas/eval-string (yas/field-transform active-field))))
    ;; Now actually move...
    (cond ((>= target-pos (length live-fields))
           (yas/exit-snippet snippet))
          (target-field
           (yas/move-to-field snippet target-field))
          (t
           nil))))

(defun yas/place-overlays (snippet field)
  "Correctly place overlays for SNIPPET's FIELD"
  (yas/make-move-field-protection-overlays snippet field)
  (yas/make-move-active-field-overlay snippet field))

(defun yas/move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas/field-start field))
  (setf (yas/snippet-active-field snippet) field)
  (yas/place-overlays snippet field)
  (overlay-put yas/active-field-overlay 'yas/field field)
  ;;; primary field transform: first call to snippet transform
  (unless (yas/field-modified-p field)
    (if (yas/field-update-display field snippet)
	(let ((inhibit-modification-hooks t))
	  (yas/update-mirrors snippet))
      (setf (yas/field-modified-p field) nil))))

(defun yas/prev-field ()
  "Navigate to prev field.  If there's none, exit the snippet."
  (interactive)
  (yas/next-field -1))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET."
  (interactive)
  (setf (yas/snippet-force-exit snippet) t)
  (goto-char (if (yas/snippet-exit snippet)
		 (yas/snippet-exit snippet)
	       (overlay-end (yas/snippet-control-overlay snippet)))))

;;; Apropos markers-to-points:
;;;
;;; This was ground useful for performance
;;; reasons, so that an excessive number of live markers arent kept
;;; aroung in the `buffer-undo-list'. However, in `markers-to-points',
;;; the set-to-nil markers can't simply be discarded and replaced with
;;; fresh ones in `points-to-markers'. The original marker that was
;;; just set to nilhas to be reused.
;;;
;;; This shouldn't bring horrible problems with undo/redo, but it
;;; would be one of the the first thing I'd remove if I was debugging that...
;;;

(defun yas/markers-to-points (snippet)
  "Convert all markers in SNIPPET to a cons (POINT . MARKER)
where POINT is the original position of the marker and MARKER is
the original marker object with the position set to nil."
  (dolist (field (yas/snippet-fields snippet))
    (let ((start (marker-position (yas/field-start field)))
	  (end (marker-position (yas/field-end field))))
      (set-marker (yas/field-start field) nil)
      (set-marker (yas/field-end field) nil)
      (setf (yas/field-start field) (cons start (yas/field-start field)))
      (setf (yas/field-end field) (cons end (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (let ((start (marker-position (yas/mirror-start mirror)))
	    (end (marker-position (yas/mirror-end mirror))))
	(set-marker (yas/mirror-start mirror) nil)
	(set-marker (yas/mirror-end mirror) nil)
	(setf (yas/mirror-start mirror) (cons start (yas/mirror-start mirror)))
	(setf (yas/mirror-end mirror) (cons end (yas/mirror-end mirror))))))
  (when (yas/snippet-exit snippet)
    (let ((exit (marker-position (yas/snippet-exit snippet))))
      (set-marker (yas/snippet-exit snippet) nil)
      (setf (yas/snippet-exit snippet) (cons exit (yas/snippet-exit snippet))))))

(defun yas/points-to-markers (snippet)
  "Convert all cons (POINT . MARKER) in SNIPPET to markers. This
is done by setting MARKER to POINT with `set-marker'."
  (dolist (field (yas/snippet-fields snippet))
    (setf (yas/field-start field) (set-marker (cdr (yas/field-start field)) (car (yas/field-start field))))
    (setf (yas/field-end field) (set-marker (cdr (yas/field-end field)) (car (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (setf (yas/mirror-start mirror) (set-marker (cdr (yas/mirror-start mirror)) (car (yas/mirror-start mirror))))
      (setf (yas/mirror-end mirror) (set-marker (cdr (yas/mirror-end mirror)) (car (yas/mirror-end mirror))))))
  (when (yas/snippet-exit snippet)
    (setf (yas/snippet-exit snippet) (set-marker (cdr (yas/snippet-exit snippet)) (car (yas/snippet-exit snippet))))))

(defun yas/commit-snippet (snippet &optional no-hooks)
  "Commit SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text.

Return a buffer position where the point should be placed if
exiting the snippet.

NO-HOOKS means don't run the `yas/after-exit-snippet-hook' hooks."

  (let ((control-overlay (yas/snippet-control-overlay snippet))
	yas/snippet-beg
	yas/snippet-end)
    ;;
    ;; Save the end of the moribund snippet in case we need to revive it
    ;; its original expansion.
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas/snippet-beg (overlay-start control-overlay))
      (setq yas/snippet-end (overlay-end control-overlay))
      (delete-overlay control-overlay))

    (let ((inhibit-modification-hooks t))
      (when yas/active-field-overlay
	(delete-overlay yas/active-field-overlay))
      (when yas/field-protection-overlays
	(mapcar #'delete-overlay yas/field-protection-overlays)))

    ;; stacked expansion: if the original expansion took place from a
    ;; field, make sure we advance it here at least to
    ;; `yas/snippet-end'...
    ;;
    (let ((previous-field (yas/snippet-previous-active-field snippet)))
      (when (and yas/snippet-end previous-field)
	(yas/advance-field-end-marker previous-field yas/snippet-end)))

    ;; Convert all markers to points,
    ;;
    (yas/markers-to-points snippet)

    ;; Take care of snippet revival
    ;;
    (if yas/snippet-revival
	(push `(apply yas/snippet-revive ,yas/snippet-beg ,yas/snippet-end ,snippet)
	      buffer-undo-list)
      ;; Dismember the snippet... this is useful if we get called
      ;; again from `yas/take-care-of-redo'....
      (setf (yas/snippet-fields snippet) nil))
    
    ;; XXX: `yas/after-exit-snippet-hook' should be run with
    ;; `yas/snippet-beg' and `yas/snippet-end' bound. That might not
    ;; be the case if the main overlay had somehow already
    ;; disappeared, which sometimes happens when the snippet's messed
    ;; up...
    ;;
    (unless no-hooks (run-hooks 'yas/after-exit-snippet-hook)))
  
  (message "[yas] snippet exited."))

(defun yas/check-commit-snippet ()
  "Checks if point exited the currently active field of the
snippet, if so cleans up the whole snippet up."
  (let* ((snippets (yas/snippets-at-point 'all-snippets))
	 (snippets-left snippets))
    (dolist (snippet snippets)
      (let ((active-field (yas/snippet-active-field snippet)))
	(cond ((or (prog1 (yas/snippet-force-exit snippet)
		     (setf (yas/snippet-force-exit snippet) nil))
		   (not (and active-field (yas/field-contains-point-p active-field))))
	       (setq snippets-left (delete snippet snippets-left))
	       (yas/commit-snippet snippet snippets-left))
	      ((and active-field
		    (or (not yas/active-field-overlay)
			(not (overlay-buffer yas/active-field-overlay))))
	       ;;
	       ;; stacked expansion: this case is mainly for recent
	       ;; snippet exits that place us back int the field of
	       ;; another snippet
	       ;;
	       (save-excursion
		 (yas/move-to-field snippet active-field)
		 (yas/update-mirrors snippet)))
	      (t
	       nil))))
    (unless snippets-left
      (remove-hook 'post-command-hook 'yas/post-command-handler 'local)
      (remove-hook 'pre-command-hook 'yas/pre-command-handler 'local))))

(defun yas/field-contains-point-p (field &optional point)
  (let ((point (or point
		   (point))))
    (and (>= point (yas/field-start field))
	 (<= point (yas/field-end field)))))

(defun yas/pre-command-handler () )

(defun yas/post-command-handler ()
  "Handles various yasnippet conditions after each command."
  (cond (yas/protection-violation
	 (goto-char yas/protection-violation)
	 (setq yas/protection-violation nil))
	((eq 'undo this-command)
	 ;;
	 ;; After undo revival the correct field is sometimes not
	 ;; restored correctly, this condition handles that
	 ;;
	 (let* ((snippet (car (yas/snippets-at-point)))
		(target-field (and snippet
				   (find-if-not #'yas/field-probably-deleted-p
						(remove nil
							(cons (yas/snippet-active-field snippet)
							      (yas/snippet-fields snippet)))))))
	   (when target-field
	     (yas/move-to-field snippet target-field))))
	((not (yas/undo-in-progress))
	 ;; When not in an undo, check if we must commit the snippet (use exited it).
	 (yas/check-commit-snippet))))

(defun yas/field-text-for-display (field)
  "Return the propertized display text for field FIELD.  "
  (buffer-substring (yas/field-start field) (yas/field-end field)))

(defun yas/undo-in-progress ()
  "True if some kind of undo is in progress"
  (or undo-in-progress
      (eq this-command 'undo)
      (eq this-command 'redo)))

(defun yas/make-control-overlay (snippet start end)
  "Creates the control overlay that surrounds the snippet and
holds the keymap."
  (let ((overlay (make-overlay start
                               end
                               nil
                               nil
                               t)))
    (overlay-put overlay 'keymap yas/keymap)
    (overlay-put overlay 'yas/snippet snippet)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun yas/skip-and-clear-or-delete-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-char'."
  (interactive)
  (let ((field (or field
		   (and yas/active-field-overlay
			(overlay-buffer yas/active-field-overlay)
			(overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
		(not (yas/field-modified-p field))
		(eq (point) (marker-position (yas/field-start field))))
	   (yas/skip-and-clear field)
	   (yas/next-field 1))
	  (t
	   (call-interactively 'delete-char)))))

(defun yas/skip-and-clear (field)
  "Deletes the region of FIELD and sets it modified state to t"
  (setf (yas/field-modified-p field) t)
  (delete-region (yas/field-start field) (yas/field-end field)))

(defun yas/advance-field-end-marker (field newend)
  "Advance FIELDs end-marker to NEWEND and recurse for parent fields"
  (when (< (yas/field-end field) newend)
    (set-marker (yas/field-end field) newend)
    (when (yas/field-parent-field field)
      (yas/advance-field-end-marker (yas/field-parent-field field) newend)))
  ;; take care of adjacent fields
  (let ((adjacents (yas/field-back-adjacent-fields field)))
    (when adjacents
      (dolist (adjacent adjacents)
	(when (< (yas/field-start adjacent) newend)
	  (set-marker (yas/field-start adjacent) newend))
	(yas/advance-field-end-marker adjacent newend))))
  ;; take care of adjacent mirrors
  (let ((adjacents (yas/field-back-adjacent-mirrors field)))
    (when adjacents
      (dolist (adjacent adjacents)
	(when (< (yas/mirror-start adjacent) newend)
	  (set-marker (yas/mirror-start adjacent) newend))))))

(defun yas/make-move-active-field-overlay (snippet field)
  "Place the active field overlay in SNIPPET's FIELD.

Move the overlay, or create it if it does not exit."
  (if (and yas/active-field-overlay
	   (overlay-buffer yas/active-field-overlay))
      (move-overlay yas/active-field-overlay
		    (yas/field-start field)
		    (yas/field-end field))
    (setq yas/active-field-overlay
	  (make-overlay (yas/field-start field)
			(yas/field-end field)
			nil nil t))
    (overlay-put yas/active-field-overlay 'face 'yas/field-highlight-face)
    (overlay-put yas/active-field-overlay 'yas/snippet snippet)
    (overlay-put yas/active-field-overlay 'modification-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-in-front-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-behind-hooks '(yas/on-field-overlay-modification))))

(defun yas/on-field-overlay-modification (overlay after? beg end &optional length)
  "Clears the field and updates mirrors, conditionally.

Only clears the field if it hasn't been modified and it point it
at field start. This hook doesn't do anything if an undo is in
progress."
  (unless (yas/undo-in-progress)
    (let ((field (overlay-get yas/active-field-overlay 'yas/field)))
      (cond (after?
	     (yas/advance-field-end-marker field (overlay-end overlay))
	     ;;; primary field transform: normal calls to expression
	     (let ((saved-point (point)))
	       (yas/field-update-display field (car (yas/snippets-at-point)))
	       (goto-char saved-point))
	     (yas/update-mirrors (car (yas/snippets-at-point))))
	    (field
	     (when (and (not after?)
			(not (yas/field-modified-p field))
			(eq (point) (if (markerp (yas/field-start field))
					(marker-position (yas/field-start field))
				      (yas/field-start field))))
	       (yas/skip-and-clear field))
	     (setf (yas/field-modified-p field) t))))))

;;; Apropos protection overlays:
;;;
;;; These exist for nasty users who will try to delete parts of the
;;; snippet outside the active field. Actual protection happens in
;;; `yas/on-protection-overlay-modification'.
;;;
;;; Currently this signals an error which inhibits the command. For
;;; commands that move point (like `kill-line'), point is restored in
;;; the `yas/post-command-handler' using a global
;;; `yas/protection-violation' variable.
;;;
;;; Alternatively, I've experimented with an implementation that
;;; commits the snippet before actually calling `this-command'
;;; interactively, and then signals an eror, which is ignored. but
;;; blocks all other million modification hooks. This presented some
;;; problems with stacked expansion.
;;;

(defun yas/make-move-field-protection-overlays (snippet field)
  "Place protection overlays surrounding SNIPPET's FIELD.

Move the overlays, or create them if they do not exit."
  (let ((start (yas/field-start field))
	(end (yas/field-end field)))
    ;; First check if the (1+ end) is contained in the buffer,
    ;; otherwise we'll have to do a bit of cheating and silently
    ;; insert a newline. the `(1+ (buffer-size))' should prevent this
    ;; when using stacked expansion
    ;; 
    (when (< (buffer-size) end)
      (save-excursion
	(let ((inhibit-modification-hooks t))
	  (goto-char (point-max))
	  (newline))))
    ;; go on to normal overlay creation/moving
    ;; 
    (cond ((and yas/field-protection-overlays
		(every #'overlay-buffer yas/field-protection-overlays))
	   (move-overlay (first yas/field-protection-overlays) (1- start) start)
	   (move-overlay (second yas/field-protection-overlays) end (1+ end)))
	  (t
	   (setq yas/field-protection-overlays
		 (list (make-overlay (1- start) start nil t nil)
		       (make-overlay end (1+ end) nil t nil)))
	   (dolist (ov yas/field-protection-overlays)
	     (overlay-put ov 'face 'yas/field-debug-face)
	     (overlay-put ov 'yas/snippet snippet)
	     ;; (overlay-put ov 'evaporate t)
	     (overlay-put ov 'modification-hooks '(yas/on-protection-overlay-modification)))))))

(defvar yas/protection-violation nil
  "When non-nil, signals attempts to erronesly exit or modify the snippet.

Functions in the `post-command-hook', for example
`yas/post-command-handler' can check it and reset its value to nil. The variables value is the point where the violation originated")

(defun yas/on-protection-overlay-modification (overlay after? beg end &optional length)
  "Signals a snippet violation, then issues error.

The error should be ignored in `debug-ignored-errors'"
  (cond ((not (or after?
		  (yas/undo-in-progress)))
	 (setq yas/protection-violation (point))
	 (error "Exit the snippet first!"))))

(add-to-list 'debug-ignored-errors "^Exit the snippet first!$")

;;; Apropos stacked expansion:
;;;
;;; the parent snippet does not run its fields modification hooks
;;; (`yas/on-field-overlay-modification' and
;;; `yas/on-protection-overlay-modification') while the child snippet
;;; is active. This means, among other things, that the mirrors of the
;;; parent snippet are not updated, this only happening when one exits
;;; the child snippet.
;;;
;;; Unfortunately, this also puts some ugly (and not fully-tested)
;;; bits of code in `yas/expand-snippet' and
;;; `yas/commit-snippet'. I've tried to mark them with "stacked
;;; expansion:".
;;;
;;; This was thought to be safer in in an undo/redo perpective, but
;;; maybe the correct implementation is to make the globals
;;; `yas/active-field-overlay' and `yas/field-protection-overlays' be
;;; snippet-local and be active even while the child snippet is
;;; running. This would mean a lot of overlay modification hooks
;;; running, but if managed correctly (including overlay priorities)
;;; they should account for all situations...
;;;

(defun yas/expand-snippet (start end template &optional snippet-vars)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)
  (goto-char start)

  ;; stacked expansion: shoosh the overlay modification hooks
  ;; 
  (let ((key (buffer-substring-no-properties start end))
	(inhibit-modification-hooks t)
	(column (current-column))
	snippet)

    ;; Delete the trigger key, this *does* get undo-recorded.
    ;;
    (delete-region start end)
    
    ;; Narrow the region down to the template, shoosh the
    ;; `buffer-undo-list', and create the snippet, the new snippet
    ;; updates its mirrors once, so we are left with some plain text.
    ;; The undo action for deleting this plain text will get recorded
    ;; at the end of this function.
    (save-restriction
      (narrow-to-region start start)
      (condition-case err
	  (let ((buffer-undo-list t))
	    ;; snippet creation might evaluate users elisp, which
	    ;; might generate errors, so we have to be ready to catch
	    ;; them mostly to make the undo information
	    ;;
	    (setq yas/start-column (save-restriction (widen) (current-column)))
	    (insert template)
	    (setq yas/deleted-text key)
	    (setq yas/selected-text (when mark-active key))
	    (setq snippet
		  (if snippet-vars
		      (eval `(let ,(read snippet-vars)
			       (yas/snippet-create (point-min) (point-max))))
		    (yas/snippet-create (point-min) (point-max)))))
	(error
	 (push (cons (point-min) (point-max)) buffer-undo-list)
	 (error (format "[yas] parse error: %s" (cadr err))))))

    ;; stacked-expansion: This checks for stacked expansion, save the
    ;; `yas/previous-active-field' and advance its boudary.
    ;;
    (let ((existing-field (and yas/active-field-overlay
			       (overlay-buffer yas/active-field-overlay)
			       (overlay-get yas/active-field-overlay 'yas/field))))
      (when existing-field
	(setf (yas/snippet-previous-active-field snippet) existing-field)
	(yas/advance-field-end-marker existing-field (overlay-end yas/active-field-overlay))))
    
    ;; Exit the snippet immediately if no fields
    ;;
    (unless (yas/snippet-fields snippet)
      (yas/exit-snippet snippet))
    
    ;; Push two undo actions: the deletion of the inserted contents of
    ;; the new snippet (whitout the "key") followed by an apply of
    ;; `yas/take-care-of-redo' on the newly inserted snippet boundaries
    ;; 
    (let ((start (overlay-start (yas/snippet-control-overlay snippet)))
	  (end (overlay-end (yas/snippet-control-overlay snippet))))
      (push (cons start end) buffer-undo-list)
      (push `(apply yas/take-care-of-redo ,start ,end ,snippet)
	    buffer-undo-list))
    ;; Now, move to the first field
    ;;
    (let ((first-field (car (yas/snippet-fields snippet))))
      (when first-field
	(yas/move-to-field snippet first-field))))
  (message "[yas] snippet expanded."))

(defun yas/take-care-of-redo (beg end snippet)
  "Commits SNIPPET, which in turn pushes an undo action for
reviving it.

Meant to exit in the `buffer-undo-list'."
  ;; slightly optimize: this action is only needed for snippets with
  ;; at least one field
  (when (yas/snippet-fields snippet)
    (yas/commit-snippet snippet 'no-hooks)))

(defun yas/snippet-revive (beg end snippet)
  "Revives the SNIPPET and creates a control overlay from BEG to
END.

BEG and END are, we hope, the original snippets boudaries. All
the markers/points exiting existing inside SNIPPET should point
to their correct locations *at the time the snippet is revived*.

After revival, push the `yas/take-care-of-redo' in the
`buffer-undo-list'"
  ;; Reconvert all the points to markers
  ;;
  (yas/points-to-markers snippet)
  ;; When at least one editable field existed in the zombie snippet,
  ;; try to revive the whole thing...
  ;;
  (let ((target-field (or (yas/snippet-active-field snippet)
			  (car (yas/snippet-fields snippet)))))
    (when target-field
      (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet beg end))
      (overlay-put (yas/snippet-control-overlay snippet) 'yas/snippet snippet)
   
      (yas/move-to-field snippet target-field)

      (add-hook 'post-command-hook 'yas/post-command-handler nil t)
      (add-hook 'pre-command-hook 'yas/pre-command-handler t t)
  
      (push `(apply yas/take-care-of-redo ,beg ,end ,snippet)
	    buffer-undo-list))))

(defun yas/snippet-create (begin end)
  "Creates a snippet from an template inserted between BEGIN and END.

Returns the newly created snippet."
  (let ((snippet (yas/make-snippet)))
    (goto-char begin)
    (yas/snippet-parse-create snippet)

    ;; Sort and link each field
    (yas/snippet-sort-link-fields snippet)

    ;; Calculate field and mirror adjacencies
    (yas/calculate-adjacencies snippet)
    
    ;; Update the mirrors for the first time
    (yas/update-mirrors snippet)

    ;; Create keymap overlay for snippet
    (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet (point-min) (point-max)))

    ;; Move to end
    (goto-char (point-max))

    ;; Setup hooks
    (add-hook 'post-command-hook 'yas/post-command-handler nil t)
    (add-hook 'pre-command-hook 'yas/pre-command-handler t t)
    
    snippet))

(defun yas/snippet-sort-link-fields (snippet)
  (setf (yas/snippet-fields snippet)
	(sort (yas/snippet-fields snippet)
	      '(lambda (field1 field2)
		 (yas/snippet-field-compare field1 field2)))))

(defun yas/calculate-adjacencies (snippet)
  ;; For each field in the snippet
  ;; 
  (dolist (field (yas/snippet-fields snippet))
    ;; Calculate its adjacencies to other mirrors and fields
    ;; 
    (dolist (otherfield (yas/snippet-fields snippet))
      (dolist (mirror (yas/field-mirrors otherfield))
	(when (= (yas/field-end field) (yas/mirror-start mirror))
	  (push mirror (yas/field-back-adjacent-mirrors field))))
      (when (and (not (eq otherfield field))
		 (= (yas/field-end field) (yas/field-start otherfield)))
	(when (not (find field (yas/field-back-adjacent-fields otherfield)))
	  (push otherfield (yas/field-back-adjacent-fields field)))))
    ;; Calculate the adjacencies of each one of its mirrors
    ;;
    ;; TODO: Known bug.
    ))

(defun yas/snippet-parse-create (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes"
  (let ((parse-start (point)))
    ;; protect quote and backquote escapes
    ;; 
    (yas/protect-escapes '(?` ?'))
    ;; replace all backquoted expressions
    ;;
    (goto-char parse-start)
    (yas/replace-backquotes)
    ;; protect escapes again since previous stepds might have
    ;; generated more characters needing escapinge
    ;;
    (goto-char parse-start)
    (yas/protect-escapes)
    ;; parse fields with {}
    ;; 
    (goto-char parse-start)
    (yas/field-parse-create snippet)
    ;; parse simple mirrors and fields
    ;; 
    (goto-char parse-start)
    (yas/simple-mirror-parse-create snippet)
    ;; parse mirror transforms
    ;;
    (goto-char parse-start)
    (yas/transform-mirror-parse-create snippet)
    ;; restore escapes
    ;;
    (goto-char parse-start)
    (yas/restore-escapes)
    ;; update mirrors for the first time
    ;;
    (yas/update-mirrors snippet)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    (yas/indent snippet)))

(defun yas/indent (snippet)
  (save-excursion
    (while (re-search-forward "$>" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (when (not (eq yas/indent-line 'auto))
	(indent-according-to-mode))))
  (save-excursion
    (cond ((eq yas/indent-line 'fixed)
	   (let* ((indent (if indent-tabs-mode
			      (concat (make-string (/ column tab-width) ?\t)
				      (make-string (% column tab-width) ?\ ))
			    (make-string (current-colum) ?\ ))))
	     (goto-char (point-min))
	     (while (and (zerop (forward-line))
			 (= (current-column) 0))
	       (insert indent))))
	  ((eq yas/indent-line 'auto)
	   (let ((end (set-marker (make-marker) (point-max)))
		 (snippet-markers (yas/collect-snippet-markers snippet)))
	     (save-restriction
	       (widen)
	       ;; XXX: Here seems to be the indent problem:
	       ;;
	       ;; `indent-according-to-mode' uses whatever
	       ;; `indent-line-function' is available. Some
	       ;; implementations of these functions delete text
	       ;; before they insert. If there happens to be a marker
	       ;; just after the text being deleted, the insertion
	       ;; actually happens after the marker, which misplaces
	       ;; it.
	       ;;
	       ;; This would also happen if we had used overlays with
	       ;; the `front-advance' property set to nil.
	       ;;
	       (while (and (zerop (forward-line 1))
			   (not (eobp))
			   (<= (point) end))
		 (goto-char (yas/real-line-beginning))
		 (let ((trouble-markers (remove-if-not #'(lambda (marker)
							   (= marker (point)))
						       snippet-markers)))
		       (indent-according-to-mode)
		       (mapc #'(lambda (marker)
				 (set-marker marker (point)))
			     trouble-markers)
		   (indent-according-to-mode)))
	       (set-marker end nil))))
	  (t
	   nil))))

(defun yas/collect-snippet-markers (snippet)
  "Make a list of all the markers used by SNIPPET."
  (let (markers)
    (dolist (field (yas/snippet-fields snippet))
      (push (yas/field-start field) markers)
      (push (yas/field-end field) markers)
      (dolist (mirror (yas/field-mirrors field))
	(push (yas/mirror-start mirror) markers)
	(push (yas/mirror-end mirror) markers)))
    (when (and (yas/snippet-exit snippet)
	       (marker-buffer (yas/snippet-exit snippet)))
      (push (yas/snippet-exit snippet) markers))
    markers))

(defun yas/real-line-beginning ()
  (let ((c (char-after (line-beginning-position)))
        (n (line-beginning-position)))
    (while (or (eql c ?\ )
               (eql c ?\t))
      (incf n)
      (setq c (char-after n)))
    n))


(defun yas/escape-string (escaped)
  (concat "YASESCAPE" (format "%d" escaped) "PROTECTGUARD"))

(defun yas/protect-escapes (&optional escaped)
  "Protect all escaped characters with their numeric ASCII value."
  (mapc #'(lambda (escaped)
	    (yas/replace-all (concat "\\" (char-to-string escaped))
			     (yas/escape-string escaped)))
  (or escaped yas/escaped-characters)))

(defun yas/restore-escapes (&optional text)
  "Restore all escaped characters from their numeric ASCII value.

With optional string TEXT do it in string instead"
  (let ((changed-text text)
	(text-provided-p text))
    (mapc #'(lambda (escaped)
	      (setq changed-text
		    (yas/replace-all (yas/escape-string escaped)
				     (char-to-string escaped)
				     (when text-provided-p changed-text))))
	  yas/escaped-characters)
    changed-text))

(defun yas/replace-backquotes ()
  "Replace all the \"`(lisp-expression)`\"-style expression
  with their evaluated value"
  (while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
  (let ((transformed (yas/eval-string (yas/restore-escapes (match-string 1)))))
    (goto-char (match-end 0))
    (when transformed (insert transformed))
    (delete-region (match-beginning 0) (match-end 0)))))

(defun yas/scan-sexps (from count)
  (condition-case err
      (with-syntax-table (standard-syntax-table)
	(scan-sexps from count))
    (error
     nil)))

(defun yas/make-marker (pos)
  "Create a marker at POS with `nil' `marker-insertion-type'"
  (let ((marker (set-marker (make-marker) pos)))
    (set-marker-insertion-type marker nil)
    marker))

(defun yas/field-parse-create (snippet &optional parent-field)
  "Parse most field expression, except for the simple one \"$n\".

The following count as a field:

* \"${n: text}\", for a numbered field with default text, as long as N is not 0;
* \"${n: text$(expression)}, the same with a lisp expression;
* the same as above but unnumbered, (no N:) and number is calculated automatically.

When multiple expressions are found, only the last one counts."
  (save-excursion
  (while (re-search-forward yas/field-regexp nil t)
    (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
	   (number (and (match-string-no-properties 1)
			(string-to-number (match-string-no-properties 1))))
	   (brand-new-field (and real-match-end-0
				 (not (save-match-data
					(eq (string-match "$[ \t\n]*(" (match-string-no-properties 2)) 0)))
				 (not (and number (zerop number)))
				 (yas/make-field number
						 (yas/make-marker (match-beginning 2))
						 (yas/make-marker (1- real-match-end-0))
						 parent-field))))
      (when brand-new-field
	(delete-region (1- real-match-end-0) real-match-end-0)
	(delete-region (match-beginning 0) (match-beginning 2))
	(push brand-new-field (yas/snippet-fields snippet))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (yas/field-start brand-new-field) (yas/field-end brand-new-field))
	    (goto-char (point-min))
	    (yas/field-parse-create snippet brand-new-field)))))))
  (when parent-field
  (save-excursion
    (while (re-search-forward yas/multi-dollar-lisp-expression-regexp nil t)
      (let* ((real-match-end-1 (yas/scan-sexps (match-beginning 1) 1)))
	(when real-match-end-1
	  (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1) real-match-end-1)))
	    (setf (yas/field-transform parent-field) (yas/restore-escapes lisp-expression-string)))
	  (delete-region (match-beginning 0) real-match-end-1)))))))

(defun yas/transform-mirror-parse-create (snippet)
  "Parse the \"${n:$(lisp-expression)}\" mirror transformations."
  (while (re-search-forward yas/transform-mirror-regexp nil t)
  (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
	 (number (string-to-number (match-string-no-properties 1)))
	 (field (and number
		     (not (zerop number))
		     (yas/snippet-find-field snippet number))))
    (when (and real-match-end-0
	       field)
      (push (yas/make-mirror (yas/make-marker (match-beginning 0))
			     (yas/make-marker (match-beginning 0))
			     (yas/restore-escapes (buffer-substring-no-properties (match-beginning 2)
										  (1- real-match-end-0))))
	    (yas/field-mirrors field))
      (delete-region (match-beginning 0) real-match-end-0)))))

(defun yas/simple-mirror-parse-create (snippet)
  "Parse the simple \"$n\" mirrors and the exit-marker."
  (while (re-search-forward yas/simple-mirror-regexp nil t)
  (let ((number (string-to-number (match-string-no-properties 1))))
    (cond ((zerop number)
	     
	   (setf (yas/snippet-exit snippet)
		 (yas/make-marker (match-end 0)))
	   (save-excursion
	     (goto-char (match-beginning 0))
	     (when (and yas/wrap-around-region yas/selected-text)
	       (insert yas/selected-text))
	     (delete-region (point) (yas/snippet-exit snippet))))
	  (t
	   (let ((field (yas/snippet-find-field snippet number)))
	     (if field
		 (push (yas/make-mirror (yas/make-marker (match-beginning 0))
					(yas/make-marker (match-beginning 0))
					nil)
		       (yas/field-mirrors field))
	       (push (yas/make-field number
				     (yas/make-marker (match-beginning 0))
				     (yas/make-marker (match-beginning 0))
				     nil)
		     (yas/snippet-fields snippet))))
	   (delete-region (match-beginning 0) (match-end 0)))))))

(defun yas/update-mirrors (snippet)
  "Updates all the mirrors of SNIPPET."
  (save-excursion
  (dolist (field (yas/snippet-fields snippet))
    (dolist (mirror (yas/field-mirrors field))
      ;; stacked expansion: I added an `inhibit-modification-hooks'
      ;; here, for safety, may need to remove if we the mechanism is
      ;; altered.
      ;; 
      (let ((inhibit-modification-hooks t))
	(yas/mirror-update-display mirror field)
	;; Take care of the fields adjacent to this mirror's back
	;; TODO: Known bug
	
	;; `yas/place-overlays' is needed if the active field and
	;; protected overlays have been changed because of insertions
	;; in `yas/mirror-update-display'
	;;
	(when (eq field (yas/snippet-active-field snippet))
	  (yas/place-overlays snippet field)))))))

(defun yas/mirror-update-display (mirror field)
  "Update MIRROR according to FIELD (and mirror transform)."
  (let ((reflection (or (yas/apply-transform mirror field)
			(yas/field-text-for-display field))))
  (when (and reflection
	     (not (string= reflection (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror)))))
    (goto-char (yas/mirror-start mirror))
    (insert reflection)
    (if (> (yas/mirror-end mirror) (point))
	(delete-region (point) (yas/mirror-end mirror))
      (set-marker (yas/mirror-end mirror) (point))))))

(defun yas/field-update-display (field snippet)
  "Much like `yas/mirror-update-display', but for fields"
  (when (yas/field-transform field)
  (let ((inhibit-modification-hooks t)
	(transformed (yas/apply-transform field field))
	(point (point)))
    (when (and transformed
	       (not (string= transformed (buffer-substring-no-properties (yas/field-start field) (yas/field-end field)))))
      (setf (yas/field-modified-p field) t)
      (goto-char (yas/field-start field))
      (insert transformed)
      (if (> (yas/field-end field) (point))
	  (delete-region (point) (yas/field-end field))
	(set-marker (yas/field-end field) (point)))
      t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug functions.  Use (or change) at will whenever needed.
;;

(defun yas/debug-some-vars ()
  "Debug snippets, fields, mirrors and the `buffer-undo-list'."
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
  (princ "Interesting YASnippet vars: \n\n")

  (princ (format "\nPost command hook: %s\n" post-command-hook))
  (princ (format "\nPre  command hook: %s\n" pre-command-hook))

  (princ (format "%s live snippets in total\n" (length (yas/snippets-at-point (quote all-snippets)))))
  (princ (format "%s live snippets at point:\n\n" (length (yas/snippets-at-point))))
    
  (dolist (snippet (yas/snippets-at-point))
    (princ (format "\tsid: %s active field %d from %s to %s covering \"%s\"\n"
		   (yas/snippet-id snippet)
		   (yas/field-number (yas/snippet-active-field snippet))
		   (marker-position (yas/field-start (yas/snippet-active-field snippet)))
		   (marker-position (yas/field-end (yas/snippet-active-field snippet)))
		   (buffer-substring-no-properties (yas/field-start (yas/snippet-active-field snippet)) (yas/field-end (yas/snippet-active-field snippet)))))
    (dolist (field (yas/snippet-fields snippet))
      (princ (format "\tfield %d from %s to %s covering \"%s\" adj-fields %s adj-mirrors %s\n"
		     (yas/field-number field)
		     (marker-position (yas/field-start field))
		     (marker-position (yas/field-end field))
		     (buffer-substring-no-properties (yas/field-start field) (yas/field-end field))
		     (length (yas/field-back-adjacent-fields field))
		     (length (yas/field-back-adjacent-mirrors field))))
      (dolist (mirror (yas/field-mirrors field))
	(princ (format "\t\tmirror from %s to %s covering \"%s\"\n"
		       (marker-position (yas/mirror-start mirror))
		       (marker-position (yas/mirror-end mirror))
		       (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror)))))))

  

  (princ (format "\nUndo is %s and point-max is %s.\n"
		 (if (eq buffer-undo-list t)
		     "DISABLED"
		   "ENABLED")
		 (point-max)))
  (unless (eq buffer-undo-list t)
    (princ (format "Undpolist has %s elements. First 10 elements follow:\n" (length buffer-undo-list)))
    (let ((first-ten (subseq buffer-undo-list 0 19)))
      (dolist (undo-elem first-ten)
	(princ (format "%2s:  %s\n" (position undo-elem first-ten) (truncate-string-to-width (format "%s" undo-elem) 70))))))))


(defun yas/exterminate-package ()
  (interactive)
  (yas/global-mode -1)
  (yas/minor-mode -1)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(defun yas/debug-test (&optional quiet)
  (interactive "P")
  (yas/load-directory (or (and (listp yas/root-directory)
			       (first yas/root-directory))
			  yas/root-directory
			  "~/Source/yasnippet/snippets/"))
  ;;(kill-buffer (get-buffer "*YAS TEST*"))
  (set-buffer (switch-to-buffer "*YAS TEST*"))
  (mapcar #'yas/commit-snippet (yas/snippets-at-point 'all-snippets))
  (erase-buffer)
  (setq buffer-undo-list nil)
  (setq undo-in-progress nil)
  (snippet-mode)
  (yas/minor-mode 1)
  (let ((abbrev))
    ;; (if (require 'ido nil t)
    ;; 	(setq abbrev (ido-completing-read "Snippet abbrev: " '("crazy" "prip" "prop")))
    ;;   (setq abbrev "prop"))
  (setq abbrev "$f")
  (insert abbrev))
  (unless quiet
    (add-hook 'post-command-hook 'yas/debug-some-vars 't 'local)))

(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
  ad-do-it
  (error (message (error-message-string err)))))

;; disable c-electric-* serial command in YAS fields
(add-hook 'c-mode-common-hook
          '(lambda ()
	  (make-variable-buffer-local 'yas/keymap)
	  (dolist (k '(":" ">" ";" "<" "{" "}"))
	    (define-key yas/keymap
	      k 'self-insert-command))))


;;; yasnippet.el ends here

;;; dropdown-list.el --- Drop-down menu interface
;;
;; Filename: dropdown-list.el
;; Description: Drop-down menu interface
;; Author: Jaeyoun Chung [jay.chung@gmail.com]
;; Maintainer:
;; Copyright (C) 2008 Jaeyoun Chung
;; Created: Sun Mar 16 11:20:45 2008 (Pacific Daylight Time)
;; Version:
;; Last-Updated: Sun Mar 16 12:19:49 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 43
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dropdown-list.el
;; Keywords: convenience menu
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  According to Jaeyoun Chung, "overlay code stolen from company-mode.el."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/16 dadams
;;     Clean-up - e.g. use char-to-string for control chars removed by email posting.
;;     Moved example usage code (define-key*, command-selector) inside the library.
;;     Require cl.el at byte-compile time.
;;     Added GPL statement.
;; 2008/01/06 Jaeyoun Chung
;;     Posted to gnu-emacs-sources@gnu.org at 9:10 p.m.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; decf, fourth, incf, loop, mapcar*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface dropdown-list-face
  '((t :inherit default :background "lightyellow" :foreground "black"))
  "*Bla." :group 'dropdown-list)

(defface dropdown-list-selection-face
  '((t :inherit dropdown-list-face :background "purple"))
  "*Bla." :group 'dropdown-list)

(defvar dropdown-list-overlays nil)

(defun dropdown-list-hide ()
  (while dropdown-list-overlays
    (delete-overlay (pop dropdown-list-overlays))))

(defun dropdown-list-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2 (overlay-put ov prop2 value2)))
    ov))

(defun dropdown-list-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0) (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string (propertize before-string 'face 'default)))
      (setq after-string (when after-string (propertize after-string 'face 'default))))
    (let ((string (concat before-string replacement after-string)))
      (if no-insert
          string
        (push (dropdown-list-put-overlay beg-point end-point 'invisible t
                                         'after-string string)
              dropdown-list-overlays)))))

(defun dropdown-list-start-column (display-width)
  (let ((column (mod (current-column) (window-width)))
        (width (window-width)))
    (cond ((<= (+ column display-width) width) column)
          ((> column display-width) (- column display-width))
          ((>= width display-width) (- width display-width))
          (t nil))))

(defun dropdown-list-move-to-start-line (candidate-count)
  (decf candidate-count)
  (let ((above-line-count (save-excursion (- (vertical-motion (- candidate-count)))))
        (below-line-count (save-excursion (vertical-motion candidate-count))))
    (cond ((= below-line-count candidate-count)
           t)
          ((= above-line-count candidate-count)
           (vertical-motion (- candidate-count))
           t)
          ((>= (+ below-line-count above-line-count) candidate-count)
           (vertical-motion (- (- candidate-count below-line-count)))
           t)
          (t nil))))

(defun dropdown-list-at-point (candidates &optional selidx)
  (dropdown-list-hide)
  (let* ((lengths (mapcar #'length candidates))
         (max-length (apply #'max lengths))
         (start (dropdown-list-start-column (+ max-length 3)))
         (i -1)
         (candidates (mapcar* (lambda (candidate length)
                                (let ((diff (- max-length length)))
                                  (propertize
                                   (concat (if (> diff 0)
                                               (concat candidate (make-string diff ? ))
                                             (substring candidate 0 max-length))
                                           (format "%3d" (+ 2 i)))
                                   'face (if (eql (incf i) selidx)
                                             'dropdown-list-selection-face
                                           'dropdown-list-face))))
                              candidates
                              lengths)))
    (save-excursion
      (and start
           (dropdown-list-move-to-start-line (length candidates))
           (loop initially (vertical-motion 0)
                 for candidate in candidates
                 do (dropdown-list-line (+ (current-column) start) candidate)
                 while (/= (vertical-motion 1) 0)
                 finally return t)))))

(defun dropdown-list (candidates)
  (let ((selection)
        (temp-buffer))
    (save-window-excursion
      (unwind-protect
          (let ((candidate-count (length candidates))
                done key (selidx 0))
            (while (not done)
              (unless (dropdown-list-at-point candidates selidx)
                (switch-to-buffer (setq temp-buffer (get-buffer-create "*selection*"))
                                  'norecord)
                (delete-other-windows)
                (delete-region (point-min) (point-max))
                (insert (make-string (length candidates) ?\n))
                (goto-char (point-min))
                (dropdown-list-at-point candidates selidx))
              (setq key (read-key-sequence ""))
              (cond ((and (stringp key)
                          (>= (aref key 0) ?1)
                          (<= (aref key 0) (+ ?0 (min 9 candidate-count))))
                     (setq selection (- (aref key 0) ?1)
                           done      t))
                    ((member key `(,(char-to-string ?\C-p) [up] "p"))
                     (setq selidx (mod (+ candidate-count (1- (or selidx 0)))
                                       candidate-count)))
                    ((member key `(,(char-to-string ?\C-n) [down] "n"))
                     (setq selidx (mod (1+ (or selidx -1)) candidate-count)))
                    ((member key `(,(char-to-string ?\f))))
                    ((member key `(,(char-to-string ?\r) [return]))
                     (setq selection selidx
                           done      t))
                    (t (setq done t)))))
        (dropdown-list-hide)
        (and temp-buffer (kill-buffer temp-buffer)))
      ;;     (when selection
      ;;       (message "your selection => %d: %s" selection (nth selection candidates))
      ;;       (sit-for 1))
      selection)))

(defun define-key* (keymap key command)
  "Add COMMAND to the multiple-command binding of KEY in KEYMAP.
Use multiple times to bind different COMMANDs to the same KEY."
  (define-key keymap key (combine-command command (lookup-key keymap key))))

(defun combine-command (command defs)
  "$$$$$ FIXME - no doc string"
  (cond ((null defs) command)
        ((and (listp defs)
              (eq 'lambda (car defs))
              (= (length defs) 4)
              (listp (fourth defs))
              (eq 'command-selector (car (fourth defs))))
         (unless (member `',command (cdr (fourth defs)))
           (setcdr (fourth defs) (nconc (cdr (fourth defs)) `(',command))))
         defs)
        (t
         `(lambda () (interactive) (command-selector ',defs ',command)))))

(defvar command-selector-last-command nil "$$$$$ FIXME - no doc string")

(defun command-selector (&rest candidates)
  "$$$$$ FIXME - no doc string"
  (if (and (eq last-command this-command) command-selector-last-command)
      (call-interactively command-selector-last-command)
    (let* ((candidate-strings
            (mapcar (lambda (candidate)
                      (format "%s" (if (symbolp candidate)
                                       candidate
                                     (let ((s (format "%s" candidate)))
                                       (if (>= (length s) 7)
                                           (concat (substring s 0 7) "...")
                                         s)))))
                    candidates))
           (selection (dropdown-list candidate-strings)))
      (when selection
        (let ((cmd (nth selection candidates)))
          (call-interactively cmd)
          (setq command-selector-last-command cmd))))))

;;;;;;;;;;;;;;;;;;;;

(provide 'dropdown-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el ends here;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/initialize-bundle ()
  "Initialize YASnippet and load snippets in the bundle."  (yas/global-mode 1)
;;; snippets for text-mode
(yas/define-snippets 'text-mode
'(
  ("time" "`(current-time-string)`" "(current time)" nil nil)
  ("email" "`(replace-regexp-in-string \"@\" \"@NOSPAM.\" user-mail-address)`" "(user's email)" nil nil)
  )
nil)

;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
'(
  ("struct" "struct ${1:name}
{
    $0
};" "struct ... { ... }" nil nil)
  ("once" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil)
  ("main" "int main(int argc, char *argv[])
{
    $0
    return 0;
}
" "int main(argc, argv) { ... }" nil nil)
  ("inc.1" "#include <$1>
" "#include <...>" nil nil)
  ("inc" "#include \"$1\"
" "#include \"...\"" nil nil)
  ("if" "if (${1:condition})
{
    $0
}" "if (...) { ... }" nil nil)
  ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}" "for (...; ...; ...) { ... }" nil nil)
  ("do" "do
{
    $0
} while (${1:condition});" "do { ... } while (...)" nil nil)
  )
'text-mode)

;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
'(
  ("using" "using namespace ${std};
$0" "using namespace ... " nil nil)
  ("template" "template <typename ${T}>" "template <typename ...>" nil nil)
  ("ns" "namespace " "namespace ..." nil nil)
  ("class" "class ${1:Name}
{
public:
    ${1:$(yas/substr text \"[^: ]*\")}($2);
    virtual ~${1:$(yas/substr text \"[^: ]*\")}();
};" "class ... { ... }" nil nil)
  ("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil)
  )
'cc-mode)

;;; snippets for c-mode
(yas/define-snippets 'c-mode
'(
  ("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");
" "FILE *fp = fopen(..., ...);" nil nil)
  )
'cc-mode)

;;; snippets for csharp-mode
(yas/define-snippets 'csharp-mode
'(
  ("using.2" "using System.$1;
" "using System....;" nil nil)
  ("using.1" "using System;
" "using System;" nil nil)
  ("using" "using $1;
" "using ...;" nil nil)
  ("region" "#region $1
$0
#endregion
" "#region ... #endregion" nil nil)
  ("prop" "/// <summary>
/// $5
/// </summary>
/// <value>$6</value>
$1 $2 $3
{
    get {
        return this.$4;
    }
    set {
        this.$4 = value;
    }
}
" "property ... ... { ... }" nil nil)
  ("namespace" "namespace $1
{
$0
}
" "namespace .. { ... }" nil nil)
  ("method" "/// <summary>
/// ${5:Description}
/// </summary>${2:$(if (string= (upcase text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" text \"</c></returns>\"))}
${1:public} ${2:void} ${3:MethodName}($4)
{
$0
}
" "public void Method { ... }" nil nil)
  ("comment.3" "/// <exception cref=\"$1\">$2</exception>
" "/// <exception cref=\"...\"> ... </exception>" nil nil)
  ("comment.2" "/// <returns>$1</returns>
" "/// <param name=\"...\"> ... </param>" nil nil)
  ("comment.1" "/// <param name=\"$1\">$2</param>
" "/// <param name=\"...\"> ... </param>" nil nil)
  ("comment" "/// <summary>
/// $1
/// </summary>
" "/// <summary> ... </summary>" nil nil)
  ("class" "${5:public} class ${1:Name}
{
    #region Ctor & Destructor
    /// <summary>
    /// ${3:Standard Constructor}
    /// </summary>
    public $1($2)
    {
    }

    /// <summary>
    /// ${4:Default Destructor}
    /// </summary>    
    public ~$1()
    {
    }
    #endregion
}
" "class ... { ... }" nil nil)
  ("attrib.2" "/// <summary>
/// $3
/// </summary>
private $1 ${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};

/// <summary>
/// ${3:Description}
/// </summary>
/// <value><c>$1</c></value>
public ${1:Type} ${2:Name}
{
    get {
        return this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};
    }
    set {
        this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")} = value;
    }
}
" "private _attribute ....; public Property ... ... { ... }" nil nil)
  ("attrib.1" "/// <summary>
/// $3
/// </summary>
private $1 $2;

/// <summary>
/// $4
/// </summary>
/// <value>$5</value>
public $1 $2
{
    get {
        return this.$2;
    }
    set {
        this.$2 = value;
    }
}
" "private attribute ....; public property ... ... { ... }" nil nil)
  ("attrib" "/// <summary>
/// $3
/// </summary>
private $1 $2;
" "private attribute ....;" nil nil)
  )
'cc-mode)

;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
'(
  ("prop" "- (${1:id})${2:foo}
{
    return $2;
}

- (void)set${2:$(capitalize text)}:($1)aValue
{
    [$2 autorelease];
    $2 = [aValue retain];
}
$0" "foo { ... } ; setFoo { ... }" nil nil)
  )
'cc-mode)

;;; snippets for css-mode
(yas/define-snippets 'css-mode
'(
  ("pad.top" "padding-top: $1;
" "padding-top: ..." nil nil)
  ("pad.right" "padding-right: $1;
" "padding-right: ..." nil nil)
  ("pad.padding" "padding: ${top} ${right} ${bottom} ${left};
" "padding: top right bottom left" nil nil)
  ("pad.pad" "padding: $1;
" "padding: ..." nil nil)
  ("pad.left" "padding-left: $1;
" "padding-left: ..." nil nil)
  ("pad.bottom" "padding-bottom: $1;
" "padding-bottom: ..." nil nil)
  ("mar.top" "margin-top: $1;
" "margin-top: ..." nil nil)
  ("mar.right" "margin-right: $1;
" "margin-right: ..." nil nil)
  ("mar.margin" "margin: ${top} ${right} ${bottom} ${left};
" "margin top right bottom left" nil nil)
  ("mar.mar" "margin: $1;
" "margin: ..." nil nil)
  ("mar.left" "margin-left: $1;
" "margin-left: ..." nil nil)
  ("mar.bottom" "margin-bottom: $1;
" "margin-bottom: ..." nil nil)
  ("fs" "font-size: ${12px};
" "font-size: ..." nil nil)
  ("ff" "font-family: $1;
" "font-family: ..." nil nil)
  ("disp.none" "dislpay: none;
" "display: none" nil nil)
  ("disp.inline" "dislpay: inline;
" "display: inline" nil nil)
  ("disp.block" "dislpay: block;
" "display: block" nil nil)
  ("cl" "clear: $1;
" "clear: ..." nil nil)
  ("bor" "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil nil)
  ("bg.1" "background-image: url($1);" "background-image: ..." nil nil)
  ("bg" "background-color: #${1:DDD};" "background-color: ..." nil nil)
  )
'text-mode)

;;; snippets for emacs-lisp-mode
(yas/define-snippets 'emacs-lisp-mode
'(
  ("word-or-region" ";; example of a command that works on current word or text selection
(defun down-case-word-or-region ()
  \"Lower case the current word or text selection.\"
(interactive)
(let (pos1 pos2 meat)
  (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
    (setq pos1 (car (bounds-of-thing-at-point 'symbol))
          pos2 (cdr (bounds-of-thing-at-point 'symbol))))

  ; now, pos1 and pos2 are the starting and ending positions
  ; of the current word, or current text selection if exists

  ;; put your code here.
  $0
  ;; Some example of things you might want to do
  (downcase-region pos1 pos2) ; example of a func that takes region as args
  (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.
  (delete-region pos1 pos2) ; get rid of it
  (insert \"newText\") ; insert your new text

  )
)
" "Command that works on region or word" nil nil)
  ("traverse_dir" ";; apply a function to all files in a dir
(require 'find-lisp)
(mapc 'my-process-file (find-lisp-find-files \"~/myweb/\" \"\\\\.html$\"))
" "traversing a directory" nil nil)
  ("grabthing" "(setq $0 (thing-at-point 'symbol))
" "grab word under cursor" nil nil)
  ("grabstring" "(setq $0 (buffer-substring-no-properties myStartPos myEndPos))
" "grab buffer substring" nil nil)
  ("find-replace" "(defun replace-html-chars-region (start end)
  \"Replace œôòü<œôòý to œôòü&lt;œôòý and other chars in HTML.
This works on the current region.\"
  (interactive \"r\")
  (save-restriction 
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward \"&\" nil t) (replace-match \"&amp;\" nil t))
    (goto-char (point-min))
    (while (search-forward \"<\" nil t) (replace-match \"&lt;\" nil t))
    (goto-char (point-min))
    (while (search-forward \">\" nil t) (replace-match \"&gt;\" nil t))
    )
  )
" "find and replace on region" nil nil)
  ("file.read-lines" "(defun read-lines (filePath)
  \"Return a list of lines in FILEPATH.\"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string
     (buffer-string) \"\\n\" t)) )

;; process all lines
(mapc 
 (lambda (aLine) 
   (message aLine) ; do your stuff here
   )
 (read-lines \"inputFilePath\")
)" "read lines of a file" nil nil)
  ("file.process" "(defun doThisFile (fpath)
  \"Process the file at path FPATH ...\"
  (let ()
    ;; create temp buffer without undo record or font lock. (more efficient)
    ;; first space in temp buff name is necessary
    (set-buffer (get-buffer-create \" myTemp\"))
    (insert-file-contents fpath nil nil nil t)

    ;; process it ...
    ;; (goto-char 0) ; move to begining of file's content (in case it was open)
    ;; ... do something here
    ;; (write-file fpath) ;; write back to the file

    (kill-buffer \" myTemp\")))
" "a function that process a file" nil nil)
  ("dired.process_marked" ";; idiom for processing a list of files in dired's marked files
 
;; suppose myProcessFile is your function that takes a file path
;; and do some processing on the file

(defun dired-myProcessFile ()
  \"apply myProcessFile function to marked files in dired.\"
  (interactive)
  (require 'dired)
  (mapc 'myProcessFile (dired-get-marked-files))
)

;; to use it, type M-x dired-myProcessFile
" "process marked files in dired" nil nil)
  ("defun" "(defun $1 ()
  \"thisandthat.\"
  (interactive)
  (let (var1)
    (setq var1 some)
    $0
  )
)" "function template" nil nil)
  )
'text-mode)

;;; snippets for erlang-mode
(yas/define-snippets 'erlang-mode
'(
  ("undef" "-undef($1).
$0
" "-undef(...)." nil nil)
  ("try" "try $1 of
    $0
catch
after
end
" "try ... of ... catch after end" nil nil)
  ("rec" "-record($1,{$2}).
$0
" "-record(...,{...})." nil nil)
  ("rcv.after" "receive
after
    $1 -> $0
end
" "receive after ... -> ... end" nil nil)
  ("rcv" "receive
    $1 -> $0
end
" "receive ... -> ... end" nil nil)
  ("mod" "-module(${1:$(file-name-nondirectory 
               (file-name-sans-extension (buffer-file-name)))}).
$0

" "-module()." nil nil)
  ("loop" "${1:loop}($2) ->
    receive
	${3:_} ->
	    $1($2)
    end.
$0
" "loop(...) -> receive _ -> loop(...) end." nil nil)
  ("inc.lib" "-include_lib(\"$1\").
$0
" "-include_lib(\"...\")." nil nil)
  ("inc" "-include(\"$1\").
$0
" "-include(\"...\")." nil nil)
  ("imp" "-import(${1:lists}, [${2:map/2, sum/1}]).
$0
" "-import([])." nil nil)
  ("ifndef" "-ifndef($1).
$0
-endif.
" "-ifndef(...). ... -endif." nil nil)
  ("ifdef" "-ifdef($1).
$0
-endif.
" "-ifdef(...). ... -endif." nil nil)
  ("if" "if
    $1 -> $2;
    true -> $0
end
" "if ... -> ... ; true -> ... end" nil nil)
  ("fun" "fun ($1) -> $0 end
" "fun (...) -> ... end" nil nil)
  ("exp" "-export([${1:start/0}]).
$0
" "-export([])." nil nil)
  ("def" "-define($1,$2).
$0
" "-define(...,...)." nil nil)
  ("compile" "-compile([${1:export_all}]).
$0
" "-compile(...)." nil nil)
  ("case" "case $1 of
    $0
end
" "case ... of ... end" nil nil)
  ("beh" "-behaviour(${1:gen_server}).
$0
" "-behaviour(...)." nil nil)
  ("begin" "begin
    $0
end
" "begin ... end" nil nil)
  ("after" "after
    $1 -> $0
" "after ... ->" nil nil)
  )
'text-mode)

;;; snippets for f90-mode
(yas/define-snippets 'f90-mode
'(
  ("wr" "write (${1:*},${2:*}) $0
" "write (*,*)" nil nil)
  ("su" "subroutine $0
" "subroutine" nil nil)
  ("st" "structure $0
" "structure" nil nil)
  ("re" "read (${1:*},${2:*}) $0
" "read (*,*)" nil nil)
  ("pr" "program ${1:name}
  $0
end program ${1:name}
" "program ... end program ..." nil nil)
  ("pa" "parameter $0
" "parameter" nil nil)
  ("l" "logical $0
" "logical" nil nil)
  ("ir" "implicit real $0
" "implicit real" nil nil)
  ("intr" "intrinsic $0
" "intrinsic" nil nil)
  ("inc" "include $0
" "include" nil nil)
  ("in" "implicit none
" "implicit none" nil nil)
  ("il" "implicit logical $0
" "implicit logical" nil nil)
  ("ii" "implicit integer $0
" "implicit integer " nil nil)
  ("if" "if ( ${1:condition} ) then
   $0
end if
" "if then end if" nil nil)
  ("ich" "implicit character $0
" "implicit character" nil nil)
  ("ic" "implicit complex $0
" "implicit complex" nil nil)
  ("ib" "implicit byte $0
" "implicit byte" nil nil)
  ("eq" "equivalence $0
" "equivalence" nil nil)
  ("dp" "double precision $0
" "double precision" nil nil)
  ("do" "do while (${1:condition})
   $0
end do
" "do while (...) end do" nil nil)
  ("dc" "double complex $0
" "double complex" nil nil)
  ("cx" "complex $0
" "complex" nil nil)
  ("ch" "character $0
" "character" nil nil)
  ("c" "continue $0
" "continue" nil nil)
  ("bd" "block data $0
" "block data" nil nil)
  ("au" "automatic $0 
" "automatic" nil nil)
  )
'text-mode)

;;; snippets for html-mode
(yas/define-snippets 'html-mode
'(
  ("ul.id" "<ul id=\"$1\">
  $0
</ul>" "<ul id=\"...\">...</ul>" nil "list")
  ("ul.class" "<ul class=\"$1\">
  $0
</ul>" "<ul class=\"...\">...</ul>" nil "list")
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil "list")
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil "table")
  ("title" "<title>$1</title>" "<title>...</title>" nil nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil "table")
  ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil "table")
  ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">
  $0
</table>" "<table ...>...</table>" nil "table")
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil)
  ("span.id" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil)
  ("span.class" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil nil)
  ("script.javascript-src" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil)
  ("script.javascript" "<script type=\"text/javascript\">
  $0
</script>" "<script type=\"text/javascript\">...</script> " nil nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil nil)
  ("ol.id" "<ol id=\"$1\">
  $0
</ol>" "<ol id=\"...\">...</ol>" nil "list")
  ("ol.class" "<ol class=\"$1\">
  $0
</ol>" "<ol class=\"...\">...</ol>" nil "list")
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil "list")
  ("meta.http-equiv" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil "meta")
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta")
  ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil)
  ("link.stylesheet-ie" "<!--[if IE]>
<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />
<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil)
  ("link.stylesheet" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil)
  ("li.class" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil "list")
  ("li" "<li>$1</li>" "<li>...</li>" nil "list")
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil)
  ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil)
  ("html.xmlns" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil nil)
  ("html" "<html>
  $0
</html>
" "<html>...</html>" nil nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil)
  ("hr" "<hr />
" "<hr />" nil nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header")
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header")
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header")
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header")
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header")
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header")
  ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">
  $0
</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil)
  ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil "list")
  ("dov" "a mirror up here $3


<dov ${1:id=\"${2:some_id and here comes another nested field: ${3:nested_shit}}\"}>
    $0
</dov>
<dov $1>
    actually some other shit and $3
</dov>
" "<dov...>...</dov>" nil nil)
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta")
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta")
  ("doctype.xhtml1_1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta")
  ("doctype.xhml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil "meta")
  ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil "meta")
  ("dl.id" "<dl id=\"$1\">
    $0
</dl>" "<dl> ... </dl>" nil "list")
  ("dl" "<dl>
    $0
</dl>
" "<dl> ... </dl>" nil "list")
  ("div.id-class" "<div id=\"$1\" class=\"$2\">
  $0
</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil)
  ("div.id" "<div id=\"$1\">
  $0
</div>" "<div id=\"...\">...</div>" nil nil)
  ("div.class" "<div class=\"$1\">
  $0
</div>" "<div class=\"...\">...</div>" nil nil)
  ("div" "<div${1: id=\"${2:some_id}\"}${3: class=\"${4:some_class}\"}>$0</div> " "<div...>...</div>" nil nil)
  ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil "list")
  ("code.class" "<code class=\"$1\">
  $0
</code>" "<code class=\"...\">...</code>" nil nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil nil)
  ("br" "<br />" "<br />" nil nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil nil)
  )
'text-mode)

;;; snippets for latex-mode
(yas/define-snippets 'latex-mode
'(
  ("begin" "
\\begin{${1:environment}}
$0
\\end{$1}
" "\\begin{environment} ... \\end{environment}" nil nil)
  )
'text-mode)

;;; snippets for markdown-mode
(yas/define-snippets 'markdown-mode
'(
  ("rlink" "[${1:Link Text}][$2] $0
" "Reference Link" nil nil)
  ("rlb" "[${1:Reference}]: ${2:URL} $3
$0
" "Reference Label" nil nil)
  ("rimg" "![${1:Alt Text}][$2] $0
" "Referenced Image" nil nil)
  ("ol" "${1:1}. ${2:Text}
${1:$(number-to-string (1+ (string-to-number text)))}. $0
" "Ordered List" nil nil)
  ("link" "[${1:Link Text}](${2:URL} $3) $0
" "Link" nil nil)
  ("img" "![${1:Alt Text}](${2:URL} $3) $0
" "Image" nil nil)
  ("hr.2" "
*******

$0
" "Horizontal Rule (*)" nil nil)
  ("hr.1" "
----------

$0
" "Horizontal Rule (-)" nil nil)
  ("h6" "###### ${1:Header 6} ######

$0
" "Header 6" nil nil)
  ("h5" "##### ${1:Header 5} #####

$0
" "Header 5" nil nil)
  ("h4" "#### ${1:Header 4} ####

$0
" "Header 4" nil nil)
  ("h3" "### ${1:Header 3} ###

$0
" "Header 3" nil nil)
  ("h2.2" "${1:Header 2}
${1:$(make-string (string-width text) ?\\-)}

$0
" "Header 2 (-)" nil nil)
  ("h2.1" "## ${1:Header 1} ##

$0
" "Header 2 (##)" nil nil)
  ("h1.2" "${1:Header 1}
${1:$(make-string (string-width text) ?\\=)}

$0
" "Header 1 (=)" nil nil)
  ("h1.1" "# ${1:Header 1} #

$0
" "Header 1 (#)" nil nil)
  ("`" "\\`${1:Code}\\` $0
" "Inline Code" nil nil)
  ("__" "**${1:Text}** $0
" "Strong" nil nil)
  ("_" "_${1:Text}_ $0
" "Emphasis" nil nil)
  ("-" "- ${1:Text}
-$0
" "Unordered List" nil nil)
  ("+" "+ ${1:Text}
+$0
" "Unordered List" nil nil)
  )
'text-mode)

;;; snippets for nxml-mode
(yas/define-snippets 'nxml-mode
'(
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil nil)
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil nil)
  ("title" "<title>$1</title>" "<title>...</title>" nil nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil nil)
  ("tag.2l" "<${1:tag}>
  $2
</$1>$0" "<tag> \\n...\\n</tag>" nil nil)
  ("tag.1l" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil)
  ("table" "<table>
  $0
</table>" "<table>...</table>" nil nil)
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil nil)
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil nil)
  ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil)
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta")
  ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil)
  ("li" "<li>$1</li>" "<li>...</li>" nil nil)
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil)
  ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil)
  ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil)
  ("hr" "<hr />
" "<hr />" nil nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header")
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header")
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header")
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header")
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header")
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header")
  ("form" "<form method=\"$1\" action=\"$2\">
  $0
</form>" "<form method=\"...\" action=\"...\"></form>" nil nil)
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta")
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta")
  ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta")
  ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil nil)
  ("br" "<br />" "<br />" nil nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil nil)
  )
'text-mode)

;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
'(
  ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil nil)
  ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil nil)
  ("xif" "${1:expression} if ${2:condition}" "... if ..." nil nil)
  ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil nil)
  ("while" "while ($1) {
    $0
}" "while (...) { ... }" nil nil)
  ("unless" "unless ($1) {
    $0
}" "unless (...) { ... }" nil nil)
  ("sub" "sub ${1:function_name} {
    $0
}" "sub ... { ... }" nil nil)
  ("ifee" "if ($1) {
	${2:# body...}
} elsif ($3) {
	${4:# elsif...}
} else {
	${5:# else...}
}" "if, elsif, else ..." nil nil)
  ("ife" "if ($1) {
    $2
} else {
    $3
}" "if (...) { ... } else { ... }" nil nil)
  ("if" "if ($1) {
    $0
}" "if (...) { ... }" nil nil)
  ("fore" "foreach my \\$${1:x} (@${2:array}) {
    ${3:# body...}
}" "foreach ... { ... }" nil nil)
  ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {
    ${3:# body...}
}" "for (...) { ... }" nil nil)
  ("eval" "eval {
    ${1:# do something risky...}
};
if (\\$@) {
    ${2:# handle failure...}
}" "eval { ... } if ($@) { ... }" nil nil)
  )
'text-mode)

;;; snippets for cperl-mode
(yas/define-snippets 'cperl-mode
'(
  )
'perl-mode)

;;; snippets for python-mode
(yas/define-snippets 'python-mode
'(
  ("while" "while ${condition}:
    $0" "while ... : ..." nil nil)
  ("propsg" "def _set_${1:foo}(self, value):
    self._$1 = value

def _get_$1(self):
    return self._$1

$1 = property(_get_$1, _set_$1)

$0
" "_get_foo ... _set_foo ... foo=property(...)" nil nil)
  ("propg" "def _get_${1:foo}(self):
    return self._$1

$1 = property(_get_$1)

$0
" "_get_foo ... foo=property(...)" nil nil)
  ("prop" "def ${1:foo}():
   doc = \"\"\"${2:Doc string}\"\"\"
   def fget(self):
       return self._$1
   def fset(self, value):
       self._$1 = value
   def fdel(self):
       del self._$1
   return locals()
$1 = property(**$1())

$0
" "prop" nil nil)
  ("ifmain" "if __name__ == '__main__':
    $0" "if __name__ == '__main__': ..." nil nil)
  ("for" "for ${var} in ${collection}:
    $0" "for ... in ... : ..." nil nil)
  ("defm" "def ${1:name}(self, $2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" "defm" nil nil)
  ("def" "def ${1:name}($2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" "def" nil nil)
  ("class" "class ${1:ClassName}(${2:object}):
    \"\"\"$3
    \"\"\"

    def __init__(self, $4):
        \"\"\"$5
        ${4:$
        (let* ((indent
                (concat \"\\n\" (make-string (current-column) 32)))
               (args
                (mapconcat
                 '(lambda (x)
                    (if (not (string= (nth 0 x) \"\"))
                        (concat \"- \" (char-to-string 96) (nth 0 x)
                                (char-to-string 96) \":\")))
                 (mapcar
                  '(lambda (x)
                     (mapcar
                      (lambda (x)
                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))
                  (mapcar '(lambda (x) (split-string x \"=\"))
                          (split-string text \",\")))
                 indent)))
          (if (string= args \"\")
              (make-string 3 34)
            (mapconcat
             'identity
             (list \"\" \"Arguments:\" args (make-string 3 34))
             indent)))
        }
        ${4:$
        (mapconcat
         '(lambda (x)
            (if (not (string= (nth 0 x) \"\"))
                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))
         (mapcar
          '(lambda (x)
             (mapcar
              '(lambda (x)
                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
              x))
          (mapcar '(lambda (x) (split-string x \"=\"))
                  (split-string text \",\")))
         (concat \"\\n\" (make-string (current-column) 32)))
        }
        $0
" "class" nil nil)
  ("__" "__${init}__" "__...__" nil nil)
  )
'text-mode)

;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
'(
  ("tit" "${1:$(make-string (string-width text) ?\\=)}
${1:Title}
${1:$(make-string (string-width text) ?\\=)}

$0" "Document title" nil nil)
  ("sec" "${1:Section}
${1:$(make-string (string-width text) ?\\-)}

$0" "Section title" nil nil)
  ("chap" "${1:Chapter}
${1:$(make-string (string-width text) ?\\=)}

$0" "Chapter title" nil nil)
  )
'text-mode)

;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
'(
  ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil "collections")
  ("y" ":yields: $0" ":yields: arguments (rdoc)" nil "general")
  ("while" "while ${condition}
  $0
end" "while ... end" nil "control structure")
  ("when" "when ${condition}
  $0
end" "when ... end" nil "control structure")
  ("w" "attr_writer :" "attr_writer ..." nil "definitions")
  ("upt" "upto(${n}) { |${i}|
  $0
}" "upto(...) { |n| ... }" nil "control structure")
  ("until" "until ${condition}
  $0
end" "until ... end" nil "control structure")
  ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil "control structure")
  ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil "collections")
  ("rw" "attr_accessor :" "attr_accessor ..." nil "definitions")
  ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil "general")
  ("req" "require \"$0\"" "require \"...\"" nil "general")
  ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil "collections")
  ("rb" "#!/usr/bin/ruby -wKU
" "/usr/bin/ruby -wKU" nil "general")
  ("r" "attr_reader :" "attr_reader ..." nil "definitions")
  ("mm" "def method_missing(method, *args)
  $0
end" "def method_missing ... end" nil "definitions")
  ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil "collections")
  ("ife" "if ${1:condition}
  $2
else
  $3
end" "if ... else ... end" nil "control structure")
  ("if" "if ${1:condition}
  $0
end" "if ... end" nil "control structure")
  ("forin" "for ${1:element} in ${2:collection}
  $0
end" "for ... in ...; ... end" nil "control structure")
  ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil "collections")
  ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil "collections")
  ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil "collections")
  ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil "collections")
  ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil "collections")
  ("dow" "downto(${0}) { |${n}|
  $0
}" "downto(...) { |n| ... }" nil "control structure")
  ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil "collections")
  ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil "collections")
  ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil "general")
  ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil "collections")
  ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory
                                 (file-name-sans-extension
				 (or (buffer-file-name)
				     (buffer-name (current-buffer))))))))
           (cond
             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))
              (t fn)))`}
  $0
end
" "class ... end" nil "definitions")
  ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil "collections")
  ("cla" "class << ${self}
  $0
end" "class << self ... end" nil "definitions")
  ("case" "case ${1:object}
when ${2:condition}
  $0
end" "case ... end" nil "general")
  ("bm" "Benchmark.bmbm(${1:10}) do |x|
  $0
end" "Benchmark.bmbm(...) do ... end" nil "general")
  ("app" "if __FILE__ == $PROGRAM_NAME
  $0
end" "if __FILE__ == $PROGRAM_NAME ... end" nil "general")
  ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil "collections")
  ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil "definitions")
  ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil "collections")
  ("Comp" "include Comparable

def <=> other
  $0
end" "include Comparable; def <=> ... end" nil "definitions")
  ("=b" "=begin rdoc
  $0
=end" "=begin rdoc ... =end" nil "general")
  ("#" "# => " "# =>" nil "general")
  )
'text-mode)

;;; snippets for scala-mode
(yas/define-snippets 'scala-mode
'(
  ("with" "with $0" "with T" nil nil)
  ("whi" "while (${1:condition}) {
  $0
}" "while(cond) { .. }" nil nil)
  ("var.ret" "var ${1:name}: ${2:T} = ${3:obj} $0
" "var name: T = .." nil nil)
  ("var.new" "var ${1:name} = new ${2:obj} $0
" "var name = new .." nil nil)
  ("var" "var ${1:name} = ${2:obj} $0
" "var name = .." nil nil)
  ("val.ret" "val ${1:name}: ${2:T} = ${3:obj} $0
" "val name: T = .." nil nil)
  ("val.new" "val ${1:name} = new ${2:obj} $0" "val name = new .." nil nil)
  ("val" "val ${1:name} = ${2:obj} $0" "val name = .." nil nil)
  ("tup.paren" "(${1:element1}, ${2:element2}) $0" "(element1, element2)" nil nil)
  ("tup.arrow" "${1:element1} -> ${2:element2} $0" "element1 -> element2" nil nil)
  ("try.finally" "try {

} finally {
  $0
}" "try { .. } finally { .. }" nil nil)
  ("try.catch-finally" "try {
  $0
} catch {
  case ${1:e}: ${2:Exception} => 
    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}
} finally {

}" "try { .. } catch { case e => ..} finally { ..}" nil nil)
  ("try" "try {
  $0
} catch {
  case ${1:e}: ${2:Exception} => 
    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}
}" "try { .. } catch { case e => ..}" nil nil)
  ("tr.with" "trait ${1:name} with ${2:trait} {
  $0
}" "trait T1 with T2 { .. }" nil nil)
  ("tr.ext-with" "trait ${1:name} extends ${2:class} with ${3:trait} {
  $0
}" "trait T1 extends C with T2 { .. }" nil nil)
  ("tr.ext" "trait ${1:name} extends ${2:class} {
  $0
}" "trait T extends C { .. }" nil nil)
  ("tr" "trait ${1:name} {
  $0
}" "trait T { .. }" nil nil)
  ("throw" "throw new ${1:Exception}(${2:msg}) $0" "throw new Exception" nil nil)
  ("test" "//@Test
def test${1:name} = {
  $0
}" "@Test def testX = ..." nil nil)
  ("suite" "import org.scalatest._

class ${1:name} extends Suite {
  $0
}" "class T extends Suite { .. }" nil nil)
  ("pro.param" "protected[${1:this}] $0" "protected[this]" nil nil)
  ("pro" "protected $0" "protected" nil nil)
  ("pri.param" "private[${1:this}] $0" "private[this]" nil nil)
  ("pri" "private $0" "private" nil nil)
  ("pr.trace" "println(\"${1:obj}: \" + ${1:obj}) $0" "println(\"obj: \" + obj)" nil nil)
  ("pr.string" "println(\"${1:msg}\") $0" "println(\"..\")" nil nil)
  ("pr.simple" "print(${1:obj}) $0" "print(..)" nil nil)
  ("pr.newline" "println(${1:obj}) $0" "println(..)" nil nil)
  ("pac" "package $0" "package .." nil nil)
  ("ob" "object ${1:name} extends ${2:type} $0" "object name extends T" nil nil)
  ("mix" "trait ${1:name} {
  $0
}" "trait T { .. }" nil nil)
  ("match.option" "${1:option} match {
  case None => $0
  case Some(res) => 

}" "option match { case None => .. }" nil nil)
  ("match.can" "${1:option} match {
  case Full(res) => $0

  case Empty => 

  case Failure(msg, _, _) => 

}" "can match { case Full(res) => .. }" nil nil)
  ("match" "${1:cc} match {
  case ${2:pattern} => $0
}" "cc match { .. }" nil nil)
  ("map.new" "Map(${1:key} -> ${2:value}) $0" "Map(key -> value)" nil nil)
  ("map" "map(${1:x} => ${2:body}) $0" "map(x => ..)" nil nil)
  ("main" "def main(args: Array[String]) = {
  $0
}" "def main(args: Array[String]) = { ... }" nil nil)
  ("ls.val-new" "val ${1:l} = List(${2:args}, ${3:args}) $0" "val l = List(..)" nil nil)
  ("ls.new" "List(${1:args}, ${2:args}) $0" "List(..)" nil nil)
  ("isof" "isInstanceOf[${1:type}] $0" "isInstanceOf[T] " nil nil)
  ("intercept" "intercept(classOf[${1:Exception]}) {
  $0
}" "intercept(classOf[T]) { ..}" nil nil)
  ("imp" "import $0" "import .." nil nil)
  ("if.else" "if (${1:condition}) {
  $2
} else {
  $0
}" "if (cond) { .. } else { .. }" nil nil)
  ("if" "if (${1:condition}) {
  $0
}" "if (cond) { .. }" nil nil)
  ("hset.val-new" "val ${1:m} = new HashSet[${2:key}] $0" "val m = new HashSet[K]" nil nil)
  ("hset.new" "new HashSet[${1:key}] $0
" "new HashSet[K]" nil nil)
  ("hmap.val-new" "val ${1:m} = new HashMap[${2:key}, ${3:value}] $0" "val m = new HashMap[K, V]" nil nil)
  ("hmap.new" "new HashMap[${1:key}, ${2:value}] $0" "new HashMap[K, V]" nil nil)
  ("foreach" "foreach(${1:x} => ${2:body}) $0" "foreach(x => ..)" nil nil)
  ("for.multi" "for {
  ${1:x} <- ${2:xs}
  ${3:x} <- ${4:xs}
} {
  yield $0
}" "for {x <- xs \\ y <- ys} { yield }" nil nil)
  ("for.loop" "for (${1:x} <- ${2:xs}) {
  $0
}" "for (x <- xs) { ... }" nil nil)
  ("for.if" "for (${1:x} <- ${2:xs} if ${3:guard}) {
  $0
}" "for (x <- xs if guard) { ... }" nil nil)
  ("for.extract" "${1:x} <- ${2:xs}" "x <- xs" nil nil)
  ("ext" "extends $0" "extends T" nil nil)
  ("expect" "expect(${1:reply}) {
  $0
}" "expect(value) { ..}" nil nil)
  ("doc.scaladoc" "/**
 * ${1:description}
 * $0
 */" "/** ... */" nil nil)
  ("doc.file-scala-api" "/*                     __                                               *\\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")`, LAMP/EPFL             **
**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\\*                                                                      */
/** 
 * $0
 * @author ${1:name} 
 * @version ${2:0.1}
 * $Id$
 */" "/** scala api file */" nil nil)
  ("doc.file-scala" "/*                     __                                               *\\
**     ________ ___   / /  ___     Scala $3                               **
**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")` , LAMP/EPFL             **
**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\\*                                                                      */
/** 
 * $0
 * @author ${1:name} 
 * @version ${2:0.1}
 * $Id$
 */" "/** scala file */" nil nil)
  ("doc.file" "/**
 * `(scala-mode-file-doc)`
 * $0
 * @author ${1:name}
 * @version ${2:0.1} 
 */" "/** file name */" nil nil)
  ("doc.def" "/** 
 * `(scala-mode-def-and-args-doc)`
 */ " "/** method name */" nil nil)
  ("doc.class" "/** 
 * `(scala-mode-find-clstrtobj-name-doc)`
 * ${1:description}
 * $0  
 */" "/** cls/trt/obj name */" nil nil)
  ("def.simple" "def ${1:name} = $0" "def f = ..." nil nil)
  ("def.ret-body" "def ${1:name}: ${3:Unit} = {
  $0
}" "def f: R = {...}" nil nil)
  ("def.ret" "def ${1:name}: ${2:Unit} = $0" "def f: R = ..." nil nil)
  ("def.body" "def ${1:name} = {
  $0
}" "def f = {...}" nil nil)
  ("def.arg-ret-body" "def ${1:name}(${2:args}): ${3:Unit} = {
  $0
}" "def f(arg: T): R = {...}" nil nil)
  ("def.arg-ret" "def ${1:name}(${2:args}): ${3:Unit} = $0" "def f(arg: T): R = ..." nil nil)
  ("def.arg-body" "def ${1:name}(${2:args}) = {
  $0
}" "def f(arg: T) = {...}" nil nil)
  ("def.arg" "def ${1:name}(${2:args}) = $0" "def f(arg: T) = ..." nil nil)
  ("cons.nil" "${1:element1} :: Nil $0
" "element1 :: Nil" nil nil)
  ("cons" "${1:element1} :: ${2:element2} $0" "element1 :: element2" nil nil)
  ("co" "case object ${1:name} $0" "case object T" nil nil)
  ("clof" "classOf[${1:type}] $0" "classOf[T] " nil nil)
  ("cl.arg" "class ${1:name}(${2:args}) {
  $0
}" "class T(args) { .. }" nil nil)
  ("cl.abs-arg" "abstract class ${1:name}(${2:args}) {
  $0
}" "abstract class T(args) { .. }" nil nil)
  ("cl.abs" "abstract class ${1:name} {
  $0
}" "abstract class T { .. }" nil nil)
  ("cl" "class ${1:name} {
  $0
}" "class T { .. }" nil nil)
  ("cc" "case class ${1:name}(${2:arg}: ${3:type}) $0" "case class T(arg: A)" nil nil)
  ("cast" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil)
  ("case.match-all" "case _ => $0" "case _ => " nil nil)
  ("case" "case ${1:pattern} => $0" "case pattern => " nil nil)
  ("bang" "${1:actor} ! ${2:message} $0" "actor ! message" nil nil)
  ("at.version" "@version ${1:0.1} $0" "@version number" nil nil)
  ("at.return" "@return ${1:description} $0" "@return description" nil nil)
  ("at.param" "@param ${1:name} ${2:description} $0" "@param name description" nil nil)
  ("at.author" "@author ${1:name} $0" "@author name" nil nil)
  ("ass.true" "assert(true) $0" "assert(true)" nil nil)
  ("ass" "assert(${1:x} === ${2:y}) $0" "assert(x === y)" nil nil)
  ("asof" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil)
  ("arr.val-new" "val ${1:arr} = Array[${2:value}](${3:args}) $0" "val a = Array[T](..)" nil nil)
  ("arr.new" "Array[${1:value}](${2:args}) $0" "Array[T](..)" nil nil)
  ("app" "object ${1:name} extends Application {
  $0
}" "object name extends Application" nil nil)
  ("ano" "($1) => ${2:body} $0" "(args) => ..." nil nil)
  ("actor" "val a = actor {
  loop {
    react {
      $0
    }
  }
}" "val a = actor { ..}" nil nil)
  ("act.arg" "def act(${1:arg}: ${2:type}) = {
  loop {
    react {
      $0
    }
  }
}" "def act(arg: T) = { ..}" nil nil)
  ("act" "def act = {
  loop {
    react {
      $0
    }
  }
}" "def act = { ..}" nil nil)
  )
'text-mode)

;;; snippets for snippet-mode
(yas/define-snippets 'snippet-mode
'(
  ("vars" "# name : $1${2:
# key : ${3:expand-key}}${4:
# key : ${5:group}} 
# contributor : $6
# --
$0" "Snippet header" nil nil)
  ("$m" "\\${${2:n}:${4:\\$(${5:reflection-fn})}\\}$0

" "${n:$(...)} mirror" nil nil)
  ("$f" "\\${${1:${2:n}:}$3${4:\\$(${5:lisp-fn})}\\}$0" "${ ...  } field" nil nil)
  )
'text-mode)

;;; snippets for sql-mode
(yas/define-snippets 'sql-mode
'(
  ("references" "REFERENCES ${1:TableName}([${2:ColumnName}])
" "REFERENCES ..." nil nil)
  ("create.1" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] 
(
		$3		$4		= ${5:NULL}		${6:OUTPUT}
)
AS
BEGIN
$0
END
GO
" "create procedure ..." nil nil)
  ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] 
(
		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}
$0
	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) 
)
GO
" "create table ..." nil nil)
  ("constraint.1" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) 
" "CONSTRAINT [..] FOREIGN KEY ..." nil nil)
  ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) 
" "CONSTRAINT [..] PRIMARY KEY ..." nil nil)
  ("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}
" ", ColumnName ColumnType NOT NULL..." nil nil)
  )
'text-mode)

)

(yas/initialize-bundle)
;;;###autoload(require 'yasnippet-bundle)
(provide 'yasnippet-bundle)
;;; yasnippet-bundle.el ends here
