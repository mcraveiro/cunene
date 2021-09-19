;;; jump-tree-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jump-tree" "jump-tree.el" (0 0 0 0))
;;; Generated autoloads from jump-tree.el

(autoload 'jump-tree-mode "jump-tree" "\
Toggle jump-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.
jump-tree-mode replaces Emacs' standard position feature with a more
powerful yet easier to use version, that treats the position history
as what it is: a tree.
The following keys are available in `jump-tree-mode':
  \\{jump-tree-map}
Within the jump-tree visualizer, the following keys are available:
  \\{jump-tree-visualizer-mode-map}

If called interactively, enable Jump-Tree mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-jump-tree-mode 'globalized-minor-mode t)

(defvar global-jump-tree-mode nil "\
Non-nil if Global Jump-Tree mode is enabled.
See the `global-jump-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-jump-tree-mode'.")

(custom-autoload 'global-jump-tree-mode "jump-tree" nil)

(autoload 'global-jump-tree-mode "jump-tree" "\
Toggle Jump-Tree mode in all buffers.
With prefix ARG, enable Global Jump-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Jump-Tree mode is enabled in all buffers where
`turn-on-jump-tree-mode' would do it.
See `jump-tree-mode' for more information on Jump-Tree mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jump-tree" '("jump-tree-" "turn-on-jump-tree-mode")))

;;;***

;;;### (autoloads nil "jump-tree-pos" "jump-tree-pos.el" (0 0 0 0))
;;; Generated autoloads from jump-tree-pos.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jump-tree-pos" '("jump-tree-")))

;;;***

;;;### (autoloads nil "jump-tree-visualizer" "jump-tree-visualizer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jump-tree-visualizer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jump-tree-visualizer" '("jump-tree-")))

;;;***

;;;### (autoloads nil nil ("jump-tree-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jump-tree-autoloads.el ends here
