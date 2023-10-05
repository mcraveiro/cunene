;;; super-save-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "super-save" "super-save.el" (0 0 0 0))
;;; Generated autoloads from super-save.el

(defvar super-save-mode nil "\
Non-nil if super-save mode is enabled.
See the `super-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `super-save-mode'.")

(custom-autoload 'super-save-mode "super-save" nil)

(autoload 'super-save-mode "super-save" "\
A minor mode that saves your Emacs buffers when they lose focus.

This is a global minor mode.  If called interactively, toggle the
`super-save mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='super-save-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "super-save" '("super-save-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; super-save-autoloads.el ends here
