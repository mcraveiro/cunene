;;; org-ref-prettify-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from org-ref-prettify.el

(autoload 'org-ref-prettify-mode "org-ref-prettify" "\
Toggle Org Ref Prettify mode.

\\{org-ref-prettify-mode-map}

This is a minor mode.  If called interactively, toggle the
`Org-Ref-Prettify mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-ref-prettify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'org-ref-prettify-edit-link-at-point "org-ref-prettify" "\
Edit the current citation link in the minibuffer.
WHERE means where the point should be put in the minibuffer.  If
it is nil, try to be smart about its placement; otherwise, it can
be one of: `type', `beg', or `end'.

(fn &optional WHERE)" t)
(autoload 'org-ref-prettify-edit-link-at-mouse "org-ref-prettify" "\
Edit the citation link at mouse position in the minibuffer.
This should be bound to a mouse click EVENT type.
See `org-ref-prettify-edit-link-at-point' for the meaning of WHERE.

(fn EVENT &optional WHERE)" t)
(register-definition-prefixes "org-ref-prettify" '("org-ref-prettify-"))

;;; End of scraped data

(provide 'org-ref-prettify-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; org-ref-prettify-autoloads.el ends here
