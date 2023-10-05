;;; diff-at-point-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from diff-at-point.el

(autoload 'diff-at-point-open-and-goto-hunk "diff-at-point" "\
Open a diff of the repository in the current frame.
Jumping to the file & line.

When SCROLL-RESET is not nil the view re-centers,
otherwise the offset from the window is kept.

(fn &optional SCROLL-RESET)" t)
(autoload 'diff-at-point-goto-source-and-close "diff-at-point" "\
Go to the source and close the current diff buffer.

When SCROLL-RESET is not nil the view re-centers,
otherwise the offset from the window is kept.

(fn &optional SCROLL-RESET)" t)
(register-definition-prefixes "diff-at-point" '("diff-at-point-"))

;;; End of scraped data

(provide 'diff-at-point-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; diff-at-point-autoloads.el ends here
