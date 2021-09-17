;;; smart-hungry-delete-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "smart-hungry-delete" "smart-hungry-delete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from smart-hungry-delete.el

(autoload 'smart-hungry-delete-add-default-hooks "smart-hungry-delete" "\
Add to some hooks for sensible default character/word/delimiter configuration." t nil)

(autoload 'smart-hungry-delete-backward-char "smart-hungry-delete" "\
If there is more than one char of whitespace between previous word and point,
delete all but one unless there's whitespace or newline directly
after the point--which will delete all whitespace back to
word--, else fall back to (delete-backward-char 1).

With prefix argument ARG, just delete a single char.

\(fn ARG)" t nil)

(autoload 'smart-hungry-delete-forward-char "smart-hungry-delete" "\
If there is more than one char of whitespace between point and next word,
delete all but one unless there's whitespace or newline directly
before the point--which will delete all whitespace up to word--,
else fall back to (delete-char 1).

With prefix argument ARG, just delete a single char.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-hungry-delete" '("smart-hungry-delete-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-hungry-delete-autoloads.el ends here
