;;; json-navigator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "json-navigator" "json-navigator.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-navigator.el

(autoload 'json-navigator-navigate-after-point "json-navigator" "\
Navigate JSON after point." t nil)

(autoload 'json-navigator-navigate-region "json-navigator" "\
Navigate JSON inside region between START and END.
If START (respectively END) is nil, use `point-min' (respectively
`point-max') instead.

Interactively, if no region is active, use the whole buffer instead.

\(fn &optional START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "json-navigator" '("json-navigator-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; json-navigator-autoloads.el ends here
