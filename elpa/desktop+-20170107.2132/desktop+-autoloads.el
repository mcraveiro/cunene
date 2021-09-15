;;; desktop+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "desktop+" "desktop+.el" (0 0 0 0))
;;; Generated autoloads from desktop+.el

(autoload 'desktop+-create "desktop+" "\
Create a new session, identified by a name.
The session is created in a subdirectory of `desktop+-base-dir'.
It can afterwards be reloaded using `desktop+-load'.

As a special case, if NAME is left blank, the session is
automatically named after the current working directory.

\(fn NAME)" t nil)

(autoload 'desktop+-create-auto "desktop+" "\
Create a new session, identified by the current working directory.
The session is created in a subdirectory of `desktop+-base-dir'.
It can afterwards be reloaded using `desktop+-load'." t nil)

(autoload 'desktop+-load "desktop+" "\
Load a session previously created using `desktop+-create'.
NAME is the name which was given at session creation.  When
called interactively, it is asked in the minibuffer with
auto-completion.

As a special case, if NAME is left blank, the session is
automatically named after the current working directory.

\(fn NAME)" t nil)

(autoload 'desktop+-load-auto "desktop+" "\
Load a session previously created using `desktop+-create-auto'.
The session is identified by the current working directory." t nil)

(autoload 'desktop+--advice--desktop-save "desktop+" "\
Also save special buffers.

\(fn &rest ARGS)" nil nil)

(advice-add 'desktop-save :before #'desktop+--advice--desktop-save)

(autoload 'desktop+--advice--desktop-restore-frameset "desktop+" "\
Restore special buffers.

\(fn &rest ARGS)" nil nil)

(advice-add 'desktop-restore-frameset :before #'desktop+--advice--desktop-restore-frameset)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "desktop+" '("desktop+-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; desktop+-autoloads.el ends here
