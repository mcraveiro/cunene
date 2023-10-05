;;; codegpt-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from codegpt.el

(autoload 'codegpt-mode "codegpt" "\
Major mode for `codegpt-mode'.

\\<codegpt-mode-map>

(fn)" t)
(autoload 'codegpt-doc "codegpt" "\
Automatically write documentation for your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer.

(fn START END)" t)
(autoload 'codegpt-fix "codegpt" "\
Fix your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer.

(fn START END)" t)
(autoload 'codegpt-explain "codegpt" "\
Explain the selected code.

This command is interactive region only, the START and END are boundaries of
that region in buffer.

(fn START END)" t)
(autoload 'codegpt-improve "codegpt" "\
Improve, refactor or optimize your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer.

(fn START END)" t)
(autoload 'codegpt-custom "codegpt" "\
Do completion with custom instruction.

This command is interactive region only, the START and END are boundaries of
that region in buffer.

(fn START END)" t)
(autoload 'codegpt "codegpt" "\
Do completion with OpenAI to your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer.

(fn START END)" t)
(register-definition-prefixes "codegpt" '("codeg"))

;;; End of scraped data

(provide 'codegpt-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; codegpt-autoloads.el ends here
