;;; openai-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from openai.el

(register-definition-prefixes "openai" '("openai-"))


;;; Generated autoloads from openai-completion.el

(autoload 'openai-completion "openai-completion" "\
Query OpenAI with QUERY.

Argument CALLBACK is a function received one argument which is the JSON data.

(fn QUERY CALLBACK)")
(autoload 'openai-completion-select-insert "openai-completion" "\
Send the region to OpenAI and insert the result to the next paragraph.

START and END are selected region boundaries.

(fn START END)" t)
(autoload 'openai-completion-buffer-insert "openai-completion" "\
Send the entire buffer to OpenAI and insert the result to the end of buffer." t)
(register-definition-prefixes "openai-completion" '("openai-completon-"))


;;; Generated autoloads from openai-edit.el

(autoload 'openai-edit-prompt "openai-edit" "\
Prompt to ask for edited version." t)
(register-definition-prefixes "openai-edit" '("openai-edit-"))


;;; Generated autoloads from openai-embedding.el

(register-definition-prefixes "openai-embedding" '("openai-embedding-"))


;;; Generated autoloads from openai-engine.el

(autoload 'openai-list-engines "openai-engine" "\
List currently available (non-finetuned) models." t)
(register-definition-prefixes "openai-engine" '("openai-engine-"))


;;; Generated autoloads from openai-file.el

(autoload 'openai-list-files "openai-file" "\
List files that belong to the user's organization." t)
(autoload 'openai-upload-file "openai-file" "\
Prompt to upload the file to OpenAI server for file-tuning." t)
(autoload 'openai-delete-file "openai-file" "\
Prompt to select the file and delete it." t)
(autoload 'openai-retrieve-file "openai-file" "\
Prompt to select the file and print its' information." t)
(autoload 'openai-retrieve-file-content "openai-file" "\
Prompt to select the file and print its' content." t)
(register-definition-prefixes "openai-file" '("openai-"))


;;; Generated autoloads from openai-fine-tune.el

(autoload 'openai-list-fine-tunes "openai-fine-tune" "\
List fine-tuning jobs." t)
(register-definition-prefixes "openai-fine-tune" '("openai-fine-tune-"))


;;; Generated autoloads from openai-image.el

(autoload 'openai-image-prompt "openai-image" "\
Prompt to ask for image QUERY, and display result in a buffer.

(fn QUERY)" t)
(autoload 'openai-image-edit-prompt "openai-image" "\
Prompt to ask for image QUERY, and display result in a buffer.

(fn QUERY)" t)
(autoload 'openai-image-variation-prompt "openai-image" "\
Prompt to select an IMAGE file, and display result in a buffer.

(fn IMAGE)" t)
(register-definition-prefixes "openai-image" '("openai-"))


;;; Generated autoloads from openai-model.el

(autoload 'openai-retrieve-model "openai-model" "\
Retrieves a model instance, providing basic information about the model such
as the owner and permissioning." t)
(autoload 'openai-list-models "openai-model" "\
Lists the currently available models, and provides basic information about
each one such as the owner and availability." t)
(register-definition-prefixes "openai-model" '("openai-model"))


;;; Generated autoloads from openai-moderation.el

(register-definition-prefixes "openai-moderation" '("openai-moderation-"))

;;; End of scraped data

(provide 'openai-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; openai-autoloads.el ends here
