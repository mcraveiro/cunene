;;; consult-yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult-yasnippet" "consult-yasnippet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from consult-yasnippet.el

(autoload 'consult-yasnippet-visit-snippet-file "consult-yasnippet" "\
Visit the snippet file associated with TEMPLATE.
When called interactively this command previews snippet completions in
the current buffer, and then opens the selected snippets template file
using `yas--visit-snippet-file-1'.

\(fn TEMPLATE)" t nil)

(autoload 'consult-yasnippet "consult-yasnippet" "\
Interactively select and expand the yasnippet template TEMPLATE.
When called interactively this command presents a completing read interface
containing all currently available snippet expansions, with live previews for
each snippet. Once selected a chosen snippet will be expanded at point using
`yas-expand-snippet'.

\(fn TEMPLATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-yasnippet" '("consult-yasnippet--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-yasnippet-autoloads.el ends here
