(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mustache-mode slime slime-company clojure-mode doom-themes drag-stuff ssh helm-c-yasnippet google-this treemacs-projectile lsp-treemacs eshell-git-prompt expand-region windswap ibuffer-projectile rainbow-mode ztree smart-hungry-delete color-identifiers-mode inf-mongo logview consult-yasnippet prodigy consult-flycheck which-key define-word diff-at-point json-mode git-timemachine browse-kill-ring git-gutter-fringe yaml-mode mastodon aggressive-indent helpful use-package marginalia jq-format doom-modeline bm codegpt backup-walker ox-tufte lsp-ui bongo org-ql treemacs-magit engine-mode dimmer jq-mode rainbow-delimiters super-save verb treemacs-persp flycheck-plantuml org-present cmake-mode helm-ls-git consult-dir powershell eyebrowse hl-todo git-messenger epc sql-clickhouse crux beacon jump-tree orderless synosaurus vertico treemacs-icons-dired persistent-scratch msgu company-box yasnippet-snippets anzu rg dashboard company-posframe hide-mode-line diminish treemacs-evil org-ref-prettify smartparens mustache treemacs-all-the-icons volatile-highlights org-fancy-priorities))
 '(safe-local-variable-values
   '((org-html-validation-link)
     (org-roam-directory . "/work/DomainDrivenConsulting/masd/dogen/integration")
     (projectile-project-compilation-cmd . "cmake --build --preset linux-clang-release")
     (projectile-project-test-cmd . "cmake --build --preset linux-clang-release --target rat")
     (projectile-project-run-cmd . "cmake --build --preset linux-clang-release --target gao")
     (projectile-project-name . "dogen - integration")
     (projectile-project-type . "cmake")
     (eval setq-local org-roam-db-location
           (expand-file-name ".org-roam.db" org-roam-directory))
     (eval setq-local org-roam-directory
           (expand-file-name
            (locate-dominating-file default-directory ".dir-locals.el"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 5.0)))))
