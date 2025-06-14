(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/marco/.emacs.d/bookmarks")
 '(org-agenda-files
   '("~/Development/IonaRisk/doc/agile/v0/sprint_backlog_01.org"
     "/home/marco/Development/OreStudio/doc/agile/v0/sprint_backlog_02.org"
     "/home/marco/Development/OreStudio/doc/agile/v0/sprint_backlog_01.org"))
 '(package-selected-packages
   '(aggressive-indent all-the-icons-completion all-the-icons-dired
                       all-the-icons-ibuffer all-the-icons-nerd-fonts anzu
                       backup-walker beacon bm bongo browse-kill-ring cape
                       chatgpt-shell citeproc-org cmake-mode
                       color-identifiers-mode company-box company-posframe
                       consult-dir consult-eglot consult-flycheck
                       consult-flyspell consult-gh consult-yasnippet corfu crux
                       csproj-mode csv-mode dashboard define-word diff-at-point
                       diminish dimmer dired-sidebar docker dockerfile-mode
                       doom-modeline doom-themes dotnet drag-stuff eldoc-box
                       ellama embark-consult engine-mode eshell-git-prompt evil
                       expand-region eyebrowse flycheck-eglot flycheck-plantuml
                       flyspell-correct git-gutter-fringe git-messenger
                       git-modes git-timemachine google-this haproxy-mode
                       helpful hide-mode-line hl-todo ibuffer-git
                       ibuffer-project ibuffer-projectile ibuffer-sidebar
                       ibuffer-vc iedit imenu-list inf-clojure inf-mongo
                       jq-format jq-mode json-mode jump-tree llama-cpp logview
                       marginalia markdown-mode mastodon msgu mustache
                       mustache-mode nerd-icons-corfu orderless
                       org-fancy-priorities org-present org-roam-ui paimon
                       pcmpl-args persistent-scratch powershell prodigy
                       project-shells protobuf-mode rainbow-delimiters
                       rainbow-mode redis rg sharper smart-hungry-delete
                       smartparens sql-clickhouse ssh super-save terraform-mode
                       treemacs-icons-dired treemacs-magit treemacs-persp
                       treemacs-projectile treemacs-tab-bar undo-tree verb
                       vertico volatile-highlights vscode-icon web-mode windswap
                       yaml-mode yasnippet-snippets ztree))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(safe-local-variable-values
   '((checkdoc-allow-quoting-nil-and-t . t)
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
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 5.0))))
 '(dired-sidebar-face ((t (:height 0.8 :foundry "Helvetica"))) t))
