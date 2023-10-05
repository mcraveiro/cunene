(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(org-agenda-files
   '("/work/DomainDrivenConsulting/masd/dogen/integration/doc/agile/v1/sprint_backlog_33.org"))
 '(package-selected-packages
   '(chatgpt-shell undo-tree org-roam-ui org-roam-protocol doom-themes drag-stuff ssh helm-c-yasnippet google-this treemacs-projectile lsp-treemacs eshell-git-prompt expand-region windswap ibuffer-projectile rainbow-mode ztree smart-hungry-delete color-identifiers-mode inf-mongo logview consult-yasnippet prodigy consult-flycheck which-key define-word diff-at-point json-mode git-timemachine browse-kill-ring git-gutter-fringe yaml-mode mastodon aggressive-indent helpful use-package marginalia jq-format doom-modeline bm codegpt backup-walker ox-tufte lsp-ui bongo org-ql treemacs-magit engine-mode dimmer jq-mode rainbow-delimiters super-save verb treemacs-persp flycheck-plantuml org-present cmake-mode helm-ls-git consult-dir powershell eyebrowse hl-todo git-messenger epc sql-clickhouse crux beacon jump-tree orderless synosaurus vertico treemacs-icons-dired persistent-scratch msgu company-box yasnippet-snippets anzu rg dashboard company-posframe hide-mode-line diminish treemacs-evil org-ref-prettify smartparens mustache treemacs-all-the-icons volatile-highlights org-fancy-priorities))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "cmake --build --preset linux-clang-release")
     (projectile-project-test-cmd . "cmake --build --preset linux-clang-release --target rat")
     (projectile-project-run-cmd . "cmake --build --preset linux-clang-release --target gao")
     (yaml-tab-width . 4)
     (projectile-project-compilation-cmd . "cmake --preset linux-clang-release && cmake --build --preset linux-clang-release")
     (projectile-project-compilation-cmd . "export LD_LIBRARY_PATH=~/local/odb/lib && export PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH && export CMAKE_PROGRAM_PATH=~/local/odb/bin:/work/DomainDrivenConsulting/masd/dogen/integration/build/output/clang9/Release/stage/bin && export CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake && /work/DomainDrivenConsulting/masd/cpp_ref_impl/integration/build/scripts/build.linux.sh Release 6 clang10")
     (cmake-tab-width . 4)
     (org-roam-directory . "/work/DomainDrivenConsulting/masd/dogen/integration")
     (projectile-project-compilation-cmd . "FROZEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/frozen/master/projects DOGEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/dogen/integration/projects CPP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/cpp_ref_impl/master/projects CSHARP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/csharp_ref_impl/master/Src PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake /work/DomainDrivenConsulting/masd/dogen/integration/build/scripts/build.linux.sh Release 6 clang11")
     (projectile-project-test-cmd . "FROZEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/frozen/master/projects DOGEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/dogen/integration/projects CPP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/cpp_ref_impl/master/projects CSHARP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/csharp_ref_impl/master/Src PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake /work/DomainDrivenConsulting/masd/dogen/integration/build/scripts/build.linux.sh Release 6 clang11 rat")
     (projectile-project-run-cmd . "FROZEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/frozen/master/projects DOGEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/dogen/integration/projects CPP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/cpp_ref_impl/master/projects CSHARP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/csharp_ref_impl/master/Src PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake /work/DomainDrivenConsulting/masd/dogen/integration/build/scripts/build.linux.sh Release 6 clang11 gao")
     (projectile-project-name . "dogen - integration")
     (projectile-project-type . "cmake")
     (flycheck-disabled-checkers emacs-lisp-checkdoc)))
 '(super-save-mode t)
 '(treemacs-tag-follow-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 5.0))))
 '(header-line ((t (:inherit mode-line :background "#2b2b2b" :foreground "#f0dfaf" :box (:line-width -1 :color "#252525" :style released-button))))))
