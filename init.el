;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Created: February 22, 2021
;; Homepage: https://github.com/mcraveiro/cunene

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(setq-default
 load-prefer-newer t                    ;; Load newest version of lisp code.
 ;; mode-line-format nil                ;; No mode line format
 package-enable-at-startup nil          ;; Use installed packages after init file.
 package-native-compile t               ;; Use native compilation.
 org-src-preserve-indentation t         ;; Do not add indentation to src blocks.
 )

(setq-default
 default-frame-alist
 '(
   ;; (background-color . "#101010")       ;; Default background color
   ;; (foreground-color . "#FAFAFA")       ;; Default foreground color
   (font . "Source Code Pro 9")           ;; Font to use
   ;; (fullscreen . maximized)             ;; Maximize the window by default
   (horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   ;; (left-fringe . 8)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ;; No menu bar
   ;; (right-divider-width . 1)            ;; Thin vertical window divider
   ;; (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   ;; (undecorated . t)                    ;; Remove extraneous X decorations
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;; (set-frame-font "Source Code Pro 9")
;; (set-frame-font "Hack 8")
;; (set-frame-font "Menlo 8")
;; (set-frame-font "FiraCode 8")

;;
;; All packages we could not find in the repositories are stored here.
;;
(defvar cunene/vendor-packages
  (expand-file-name "~/.emacs.d/vendor")
  "Location for third-party packages.")

;;
;; Hydra method, needs to evaluate before we can load the main init.
;;
(defun cunene/hydra-heading (&rest headings)
  "Format HEADINGS to look pretty in a hydra docstring."
  (concat "\n "
          (mapconcat (lambda (heading)
                       (propertize (format "%-18s" heading) 'face 'shadow))
                     headings
                     nil)))

;; FIXME: whilst debugging package install problems.
;; (require 'package)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq package-archives
;;       '(
;;         ("gnu" . "https://elpa.gnu.org/packages/")
;;         ;; ("gnu" . "https://mirrors.163.com/elpa/gnu/")
;; 	("melpa" . "https://melpa.org/packages/"))
;;         ;; ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
;;         ;; ("melpa" . "https://melpa.org/packages/"))
;;       )

;; (package-initialize)


;; Configuration specific to startup.

(let
    (
     (default-directory user-emacs-directory) ;; FIXME: Start in the emacs directory.
     (file-name-handler-alist nil)      ;; Remove special handlers on startup.
     (gc-cons-percentage .6)
     (gc-cons-threshold most-positive-fixnum)
     (read-process-output-max (* 1024 1024))
     )

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(org-babel-tangle t))))
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Tangle and compile if necessary only, then load the configuration
  (let* ((.org "cunene.org")
         (.el (concat (file-name-sans-extension .org) ".el"))
         (modification-time
          (file-attribute-modification-time (file-attributes .org))))
    (require 'org-macs)
    (unless (org-file-newer-than-p .el modification-time)
      (require 'ob-tangle)
      (org-babel-tangle-file .org .el "emacs-lisp")
      (byte-compile-file .el))
    (load-file .el))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect)
  )

;;; init.el ends here
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes drag-stuff ssh helm-c-yasnippet google-this treemacs-projectile lsp-treemacs eshell-git-prompt expand-region windswap ibuffer-projectile rainbow-mode ztree smart-hungry-delete color-identifiers-mode inf-mongo logview consult-yasnippet prodigy consult-flycheck which-key define-word diff-at-point json-mode git-timemachine browse-kill-ring git-gutter-fringe yaml-mode mastodon aggressive-indent helpful use-package marginalia jq-format doom-modeline bm codegpt backup-walker ox-tufte lsp-ui bongo org-ql treemacs-magit engine-mode dimmer jq-mode rainbow-delimiters super-save verb treemacs-persp flycheck-plantuml org-present cmake-mode helm-ls-git consult-dir powershell eyebrowse hl-todo git-messenger epc sql-clickhouse crux beacon jump-tree orderless synosaurus vertico treemacs-icons-dired persistent-scratch msgu company-box yasnippet-snippets anzu rg dashboard company-posframe hide-mode-line diminish treemacs-evil org-ref-prettify smartparens mustache treemacs-all-the-icons volatile-highlights org-fancy-priorities)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
