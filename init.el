;;; init.el --- Cunene: My emacs configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2024 Marco Craveiro

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
 mode-line-format nil                   ;; No mode line format
 package-enable-at-startup nil          ;; Use installed packages after init file.
 package-native-compile t               ;; Use native compilation.
 org-src-preserve-indentation t         ;; Do not add indentation to src blocks.
 message-log-max 100000                 ;; Keep a lot more messages
 )

(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)

;; (set-frame-font "Inconsolata 11" nil t)
;; (set-frame-font "Fira Mono 10" nil t)
;; (set-frame-font "Hack 9" nil t)
;; (set-frame-font "JetBrainsMono Nerd Font 10" nil t)

(setq-default
 default-frame-alist
 '(
   (font . "Hack 9")                    ;; Font to use
   (fullscreen . fullboth)              ;; Maximize the window by default
   (horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (left-fringe . 8)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;;
;; Org-mode configuration location
;;
(defvar cunene/org-config
  (expand-file-name "~/.emacs.d/config")
  "Location for `org-mode' configuration.")
(add-to-list 'load-path cunene/org-config)

;;
;; All packages we could not find in the repositories are stored here.
;;
(defvar cunene/vendor-packages
  (expand-file-name "~/.emacs.d/vendor")
  "Location for third-party packages.")
(add-to-list 'load-path cunene/vendor-packages)

;;
;; Any code which we do not want to push to the public repo goes under here.
;;
(defvar cunene/site-lisp
  (expand-file-name "~/.emacs.d/site-lisp")
  "Location for site specific packages.")
(add-to-list 'load-path cunene/site-lisp)

(defun cunene/compile-and-load-file (el-file-name &optional compile-only)
  "Compile EL-FILE-NAME into ELC and load it unless COMPILE-ONLY."
  (interactive)
  (let ((elc-file-name (concat (file-name-sans-extension el-file-name) ".elc")))
    (unless (file-newer-than-file-p elc-file-name el-file-name)
      (save-restriction
        (setq byte-compile-warnings
              '(not free-vars obsolete unresolved callargs redefine
                  obsolete noruntime cl-warnings interactive-only))
        (byte-compile-file el-file-name)))
    (unless compile-only
      (message (concat "Loading " elc-file-name "..."))
      (load-file elc-file-name))
    ))

;; For tangling.
(require 'org-macs)
(require 'ob-tangle)

;; Default to utf-8 encoding. Avoid problems with tangling.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun cunene/tangle-and-load-file (org-file-name)
  "Tangle and load an org file.
ORG-FILE-NAME file to operate on."
  (let ((el-file-name (concat (file-name-sans-extension org-file-name) ".el"))
        (modification-time
         (file-attribute-modification-time (file-attributes org-file-name))))
    (message (concat "Tangling " org-file-name "..."))
    (unless (org-file-newer-than-p el-file-name modification-time)
      (org-babel-tangle-file org-file-name el-file-name "emacs-lisp"))
    (cunene/compile-and-load-file el-file-name)))

(defun cunene/files-for-extension (dir extension)
  "Return all files at DIR that have EXTENSION, if any."
  (if (file-directory-p dir)
      (directory-files-recursively dir extension)))

;; FIXME: when troubleshooting package issues
;; (require 'package)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; (package-initialize)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(let
    (
     ;; Configuration specific to startup.
     ;; For notes on GC, see:
     ;; - https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
     (default-directory user-emacs-directory)  ;; Start in the emacs directory.
     (file-name-handler-alist nil)             ;; Remove special handlers on startup.
     (gc-cons-percentage .6)
     (gc-cons-threshold most-positive-fixnum)
     (read-process-output-max (* 1024 1024)))

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(org-babel-tangle t))))
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Compile vendor packages but do not load; Loading is done by org
  ;; configuration.
  (mapc
   (lambda (vendor-file) (cunene/compile-and-load-file vendor-file t))
   (cunene/files-for-extension cunene/vendor-packages ".el$"))

  ;; Tangle and compile if necessary, then load the configuration
  (mapc 'cunene/tangle-and-load-file
        (cunene/files-for-extension cunene/org-config ".org$"))

  ;; Load site-specific lisp code, if any exists.
  (mapc 'cunene/compile-and-load-file
        (cunene/files-for-extension cunene/site-lisp ".el$"))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(docker ztree yasnippet-snippets yaml-mode windswap vscode-icon volatile-highlights vertico verb undo-tree treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired terraform-mode super-save ssh sql-clickhouse smartparens smart-hungry-delete sharper rg rainbow-mode rainbow-delimiters protobuf-mode project-shells prodigy powershell persistent-scratch pcmpl-args paimon org-roam-ui org-present org-fancy-priorities orderless nerd-icons-corfu mustache-mode mustache msgu mastodon markdown-mode marginalia logview llama-cpp jump-tree json-mode jq-mode jq-format inf-mongo inf-clojure imenu-list iedit ibuffer-vc ibuffer-sidebar ibuffer-projectile ibuffer-project ibuffer-git hl-todo hide-mode-line helpful haproxy-mode google-this git-timemachine git-modes git-messenger git-gutter-fringe flyspell-correct flycheck-plantuml flycheck-eglot eyebrowse expand-region evil eshell-git-prompt engine-mode embark-consult ellama eldoc-box drag-stuff doom-themes doom-modeline dockerfile-mode dired-sidebar dimmer diminish diff-at-point define-word dashboard csv-mode csproj-mode crux corfu consult-yasnippet consult-flyspell consult-flycheck consult-eglot consult-dir company-posframe company-box color-identifiers-mode cmake-mode citeproc-org chatgpt-shell cape browse-kill-ring bongo bm beacon backup-walker anzu all-the-icons-nerd-fonts all-the-icons-ibuffer all-the-icons-dired all-the-icons-completion aggressive-indent))
 '(safe-local-variable-values
   '((eval setq-local org-roam-db-location
	   (expand-file-name ".org-roam.db" org-roam-directory))
     (eval setq-local org-roam-directory
	   (expand-file-name
	    (locate-dominating-file default-directory ".dir-locals.el"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
