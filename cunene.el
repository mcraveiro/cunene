;;; cunene.el --- Cunene: My emacs configuration
;;
;; Copyright © 2021 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; General editor configuration

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(setq user-full-name "Marco Craveiro")
(setq user-mail-address "marco.craveiro@gmail.com")

(defconst cunene/cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Directory where all cache files should be saved")

(defun cunene/cache-concat (name)
  "Return the absolute path of NAME under `cunene/cache-directory'."
  (let* ((directory (file-name-as-directory cunene/cache-directory))
         (path (convert-standard-filename (concat directory name))))
    (make-directory (file-name-directory path) t)
    path))

(with-eval-after-load 'request
  (setq-default request-storage-directory (cunene/cache-concat "request/")))
(with-eval-after-load 'tramp
  (setq-default tramp-persistency-file-name (cunene/cache-concat "tramp.eld")))
(with-eval-after-load 'url
  (setq-default url-configuration-directory (cunene/cache-concat "url/")))

;; Moving the location of packages causes weird bootstrapping errors.
;; (with-eval-after-load 'package
;;   (setq-default package-user-dir (cunene/cache-concat "packages/")))

(defvar cunene/backup-directory (cunene/cache-concat "backups"))

(if (not (file-exists-p cunene/backup-directory))
    (make-directory cunene/backup-directory t))

(setq backup-directory-alist `(("." . ,cunene/backup-directory)))

(setq make-backup-files t               ;; Backup of a file the first time it is saved.
      backup-by-copying t               ;; Don't clobber symlinks.
      version-control t                 ;; Version numbers for backup files.
      vc-make-backup-files t            ;; Backup files even if under version control.
      delete-old-versions t             ;; delete excess backup files silently.
      delete-by-moving-to-trash t       ;; Move deleted files to trash.
      kept-old-versions 6               ;; oldest versions to keep when a new numbered backup is made
      kept-new-versions 9               ;; newest versions to keep when a new numbered backup is made
      auto-save-default t               ;; auto-save every buffer that visits a file
      auto-save-timeout 20              ;; number of seconds idle time before auto-save
      auto-save-interval 200)           ;; number of keystrokes between auto-saves

;; sensitive data
(setq auto-mode-alist
      (append
       (list
        '("\\.\\(vcf\\|gpg\\)$" . sensitive-minor-mode)
        )
       auto-mode-alist))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
      ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(with-eval-after-load 'use-package
  (setq-default
   use-package-always-defer nil     ;; Let auto-loading be managed by package.el
   use-package-always-ensure t))    ;; Install packages if not present in the system

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(eval-when-compile
  (require 'use-package))

(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinition
 auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
 delete-by-moving-to-trash t            ; Delete files to trash
 fill-column 80                         ; Set width for automatic line breaks
 help-window-select t                   ; Focus new help windows when opened
 indent-tabs-mode nil                   ; Stop using tabs to indent
 inhibit-startup-screen t               ; Disable start-up screen
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                  ; Yank at point rather than pointer
 read-process-output-max (* 1024 1024)  ; Increase read size per process
 recenter-positions '(5 top bottom)     ; Set re-centering positions
 scroll-conservatively 101              ; Avoid recentering when scrolling far
 scroll-margin 2                        ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil          ; Use a single space after dots
 show-help-function nil                 ; Disable help text everywhere
 tab-always-indent 'complete            ; Tab indents first then tries completions
 tab-width 4                            ; Smaller width for tab characters
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 warning-minimum-level :error           ; Skip warning buffers
 window-combination-resize t            ; Resize windows proportionally
 x-stretch-cursor t)                    ; Stretch cursor to the glyph width
(blink-cursor-mode 0)                   ; Prefer a still cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)           ; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding

(setq-default
 gc-cons-threshold (* 8 1024 1024))      ; Bump up garbage collection threshold.

(add-function :after after-focus-change-function
  (defun cunene/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
)

(pcase window-system
  ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
  (_ (set-frame-parameter nil 'fullscreen 'maximized)))

(require 're-builder)
(setq reb-re-syntax 'string)        ;; No need for double-slashes

(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
   non-nil optional argument DELIMITED, only replace matches
   surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
	 (re-printed (with-output-to-string (print re)))
	 (replacement (read-from-minibuffer
		       (format "Replace regexp %s with: "
			       (substring re-printed 1
					  (1- (length re-printed)))))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp re replacement delimited))))

(define-key reb-mode-map (kbd "C-M-%") 'reb-replace-regexp)

(use-package which-key
  :config
  (which-key-mode))

(use-package git-commit
  :hook
  (git-commit-mode . (lambda () (setq-local fill-column 72))))

(use-package git-gutter-fringe
  :preface
  (defun cunene/git-gutter-enable ()
    (when-let* ((buffer (buffer-file-name))
                (backend (vc-backend buffer)))
      (require 'git-gutter)
      (require 'git-gutter-fringe)
      (git-gutter-mode 1)))
  :hook
  (after-change-major-mode . cunene/git-gutter-enable)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [255] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [255 255 255 255] nil nil 'bottom)
  (define-fringe-bitmap 'git-gutter-fr:modified [255] nil nil '(center t)))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package magit
  :bind
  (:map magit-file-section-map
   ("<return>" . magit-diff-visit-file-other-window)
   :map magit-hunk-section-map
   ("<return>" . magit-diff-visit-file-other-window)
   :map magit-status-mode-map
   ("M-1" . nil)
   ("M-2" . nil)
   ("M-3" . nil)
   ("M-4" . nil))
  :hook
  (magit-post-stage-hook . cunene/magit-recenter)
  :custom
  (epg-pinentry-mode 'loopback)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-using-face))
  (magit-diff-refine-hunk 'all)
  (magit-module-sections-nested nil)
  (magit-section-initial-visibility-alist
   '((modules . show) (stashes . show) (unpulled . show) (unpushed . show)))
  :config
  (magit-add-section-hook
   'magit-status-sections-hook 'magit-insert-modules-overview 'magit-insert-merge-log)
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight))

(defun cunene/magit-recenter ()
  "Recenter the current hunk at 25% from the top of the window."
  (when (magit-section-match 'hunk)
    (let ((top (max 0 scroll-margin (truncate (/ (window-body-height) 4)))))
      (message "%s" top)
      (save-excursion
        (magit-section-goto (magit-current-section))
        (recenter top)))))

(use-package pinentry
  :hook
  (after-init . pinentry-start))

(use-package transient
  :init
  (setq-default
   transient-history-file (cunene/cache-concat "transient/history.el")
   transient-levels-file (cunene/cache-concat "transient/levels.el")
   transient-values-file (cunene/cache-concat "transient/values.el"))
  :custom
  (transient-default-level 5)
  (transient-mode-line-format nil))

;;; cunene.el ends here
