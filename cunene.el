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
  "Directory where all cache files should be saved.")

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
 gc-cons-threshold (* 8 1024 1024))      ; Bump up garbage collection threshold.

(add-function :after after-focus-change-function
  (defun cunene/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

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

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(use-package which-key
  :config
  (which-key-mode))

(pcase window-system
  ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
  (_ (set-frame-parameter nil 'fullscreen 'maximized)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
)

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
  :config (setq doom-modeline-buffer-file-name-style 'relative-to-project)

(use-package diminish)

(defvar-local cunene/hydra-super-body nil)

(defun cunene/hydra-set-super ()
  "Set the super key for hydra."
  (when-let* ((suffix "-mode")
              (position (- (length suffix)))
              (mode (symbol-name major-mode))
              (name (if (string= suffix (substring mode position))
                        (substring mode 0 position)
                      mode))
              (body (intern (format "hydra-%s/body" name))))
    (when (functionp body)
      (setq cunene/hydra-super-body body))))

(defun cunene/hydra-super-maybe ()
  "Set super conditionally."
  (interactive)
  (if cunene/hydra-super-body
      (funcall cunene/hydra-super-body)
    (user-error "Error: cunene/hydra-super: cunene/hydra-super-body is not set")))

(use-package hydra
  :bind
  ("C-c a" . hydra-applications/body)
  ("C-c d" . hydra-dates/body)
  ("C-c e" . hydra-eyebrowse/body)
  ("C-c f" . hydra-spotify/body)
  ("C-c g" . hydra-git/body)
  ("C-c o" . cunene/hydra-super-maybe)
  ("C-c p" . hydra-projectile/body)
  ("C-c s" . hydra-system/body)
  ("C-c u" . hydra-ui/body)
  :custom
  (hydra-default-hint nil))

(defhydra hydra-applications (:color teal)
  (concat (cunene/hydra-heading "Applications" "Launch" "Shell") "
 _q_ quit            _i_ erc             _T_ eshell             ^^
")
  ("q" nil)
  ("i" erc)
  ("T" (eshell t)))

(require 're-builder)
(setq reb-re-syntax 'string)        ;; No need for double-slashes

(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of `re-builder'.

With non-nil optional argument DELIMITED, only replace matches
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

;; Dired switches
(setq-default dired-listing-switches "-l")
(setq-default list-directory-brief-switches "-CF")

(add-hook
 'dired-before-readin-hook
 '(lambda ()
    (when (file-remote-p default-directory)
      (setq dired-actual-switches "-l"))))

(use-package ibuffer
  :bind
  (:map ibuffer-mode-map
	("/ e" . ibuffer-filter-by-ede-project)
	("% e" . ibuffer-mark-by-ede-project-regexp)
	("s e" . ibuffer-do-sort-by-ede-project))
  :config
  (progn
    (global-set-key (kbd "<f5>") 'ibuffer) ;; Shortcut for ibuffer
    (when (display-graphic-p) ;; Display buffer icons on GUI
      (define-ibuffer-column icon (:name " ")
	(let ((icon (if (and buffer-file-name
                             (all-the-icons-match-to-alist buffer-file-name
                                                           all-the-icons-regexp-icon-alist))
			(all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
                                                     :height 0.9 :v-adjust -0.05)
                      (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
          (if (symbolp icon)
              (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
            icon))))
    (add-hook 'ibuffer-mode-hook ;; Setup filter groups
              '(lambda ()
		 (ibuffer-auto-mode 1)
		 (ibuffer-switch-to-saved-filter-groups "home")
		 (ibuffer-do-sort-by-filename/process))))

  :custom
  (ibuffer-formats '((mark modified read-only locked
                           " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                           " " (size 9 -1 :right)
                           " " (mode 16 16 :left :elide) " " filename-and-process)
                     (mark " " (name 16 -1) " " filename)))
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (ibuffer-show-empty-filter-groups nil) ;; Remove empty groups
  (ibuffer-expert t) ;; Enable expert mode
  (ibuffer-saved-filter-groups ;; Group buffers
   (quote (("home"
            ("c++" (mode . c++-mode))
            ("python" (or
                       (mode . python-mode)
                       (name . "^\\*Python\\*$")))
            ("fsharp" (or
                       (mode . inferior-fsharp-mode)
                       (mode . fsharp-mode)))
            ("csharp" (mode . csharp-mode))
            ("java" (mode . java-mode))
            ("kotlin" (mode . kotlin-mode))
            ("ruby" (mode . ruby-mode))
            ("perl" (mode . perl-mode))
            ("json" (mode . json-mode))
            ("javascript" (or
                           (mode . javascript-mode)
                           (mode . js2-mode)
                           (mode . js-mode)))
            ("php" (mode . php-mode))
            ("org" (mode . org-mode))
            ("xml" (mode . nxml-mode))
            ("sql" (or
                    (mode . sql-mode)
                    (name . "^\\*SQL")))
            ("make" (or
                     (mode . cmake-mode)
                     (mode . makefile-mode)
                     (mode . makefile-gmake-mode)))
            ("t4" (name . ".tt$"))
            ("Dogen - Stitch" (or
                               (mode . headtail-mode)
                               (name . ".stitch$")))
            ("bash" (mode . sh-mode))
            ("awk" (mode . awk-mode))
            ("latex" (or
                      (name . ".tex$")
                      (name . ".texi$")
                      (mode . tex-mode)
                      (mode . latex-mode)))
            ("markdown" (or
                         (mode . markdown-mode)
                         (mode . gfm-mode)))
            ("emacs-lisp" (or
                           (mode . emacs-lisp-mode)
                           (name . "^\\*Compile-Log\\*$")))
            ("powershell" (or
                           (mode . powershell-mode)
                           (name . "^\\*PowerShell")))
            ("logs" (or
                     (mode . log4j-mode)
                     (mode . logview-mode)))
            ("grep" (or
                     (name . "^\\*Occur\\*$")
                     (name . "^\\*Moccur\\*$")
                     (mode . grep-mode)))
            ("irc" (or
                    (mode . erc-list-mode)
                    (mode . erc-mode)))
            ("shell" (or
                      (name . "^\\*Shell Command Output\\*$")
                      (mode . shell-mode)
                      (mode . ssh-mode)
                      (mode . eshell-mode)
                      (name . "^\\*compilation\\*$")))
            ("file management" (or
                                (mode . dired-mode)
                                (mode . tar-mode)))
            ("org" (mode . org-mode-))
            ("text files" (or
                           (mode . conf-unix-mode)
                           (mode . conf-space-mode)
                           (mode . text-mode)))
            ("yaml" (mode . yaml-mode))
            ("msdos" (mode . dos-mode))
            ("patches" (or
                        (name . "^\\*Assoc file dif")
                        (mode . diff-mode)))
            ("version control" (or
                                (name . "^\\*svn-")
                                (name . "^\\*vc")
                                (name . "^\\*cvs")
                                (name . "^\\magit")))
            ("snippets" (mode . snippet-mode))
            ("semantic" (or
                         (mode . data-debug-mode)
                         (name . "^\\*Parser Output\\*$")
                         (name . "^\\*Lexer Output\\*$")))
            ("web browsing" (or
                             (mode . w3m-mode)
                             (mode . twittering-mode)))
            ("music" (or
                      (mode . bongo-playlist-mode)
                      (mode . bongo-library-mode)))
            ("mail" (or
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode)
                     (name . "^\\*imap log\\*$")
                     (name . "^\\*gnus trace\\*$")
                     (name . "^\\*nnimap imap.")))
            ("web development" (or
                                (mode . html-mode)
                                (mode . css-mode)))
            ("documentation" (or
                              (mode . Info-mode)
                              (mode . apropos-mode)
                              (mode . woman-mode)
                              (mode . help-mode)
                              (mode . Man-mode)))
            ("system" (or
                       (name . "^\\*Packages\\*$")
                       (name . "^\\*helm M-x\\*$")
                       (name . "^\\*helm mini\\*$")
                       (name . "^\\*helm projectile\\*$")
                       (name . "^\\*RTags Log\\*$")
                       (name . "^\\**RTags Diagnostics\\*$")
                       (name . "^\\*tramp")
                       (name . "^\\**input/output of")
                       (name . "^\\**threads of")
                       (name . "^\\**breakpoints of")
                       (name . "^\\**Flycheck")
                       (name . "^\\**sx-search-result*")
                       (name . "^\\**gud-dogen.knit")
                       (name . "^\\**Warnings*")
                       (name . "^\\*debug tramp")
                       (name . "^\\*Proced log\\*$")
                       (name . "^\\*Ediff Registry\\*$")
                       (name . "^\\*Bookmark List\\*$")
                       (name . "^\\*RE-Builder\\*$")
                       (name . "^\\*Kill Ring\\*$")
                       (name . "^\\*Calendar\\*$")
                       (name . "^\\*icalendar-errors\\*$")
                       (name . "^\\*Proced\\*$")
                       (name . "^\\*WoMan-Log\\*$")
                       (name . "^\\*Apropos\\*$")
                       (name . "^\\*Completions\\*$")
                       (name . "^\\*Help\\*$")
                       (name . "^\\*Dired log\\*$")
                       (name . "^\\*scratch\\*$")
                       (name . "^\\*gnuplot\\*$")
                       (name . "^\\*Flycheck errors\\*$")
                       (name . "^\\*compdb:")
                       (name . "^\\*Backtrace\\*$")
                       (name . "^\\*Messages\\*$")))
            ("Treemacs" (or
                         (name . "^Treemacs Update")
                         (name . "^\\*nnimap imap.")))
            )))))

(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "s-W") #'kill-this-buffer)

;; (use-package desktop+
;;   :ensure t
;;   :commands (desktop-create desktop-load)
;;   :init
;;   (eval-after-load "desktop+"
;;     '(defun desktop+--set-frame-title ()
;;        (message "desktop+ set in initialization to not write to frame title")))
;;   :config
;;   (require 'desktop+)
;;   (setq desktop+-special-buffer-handlers
;;         '(org-agenda-mode shell-mode compilation-mode eshell-mode)))

;; (setq-default desktop+-base-dir (cunene/cache-concat "desktops/"))

;; could not get it to work via use-package; commands did not kick-in
;; and kept trying to reload from elpa.
(require 'desktop)
(desktop-save-mode 1)
(setq history-length 250
      desktop-base-file-name (cunene/cache-concat "desktop/desktop")
      desktop-base-lock-name (cunene/cache-concat "desktop/desktop.lock")
      desktop-restore-eager 4
      desktop-restore-forces-onscreen nil
      desktop-restore-frames t)
(add-to-list 'desktop-globals-to-save 'file-name-history)

(defun cunene/emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (cunene/emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(use-package shackle
  :hook
  (after-init . shackle-mode)
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules '((help-mode :same t)
                   (helpful-mode :same t)
                   (process-menu-mode :same t)))
  (shackle-select-reused-windows t))

(defun cunene/ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
The function FN wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(use-package windmove
  :ensure nil
  :bind
  (
   ([s-left] . windmove-left)
   ([s-down] . windmove-down)
   ([s-up] . windmove-up)
   ([s-right] . windmove-right)
   )
)

(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode))

(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
   ("<C-return>" . nil)
   ("<C-tab>" . cunene/org-cycle-parent))
  :hook
  (org-mode . cunene/hydra-set-super)
  :custom
  (org-adapt-indentation nil)
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-descriptive-links t)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  :config
  (require 'ob-shell)
  (add-to-list 'org-babel-load-languages '(shell . t))
  (modify-syntax-entry ?' "'" org-mode-syntax-table)
  (advice-add 'org-src--construct-edit-buffer-name :override #'cunene/org-src-buffer-name)
  (with-eval-after-load 'evil
    (evil-define-key* 'motion org-mode-map
      (kbd "C-j") #'cunene/org-show-next-heading-tidily
      (kbd "C-k") #'cunene/org-show-previous-heading-tidily)))

(defun cunene/org-cycle-parent (argument)
  "Go to the nearest parent heading and execute `org-cycle'.

ARGUMENT determines the visible heading."
  (interactive "p")
  (if (org-at-heading-p)
      (outline-up-heading argument)
    (org-previous-visible-heading argument))
  (org-cycle))

(defun cunene/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (outline-show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (user-error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

(defun cunene/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (outline-hide-subtree)
      (user-error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

(defun cunene/org-src-buffer-name (name &rest _)
  "Simple buffer name.
!NAME is the name of the buffer."
  (format "*%s*" name))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.5
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (cunene/cache-concat "treemacs/treemacs-persist")
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      t
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-text-scale                      -1
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t)) ;; enable cycling for `vertico-next' and `vertico-previous'.

;; from vendor directory.
(use-package vertico-quick
  :load-path cunene/vendor-packages
  :bind
  (:map vertico-map
        ("M-q" . vertico-quick-insert)
        ("C-q" . vertico-quick-exit)))

;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :custom
  (savehist-file (cunene/cache-concat "savehist/history")))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

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

(setq-default projectile-known-projects-file
              (cunene/cache-concat "projectile/bookmarks.eld"))
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package ibuffer-projectile
  :ensure t
  :after projectile
)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-indent-level 4)
  (image-auto-resize nil)
  :config
  (add-to-list 'plantuml-java-args "-DPLANTUML_LIMIT_SIZE=8192") ;; 65536
  (if (eq window-system 'w32)
      (setq plantuml-jar-path "C:/ProgramData/chocolatey/lib/plantuml/tools/plantuml.jar"
            plantuml-default-exec-mode 'jar)
    (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
          plantuml-default-exec-mode 'executable)))

(use-package flycheck-plantuml
  :ensure t
  :after (plantuml-mode flycheck)
  :init (flycheck-plantuml-setup)
)

(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

;; none of the use-package machinery seems to work with eshell, so we
;; do it manually instead via hooks.
(setq-default eshell-directory-name (cunene/cache-concat "eshell"))
(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'em-alias)
            (add-to-list 
             'eshell-command-aliases-list (list "ll" "ls -l"))
            (defalias 'ff 'find-file)
            (define-key eshell-mode-map (kbd "C-p") #'eshell-previous-matching-input-from-input)
            (define-key eshell-mode-map (kbd "C-n") #'eshell-next-matching-input-from-input)
            (define-key eshell-mode-map (kbd "<up>") #'previous-line)
            (define-key eshell-mode-map (kbd "<down>") #'next-line)))
(global-set-key (kbd "C-x m") 'eshell)

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

;;; cunene.el ends here
