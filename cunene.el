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

(defvar cunene/config-file
  (concat user-emacs-directory "cunene.el")
  "The location of the generated cunene config file.")

(defvar cunene/config-file-org
  (concat user-emacs-directory "cunene.org")
  "The location of the cunene org-mode file.")

(defun cunene/find-config ()
  "Edit cunene's config file"
  (interactive)
  (find-file cunene/config-file-org))

(defun cunene/reload-config()
  "Reload config.org"
  (interactive)
  (delete-file cunene/config-file)
  (org-babel-load-file cunene/config-file-org))

(global-set-key (kbd "C-c I") 'cunene/find-config)
(global-set-key (kbd "C-c R") 'cunene/reload-config)

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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
 warning-minimum-level :error           ; Skip warning buffers
 window-combination-resize t            ; Resize windows proportionally
 vc-follow-symlinks t                   ; Follow symlinks without asking
 x-stretch-cursor t)                    ; Stretch cursor to the glyph width
(blink-cursor-mode 0)                   ; Prefer a still cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)           ; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(column-number-mode t)                  ; Display column numbers

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Do not ask to kill a buffer.
(global-set-key (kbd "C-x k") 'kill-this-buffer)


(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (list ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

(global-set-key (kbd "C-c C-=") 'diff-buffer-with-associated-file)

(defun de-context-kill (arg)
  "Kill buffer"
  (interactive "p")
  (if (and (buffer-modified-p)
             buffer-file-name
             (not (string-match "\\*.*\\*" (buffer-name)))
             ;; erc buffers will be automatically saved
             (not (eq major-mode 'erc-mode))
             (= 1 arg))
    (let ((differences 't))
      (when (file-exists-p buffer-file-name)
        (setq differences (diff-buffer-with-associated-file)))

      (if (y-or-n-p (format "Buffer %s modified; Kill anyway? " buffer-file-name))
          (progn
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer)))))
    (if (and (boundp 'gnuserv-minor-mode)
           gnuserv-minor-mode)
        (gnuserv-edit)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(global-set-key (kbd "C-x k") 'de-context-kill)

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 2
        which-key-show-early-on-C-h t
        which-key-idle-secondary-delay 0.05)
  :diminish
  which-key-mode)

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
  :config (setq doom-modeline-buffer-file-name-style 'buffer-name)

(use-package diminish)

;; Give details about white space usage
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options
  "whitespace" "Toggle local `whitespace-mode' options." t)

;; What to highlight
(setq whitespace-style
      '(face tabs trailing lines-tail space-before-tab empty space-after-tab
             tab-mark))

;; Indicate if empty lines exist at end of the buffer
(set-default 'indicate-empty-lines t)

;; do not use global mode whitespace
(global-whitespace-mode 0)
(setq whitespace-global-modes nil)

;; Show whitespaces on these modes
(add-hook 'sh-mode-hook 'whitespace-mode)
(add-hook 'snippet-mode-hook 'whitespace-mode)
(add-hook 'tex-mode-hook 'whitespace-mode)
(add-hook 'sql-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'diff-mode-hook 'whitespace-mode)
(add-hook 'c-mode-common-hook 'whitespace-mode)
(add-hook 'cmake-mode-hook 'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'dos-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'whitespace-mode)
(add-hook 'js2-mode-hook 'whitespace-mode)

;;
;; Tabs
;;
(defun untabify-buffer ()
  "Remove tabs from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (nreverse ls)))

;; Spaces only for indentation
(set-default 'indent-tabs-mode nil)

;; Tab size
(setq tab-width 4)
(setq standard-indent 4)
(setq tab-stop-list (build-tab-stop-list tab-width))
(setq tab-stop-list (build-tab-stop-list tab-width))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(use-package crux
  :ensure t
  :bind (
         ("C-S-d" . crux-duplicate-current-line-or-region)
         ;; Move to beginning of line between head of line and head of text
         ("C-a" . crux-move-beginning-of-line)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c D" . crux-delete-file-and-buffer)))

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
  (advice-add 'org-src--construct-edit-buffer-name :override #'cunene/org-src-buffer-name))

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

(use-package bm
  :ensure t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file (cunene/cache-concat "bm/bm-repository"))

  ;; show bookmark in fringe only.
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("<f9>" . bm-toggle)
         ("S-<f9>" . bm-previous)
         ("C-<f9>" . bm-next)))

(defvar bm-after-goto-hook nil
  "Hook run after jumping to a bookmark in `bm-goto'.")

(add-hook 'bm-after-goto-hook 'org-bookmark-jump-unhide)

(defun bm-goto (bookmark)
  "Goto specified BOOKMARK."
  (if (bm-bookmarkp bookmark)
      (progn
        (if bm-goto-position
            (goto-char (max
                        ;; sometimes marker-position is before start of overlay
                        ;; marker is not updated when overlay hooks are called.
                        (overlay-start bookmark)
                        (marker-position (overlay-get bookmark 'position))))
          (goto-char (overlay-start bookmark)))
        (run-hooks 'bm-after-goto-hook)
        (setq bm-wrapped nil)           ; turn off wrapped state
        (if bm-recenter
            (recenter))
        (let ((annotation (overlay-get bookmark 'annotation)))
          (if annotation
              (message annotation)))
        (when  (overlay-get bookmark 'temporary-bookmark)
          (bm-bookmark-remove  bookmark)))
    (when (> bm-verbosity-level 0)
      (message "Bookmark not found."))))

;; Highlight current line.
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(add-hook 'occur-mode-hook #'hl-line-mode)
(add-hook 'svn-status-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'grep-setup-hook #'hl-line-mode)
(add-hook 'compilation-mode-hook #'hl-line-mode)
(add-hook 'magit-mode-hook #'hl-line-mode)
(add-hook 'vc-git-log-view-mode-hook #'hl-line-mode)
(add-hook 'log-view-hook #'hl-line-mode)
(add-hook 'find-dired-mode-hook #'hl-line-mode)
(add-hook 'gnus-summary-mode-hook #'hl-line-mode)
(add-hook 'org-agenda-finalize-hook #'hl-line-mode)

;; Turn on local highlighting for list-buffers
(defadvice list-buffers (after highlight-line activate)
  (save-excursion
    (set-buffer "*Buffer List*")
    (hl-line-mode)))

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(require 'hi-lock)

(defun cunene/unhighlight-symbol-at-point ()
  "Remove highlight of symbol at point."
  (interactive)
  (unhighlight-regexp (concat "\\_<" (thing-at-point 'symbol) "\\_>")))

;; Key bindings
(global-set-key (kbd "S-<f12>") 'cunene/unhighlight-symbol-at-point)
(global-set-key (kbd "<f12>") 'highlight-symbol-at-point)
(global-set-key (kbd "C-<f12>") 'highlight-symbol-next)
(global-set-key (kbd "M-<f12>") 'highlight-symbol-prev)

(use-package consult
 :ensure t
  :bind
  (("M-g g" . consult-goto-line)
   ("C-s" . consult-line)
   ("C-x b" . consult-buffer)))

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

(setq-default
 transient-history-file (cunene/cache-concat "transient/history.el")
 transient-levels-file (cunene/cache-concat "transient/levels.el")
 transient-values-file (cunene/cache-concat "transient/values.el"))

(use-package transient
  :init
  :custom
  (transient-default-level 5)
  (transient-mode-line-format nil))

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

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
