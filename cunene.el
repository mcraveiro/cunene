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
  (setq request-storage-directory (cunene/cache-concat "request/")))
(with-eval-after-load 'tramp
  (setq tramp-persistency-file-name (cunene/cache-concat "tramp.eld")))
(with-eval-after-load 'url
  (setq url-configuration-directory (cunene/cache-concat "url/")))
(with-eval-after-load 'recentf
  (progn
    (setq recentf-save-file (cunene/cache-concat "recentf/recentf"))
    (setq recentf-max-saved-items 500
          ;; disable recentf-cleanup on Emacs start, because it can cause
          ;; problems with remote files
          recentf-auto-cleanup 'never
          recentf-max-menu-items 15)
))

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

;; (use-package quelpa-use-package
;;  :ensure t)

(defvar cunene/config-file
  (concat user-emacs-directory "cunene.el")
  "The location of the generated cunene config file.")

(defvar cunene/config-file-org
  (concat user-emacs-directory "cunene.org")
  "The location of the cunene `org-mode' file.")

(defun cunene/find-config ()
  "Edit cunene's config file."
  (interactive)
  (find-file cunene/config-file-org))

(defun cunene/reload-config()
  "Reload config.org."
  (interactive)
  (delete-file cunene/config-file)
  (org-babel-load-file cunene/config-file-org))

(defun cunen/package-update ()
  "Update all installed packages to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update packages? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Update finished. Restart Emacs to complete the process.")))

(global-set-key (kbd "C-c I") 'cunene/find-config)
(global-set-key (kbd "C-c R") 'cunene/reload-config)

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq kill-ring-max 1000)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(setq-default
 gc-cons-threshold (* 8 1024 1024))      ; Bump up garbage collection threshold.

(add-function :after after-focus-change-function
  (defun cunene/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinition
 require-final-newline t                ; Newline at end of file
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
(fset 'yes-or-no-p 'y-or-n-p)           ; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(column-number-mode t)                  ; Display column numbers
(line-number-mode t)                    ; Display line numbers
(size-indication-mode t)                ; Display size indicator

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; repeat pop mark command without the need for C-u
(setq set-mark-command-repeat-pop t)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Do not ask to kill a buffer.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun cunene/diff-buffer-with-associated-file ()
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
(defun cunene/kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'cunene/kill-associated-diff-buf)

(global-set-key (kbd "C-c C-=") 'cunene/diff-buffer-with-associated-file)

(defun cunene/de-context-kill (arg)
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
        (setq differences (cunene/diff-buffer-with-associated-file)))

      (if (y-or-n-p (format "Buffer %s modified; Kill anyway? " buffer-file-name))
          (progn
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer)))))
    (if (and (boundp 'gnuserv-minor-mode)
           gnuserv-minor-mode)
        (gnuserv-edit)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(global-set-key (kbd "C-x k") 'cunene/de-context-kill)

(use-package super-save
  :ensure t
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

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
  (doom-themes-org-config))

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

;; limit line length
(setq whitespace-line-column 80)

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

;; do not clean whitespace on windows.
(if (not (eq window-system 'w32))
    (add-hook 'before-save-hook 'delete-trailing-whitespace))

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

(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

;; confirm exit
(global-set-key
 (kbd "C-x C-c")
 '(lambda ()
    (interactive)
    (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ?" 4 nil)
        (save-buffers-kill-emacs))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))))

;; Remap Open Dashboard
;; From https://github.com/emacs-dashboard/emacs-dashboard/issues/236
(defun cunene/new-dashboard ()
  "Jump to the dashboard buffer, if doesn't exists create one."
  (interactive)
  (switch-to-buffer dashboard-buffer-name)
  (dashboard-mode)
  (dashboard-insert-startupify-lists)
  (dashboard-refresh-buffer))

(global-set-key (kbd "<f8>") 'cunene/new-dashboard)

(use-package crux
  :ensure t
  :bind (
         ("C-S-d" . crux-duplicate-current-line-or-region)
         ;; Move to beginning of line between head of line and head of text
         ("C-a" . crux-move-beginning-of-line)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c D" . crux-delete-file-and-buffer))
  :config (crux-with-region-or-line kill-region))

(defun uuid-insert()
  (interactive)
  (require 'uuid)
  (insert (upcase (uuid-string))))

(require 'thingatpt)

(use-package drag-stuff
  :ensure t
  :bind
  (:map drag-stuff-mode-map
        ("<C-s-up>" . drag-stuff-up)
        ("<C-s-down>" . drag-stuff-down)
        ("<C-s-left>" . drag-stuff-left)
        ("<C-s-right>" . drag-stuff-right))
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Replace region when inserting text
(delete-selection-mode 1)

;; brings visual feedback to some operations by highlighting portions relating
;; to the operations.
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(require 'rect)

(use-package jump-tree
  :ensure t
  :init (global-jump-tree-mode))

(defun cunene/toggle-quotes ()
  "Toggle single quoted string to double or vice versa, and
  flip the internal quotes as well.  Best to run on the first
  character of the string."
  (interactive)
  (save-excursion
    (re-search-backward "[\"']")
    (let* ((start (point))
           (old-c (char-after start))
           new-c)
      (setq new-c
            (case old-c
              (?\" "'")
              (?\' "\"")))
      (setq old-c (char-to-string old-c))
      (delete-char 1)
      (insert new-c)
      (re-search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (delete-char 1)
        (insert new-c)
        (replace-string new-c old-c nil (1+ start) end)))))

(defun cunene/space-to-underscore-region (start end)
  "Replace space by underscore in region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward " " nil t) (replace-match "_"))))

(defun cunene/underscore-to-space-region (start end)
  "Replace underscore by space in region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "_" nil t) (replace-match " "))))

(defun cunene/replace-underscore-space-toggle ()
  "Replace underscore/space in the current region or line.
If the current line contains more “_” char than space,
then replace them to space, else replace space to _.
If there's a text selection, work on the selected text."
  (interactive)
  (let (li bds)
    (setq bds
          (if (region-active-p)
              (cons (region-beginning) (region-end))
            (bounds-of-thing-at-point 'line)))
    (setq li (buffer-substring-no-properties (car bds) (cdr bds)))
    (if (> (count 32 li) (count 95 li))
        (progn (replace-string " " "_" nil (car bds) (cdr bds)))
      (progn (replace-string "_" " " nil (car bds) (cdr bds))))))

(defun cunene/cycle-hyphen-underscore-space ()
  "Cyclically replace {underscore, space, hypen} chars current
 line or text selection.  When called repeatedly, this command
 cycles the {“ ”, “_”, “-”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0
  ;; to length of charList.
  (let (mainText charList p1 p2 currentState nextState changeFrom
             changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (progn (setq startedWithRegion-p nil )
             (setq p1 (line-beginning-position))
             (setq p2 (line-end-position)) ) )

    (setq charList (list " " "_" "-" ))

    (setq currentState
          (if (get 'cunene/cycle-hyphen-underscore-space 'state)
              (get 'cunene/cycle-hyphen-underscore-space 'state) 0))
    (setq nextState (% (+ currentState (length charList) 1) (length charList)))

    (setq changeFrom (nth currentState charList))
    (setq changeTo (nth nextState charList))

    (setq mainText
          (replace-regexp-in-string changeFrom changeTo
                                    (buffer-substring-no-properties p1 p2)))
    (delete-region p1 p2)
    (insert mainText)

    (put 'cunene/cycle-hyphen-underscore-space 'state nextState)

    (when startedWithRegion-p
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil))))

(global-set-key (kbd "C-c C--") 'cunene/cycle-hyphen-underscore-space)

(defun cunene/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ((eq major-mode 'ruby-mode)
    (string-inflection-ruby-style-cycle))
   (t
    ;; default
    (string-inflection-all-cycle))))

(use-package string-inflection
  :ensure t
  :config
  (global-set-key (kbd "C-M-j") 'cunene/string-inflection-cycle-auto))

(defun cunene/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'cunene/unfill-paragraph)

(defun my-fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
        With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
(bind-key "M-q" 'my-fill-or-unfill-paragraph)

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(require 'saveplace)
(setq save-place-file (cunene/cache-concat "saveplace/places"))
(save-place-mode 1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode +1)
  :config
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  (setq savehist-file (cunene/cache-concat "savehist/history")))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h C" . helpful-command)))

(defun cunene/configure-prettify-symbols-alist ()
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . "“") prettify-symbols-alist)
  (push '("#+END_QUOTE" . "”") prettify-symbols-alist)
  (push '("#+begin_quote" . "“") prettify-symbols-alist)
  (push '("#+end_quote" . "”") prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "»") prettify-symbols-alist)
  (push '("#+END_SRC" . "«") prettify-symbols-alist)
  (push '("#+begin_src" . "»") prettify-symbols-alist)
  (push '("#+end_src" . "«") prettify-symbols-alist)
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'cunene/configure-prettify-symbols-alist)

(defun cunene/prog-mode-configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (setq prettify-symbols-alist '(("lambda" . "λ")
                                 ("->" . "→")
                                 ("->>" . "↠")
                                 ("=>" . "⇒")
                                 ("map" . "↦")
                                 ("/=" . "≠")
                                 ("!=" . "≠")
                                 ("==" . "≡")
                                 ("<=" . "≤")
                                 (">=" . "≥")
                                 ("=<<" . "=≪")
                                 (">>=" . "≫=")
                                 ("<=<" . "↢")
                                 (">=>" . "↣")
                                 ("&&" . "∧")
                                 ("||" . "∨")
                                 ("not" . "¬")))
  (prettify-symbols-mode))

(add-hook 'prog-mode-hook 'cunene/prog-mode-configure-prettify-symbols-alist)

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
(setq dired-listing-switches "-lGh1v --group-directories-first")
(setq-default list-directory-brief-switches "-CF")

(add-hook
 'dired-before-readin-hook
 '(lambda ()
    (when (file-remote-p default-directory)
      (setq dired-actual-switches "-l"))))

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

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

  (setq ibuffer-formats '((mark modified read-only locked
                                " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                " " (size 9 -1 :right)
                                " " (mode 16 16 :left :elide) " " filename-and-process)
                          (mark " " (name 16 -1) " " filename)))
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (setq ibuffer-show-empty-filter-groups nil) ;; Remove empty groups
  (setq ibuffer-expert t) ;; Enable expert mode
  (setq ibuffer-saved-filter-groups ;; Group buffers
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
                                   (mode . helpful-mode)
                                   (mode . help-mode)
                                   (mode . Man-mode)))
                 ("lsp" (or
                          (name . "^\\*clangd")
                          (name . "^\\*lsp")))

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
                            (name . "^\\*helpful")
                            (name . "^\\*Dired log\\*$")
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*gnuplot\\*$")
                            (name . "^\\*Flycheck errors\\*$")
                            (name . "^\\*compdb:")
                            (name . "^\\*Backtrace\\*$")
                            (name . "^\\*dashboard\\*$")
                            (name . "^\\*Messages\\*$")))
                 ("Treemacs" (or
                              (name . "^Treemacs Update")
                              (name . "^\\*nnimap imap.")))
                 )))))

(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "s-W") #'kill-this-buffer)

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

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (kill-ring                . 20)
                (shell-command-history    . 50)
                register-alist)))

;; run a desktop save periodically.
(run-with-timer 300 300
                (lambda () (desktop-save-in-desktop-dir)
                  (savehist-save)
                  (message nil)) ; clear the "Desktop saved in..." message
)

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

(use-package windswap
  :demand
  :bind
  (("<f6> <down>" . windswap-down)
   ("<f6> <up>" . windswap-up)
   ("<f6> <left>" . windswap-left)
   ("<f6> <right>" . windswap-right)))

(use-package shackle
  :hook
  (after-init . shackle-mode)
  :config
  (setq shackle-inhibit-window-quit-on-same-windows t)
  (setq shackle-rules '((help-mode :same t)
                        (helpful-mode :same t)
                        (process-menu-mode :same t)))
  (setq shackle-select-reused-windows t))

(use-package windmove
  :ensure nil
  :bind
  (
   ("<f2> <left>" . windmove-left)
   ("<f2> <down>" . windmove-down)
   ("<f2> <up>" . windmove-up)
   ("<f2> <right>" . windmove-right)))

(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode))

(use-package org
  :ensure nil
  :bind
  (("C-c A" . org-agenda)
   ("C-c B" . org-switchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :hook
  (org-mode . auto-fill-mode)
  :config
  (setq org-startup-folded t)
  (setq org-adapt-indentation nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-cycle-separator-lines 0)
  (setq org-hide-leading-stars t)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-descriptive-links t)
  (setq org-edit-src-content-indentation 0)
  (setq org-edit-src-persistent-message nil)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'current-window)
  (setq org-startup-truncated nil)
  (setq org-support-shift-select 'always)
  (require 'ob-shell)
  (add-to-list 'org-babel-load-languages '(shell . t))
  (modify-syntax-entry ?' "'" org-mode-syntax-table)
  (advice-add 'org-src--construct-edit-buffer-name :override #'cunene/org-src-buffer-name))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
              ("C-c o" . hl-todo-occur))
  :hook ((prog-mode org-mode) . cunene/hl-todo-init)
  :init
  (defun cunene/hl-todo-init ()
    (setq-local hl-todo-keyword-faces '(("TODO" . "#ff9977")
                                        ("DOING" . "#FF00BC")
                                        ("DONE" . "#44bc44")
                                        ("BLOCKED" . "#003366")
                                        ("FIXME"  . "#FF0000")
                                        ("DEBUG"  . "#A020F0")
                                        ("GOTCHA" . "#FF4500")
                                        ("STUB"   . "#1E90FF")
                                         ))
    (hl-todo-mode)))

(use-package org-ref
  :ensure t
  :after org)

(defun cunene/occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;; speeds up org-ref
(setq org-ref-show-broken-links nil)
(setq org-latex-pdf-process
      '("latexmk -shell-escape -bibtex -pdf %f"))
(setq org-latex-listings t)
(setq bibtex-dialect 'biblatex)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-highlight-latex-and-related nil)

;; add classic thesis
(add-to-list 'org-latex-classes
             '("scrreprt" "\\documentclass[11pt]{scrreprt}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

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

(use-package org-agenda
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files (directory-files-recursively "~/Documents/org/" "\\.org$"))
  ;; (setq org-agenda-files '(
  ;;                          "~/Documents/org/work.org"
  ;;                          "~/Documents/org/reminder.org"
  ;;                         ))
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-prefix-format
        '((agenda . " %i %-24:c%?-16t%-10e% s")
          (todo   . " %i %-24:c %-10e")
          (tags   . " %i %-24:c")
          (search . " %i %-24:c")))

  ;;https://www.philnewton.net/blog/how-i-get-work-done-with-emacs/
  (setq org-agenda-custom-commands
        '(("d" "Today's Tasks"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-overriding-header "Today's Tasks")))))))
  )

;; (use-package outshine
;;  :quelpa (outshine :fetcher github :repo "alphapapa/outshine"))

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
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-text-scale                      -2
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

(defun cunene/setup-tab-width-treemacs-dired (&rest _)
  "Set `tab-width' to 1, so tab characters don't ruin formatting."
  (setq tab-width 1))

(add-hook 'treemacs-mode-hook #'cunene/setup-tab-width-treemacs-dired)

(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local tab-width 1)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (setq vertico-cycle t)) ;; enable cycling for `vertico-next' and `vertico-previous'.

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
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

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

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package completing-read-xref
  :load-path cunene/vendor-packages
  :commands (completing-read-xref-show-xrefs completing-read-xref-show-xrefs)
  :init (setq xref-show-definitions-function 'completing-read-xref-show-defs))

;; we only use helm for a few things we haven't learned to use in vertico.
(use-package helm
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (require 'helm-for-files)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ;; this updates things quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t))
  :bind (("C-c h" . helm-mini)
         ("C-x c SPC" . helm-all-mark-rings)))

(use-package helm-c-yasnippet
  :bind (("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region))
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package helm-ls-git
  :after helm
  :ensure t)

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-posframe
  :init (company-posframe-mode 1)
  :config
  (setq company-idle-delay 0.3
        company-show-numbers t
        company-tooltip-align-annotations t
        company-async-timeout 15
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-dabbrev-other-buffers t
        company-auto-complete nil
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-code-everywhere t
        company-dabbrev-code-ignore-case t
        company-minimum-prefix-length 1
        company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)

  :diminish)

(use-package company-box
  :hook (company-mode . company-box-mode))

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(defvar cunene/undo-tree-directory
  (cunene/cache-concat "undo")
  "Location of the undo-tree save files.")

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-diff nil) ;; causes problems with other buffers
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-history-directory-alist
        `((".*" . ,cunene/undo-tree-directory)))
  (setq undo-tree-auto-save-history t) ;; autosave the undo-tree history
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

(setq bookmark-default-file
      (cunene/cache-concat "bookmarks/bookmarks"))
(setq bookmark-save-flag 1)

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
  :bind (("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ("C-x b" . consult-buffer)
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ("M-g g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ("M-g l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("C-x c o" . consult-multi-occur)
         ("C-x c SPC" . consult-mark)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<"))

(use-package consult-flycheck
  :after flycheck)

;; Consult directory navigation
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package engine-mode
  :config
  (engine-mode t)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"))

(setq isearch-allow-scroll t)
(setq isearch-wrap-pause 'no-ding)

(defadvice isearch-update (before my-isearch-reposite activate)
  (sit-for 0)
  (recenter 1))

;; anzu-mode enhances isearch & query-replace by showing total matches and
;; current match position
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(setq-default abbrev-mode 1)

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :diminish yas
  :bind
  (:map yas-minor-mode-map
        ("C-c & t" . yas-describe-tables)
        ("C-c & &" . org-mark-ring-goto)))

(use-package yasnippet-snippets
  :defer)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package logview
  :ensure t
  :config
  (setq logview-cache-filename (cunene/cache-concat "logview/logview-cache.extmap"))
  (setq logview-additional-submodes
        '(("dogen"
           (format . "TIMESTAMP [LEVEL] [NAME]")
           (levels . "SLF4J")
           (timestamp "ISO 8601 datetime + micros")))))

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-new-workspace t)
  ;; also save side and slot windows configuration.
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))
  (eyebrowse-mode t))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 5.0)))))
  :bind
  ("M-o" . ace-window))

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

(use-package ztree
  :ensure t)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")

(defadvice ediff-files-internal
  (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ the reason
for catching the error out here (when re-thrown from the inner
advice) is to let the stack continue to unwind before we start
the new diff otherwise some code in the middle of the stack
expects some output that isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions
  (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
     (error (error-message-string err)))))

(setq erc-join-buffer 'bury)
(add-hook 'erc-mode-hook (lambda () (erc-fill-mode nil)))

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

(use-package git-messenger
  :bind ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

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
  :config
  (setq epg-pinentry-mode 'loopback)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-highlight-hunk-region-functions
        '(magit-diff-highlight-hunk-region-using-face))
  (setq magit-diff-refine-hunk 'all)
  (setq magit-module-sections-nested nil)
  (setq magit-section-initial-visibility-alist
        '((modules . show) (stashes . show) (unpulled . show) (unpushed . show)))
  (magit-add-section-hook
   'magit-status-sections-hook 'magit-insert-modules-overview 'magit-insert-merge-log)
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight))

(use-package git-timemachine
  :ensure t)

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
  :config
  (setq transient-default-level 5)
  (setq transient-mode-line-format nil))

(use-package smerge-mode
  :commands smerge-mode
  :bind ("C-c '" . hydra-hsmerge/body)
  :init
  (defun cunene/maybe-enable-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  (add-hook 'find-file-hook 'cunene/maybe-enable-smerge)
  (add-hook 'after-revert-hook 'cunene/maybe-enable-smerge)

  :config
  (defhydra hydra-smerge (:hint nil
                          :pre (smerge-mode 1)
                          :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper (mine)       _=_: upper/lower       _r_esolve
^^           _l_ower (other)      _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue)))

(setq projectile-known-projects-file
      (cunene/cache-concat "projectile/bookmarks.eld"))
(setq projectile-cache-file
      (cunene/cache-concat "projectile/projectile.cache"))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-enable-caching t))

(use-package ibuffer-projectile
  :ensure t
  :after projectile)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.2)))

(use-package color-identifiers-mode
  :ensure t
  :commands color-identifiers-mode
  :config
  (add-hook 'prog-mode-hook 'color-identifiers-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c++-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-auto-guess-root t
        lsp-session-file (cunene/cache-concat "lsp/lsp-session-v1")
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting  nil
        lsp-ui-doc-delay 5
        lsp-ui-sideline-enable nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs
  :config
  (setq lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-symbols-position-params
        '((side . right)
          (slot . 1)
          (window-width . 45)))
  :commands lsp-treemacs-errors-list)

(setq cunene/general-lsp-hydra-heads
        '(;; Xref
          ("d" xref-find-definitions "Definitions" :column "Xref")
          ("D" xref-find-definitions-other-window "-> other win")
          ("r" xref-find-references "References")
          ("s" cunene/helm-lsp-workspace-symbol-at-point "Helm search")
          ("S" cunene/helm-lsp-global-workspace-symbol-at-point "Helm global search")

          ;; Peek
          ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
          ("C-r" lsp-ui-peek-find-references "References")
          ("C-i" lsp-ui-peek-find-implementation "Implementation")

          ;; LSP
          ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
          ("C-a" lsp-execute-code-action "Execute code action")
          ("R" lsp-rename "Rename")
          ("t" lsp-goto-type-definition "Type definition")
          ("i" lsp-goto-implementation "Implementation")
          ("f" helm-imenu "Filter funcs/classes (Helm)")
          ("C-c" lsp-describe-session "Describe session")

          ;; Flycheck
          ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

        cunene/misc-lsp-hydra-heads
        '(;; Misc
          ("q" nil "Cancel" :column "Misc")
          ("b" pop-tag-mark "Back")))

  ;; Create general hydra.
(eval `(defhydra cunene/lsp-hydra (:color blue :hint nil)
         ,@(append
            cunene/general-lsp-hydra-heads
            cunene/misc-lsp-hydra-heads)))

(add-hook 'lsp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l") 'cunene/lsp-hydra/body)
            'lsp-ui-mode))

(use-package consult-lsp
  :ensure t
  :diminish)

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-indent-level 4)
  (setq image-auto-resize nil)
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

(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode org-mode) . rainbow-mode)
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :diminish
  :init
  (show-smartparens-global-mode +1)
  :config
  (setq sp-autoskip-closing-pair 'always)
  ;; (setq sp-hybrid-kill-entire-symbol nil)
)

(use-package rainbow-mode
  :ensure t
  :config
  (setq rainbow-x-colors nil))

(use-package aggressive-indent
  :ensure t)

(defun cunene/indent-buffer ()
  "Indent entire buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

;; replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)

(require 'hideshow)

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)

;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 't)

(setq hs-set-up-overlay
      (defun cunene/display-code-line-counts (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (overlay-put ov 'display
                       (propertize
                        (format " ... <%d>"
                                (count-lines (overlay-start ov)
                                             (overlay-end ov)))
                        'face 'font-lock-type-face)))))
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; https://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)

(use-package json-mode
  :ensure t)

(use-package jq-mode
  :ensure t)

(with-eval-after-load "json-mode"
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))

;; Format JSON / JSONlines with JQ
(use-package jq-format
  :ensure t)

(use-package hierarchy
  :ensure t)

(use-package json-navigator
  :ensure t)

(use-package verb
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
)

;; Default these extensions to c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'innamespace 0) ;; Do not indent namespaces.
            (c-set-offset 'arglist-intro '+) ;; indent function args properly
            (c-set-offset 'arglist-cont-nonempty '+)
            (c-toggle-hungry-state 1)          ;; use hungry delete.
            (auto-fill-mode 1)                 ;; auto fill comments
            (setq c-basic-offset tab-width)
            (setq c-default-style "stroustrup")))

;; Key bindings
(eval-after-load 'cc-mode
  '(progn
     ;; Ident when moving to a new line
     (define-key c-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     ))

(use-package csharp-mode
  :ensure t)

(defun csharp-hs-forward-sexp (&optional arg)
  "I set hs-forward-sexp-func to this function.

I found this customization necessary to do the hide/show magic in C#
code, when dealing with region/endregion. This routine
goes forward one s-expression, whether it is defined by curly braces
or region/endregion. It handles nesting, too.

The forward-sexp method takes an arg which can be negative, which
indicates the move should be backward.  Therefore, to be fully
correct this function should also handle a negative arg. However,
the hideshow.el package never uses negative args to its
hs-forward-sexp-func, so it doesn't matter that this function does not
do negative numbers.

The arg can also be greater than 1, which means go forward
multiple times. This function doesn't handle that EITHER.  But
again, I haven't see that as a problem."

  (message "csharp-hs-forward-sexp, (arg %d) (point %d)..."
           (if (numberp arg) arg -1)
           (point))

  (let ((nestlevel 0)
        (mark1 (point))
        (done nil)
        )

    (if (and arg (< arg 0))
        (message "negative arg (%d) is not supported..." arg)

      ;; else, we have a positive argument, hence move forward.
      ;; simple case is just move forward one brace
      (if (looking-at "{")
          (forward-sexp arg)

        ; The more complex case is dealing with a "region/endregion" block.
        ; We have to deal with nested regions!
        (and
         (while (not done)
           (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                              (point-max) 'move)
           (cond

            ((eobp))                    ; do nothing if at end of buffer

            ((and
              (match-beginning 1)
              ;; if the match is longer than 6 chars, we know it is "endregion"
              (if (> (- (match-end 1) (match-beginning 1)) 6)
                  (setq nestlevel (1- nestlevel))
                (setq nestlevel (1+ nestlevel))
                )
              )))

           (setq done (not (and (> nestlevel 0) (not (eobp)))))

           )                            ; while

         (if (= nest 0)
             (goto-char (match-end 2)))

         )
        )
      )
    )
  )

(unless (assoc 'csharp-mode hs-special-modes-alist)
          (push '(csharp-mode
                  ; "\\(^\\s*#\\s*region\\b\\)\\|{"      ; regexp for start block DID NOT WORK
                  "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"  ; regexp for start block

                  ; "\\(^\\s*#\\s*endregion\\b\\)\\|}"   ; regexp for end block NO WORKY!
                  "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}"   ; regexp for end block

                  "/[*/]"                                ; regexp for comment start

                  csharp-hs-forward-sexp                 ; hs-forward-sexp-func
                  hs-c-like-adjust-block-beginning       ;c-like adjust (1 char)
                  ;csharp-hs-adjust-block-beginning      ;csharp adjust ?
                  )
                hs-special-modes-alist)
          )

(use-package doxymacs
  :load-path cunene/vendor-packages
  :config
  ;; syntax highlighting for doxygen keywords.
  (defun cunene/doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'cunene/doxymacs-font-lock-hook)

  ;; start doxymacs mode in C/C++
  (add-hook 'c-mode-common-hook 'doxymacs-mode)
)

(global-set-key (kbd "C-c c") 'compile)

;; automatically scroll the output
(setq compilation-scroll-output t)

;; reuse existing frame.
(setq display-buffer-reuse-frames t)

;; kill ongoing compilation
(setq compilation-always-kill  t)

;; save buffers whenc compiling without asking
(setq compilation-ask-about-save nil)

;; Compilation from Emacs. From prelude.
(defun cunene/colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'ansi-color)
(add-hook 'compilation-filter-hook #'cunene/colorize-compilation-buffer)

(use-package persistent-scratch
  :config
  (setq persistent-scratch-save-file
        (cunene/cache-concat "scratch/persistent-scratch"))
  (persistent-scratch-setup-default))

(use-package bongo
  :ensure t
  :config
  (setq bongo-default-directory "~/Music")
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(vlc mpv))
  (setq bongo-vlc-program-name "cvlc")

;;; Bongo playlist buffer
  (defvar cunene/bongo-playlist-delimiter
    "\n******************************\n\n"
    "Delimiter for inserted items in `bongo' playlist buffers.")

  (defun cunene/bongo-playlist-section ()
    (bongo-insert-comment-text
     cunene/bongo-playlist-delimiter))

  (defun cunene/bongo-paylist-section-next ()
    "Move to next `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-forward section nil t))
          (progn
            (goto-char (point-at-eol))
            (re-search-forward section nil t))
        (goto-char (point-max)))))

  (defun cunene/bongo-paylist-section-previous ()
    "Move to previous `bongo' playlist custom section delimiter."
    (interactive)
    (let ((section "^\\*+$"))
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (goto-char (point-at-bol))
            (re-search-backward section nil t))
        (goto-char (point-min)))))

  (defun cunene/bongo-playlist-mark-section ()
    "Mark `bongo' playlist section, delimited by custom markers.
The marker is `cunene/bongo-playlist-delimiter'."
    (interactive)
    (let ((section "^\\*+$"))
      (search-forward-regexp section nil t)
      (push-mark nil t)
      (forward-line -1)
      ;; REVIEW any predicate to replace this `save-excursion'?
      (if (save-excursion (re-search-backward section nil t))
          (progn
            (search-backward-regexp section nil t)
            (forward-line 1))
        (goto-char (point-min)))
      (activate-mark)))

  (defun cunene/bongo-playlist-kill-section ()
    "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`cunene/bongo-playlist-kill-section'."
    (interactive)
    (cunene/bongo-playlist-mark-section)
    (bongo-kill))

  (defun cunene/bongo-playlist-play-random ()
    "Play random `bongo' track and determine further conditions."
    (interactive)
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (when (or (bongo-playlist-buffer-p)
              (bongo-library-buffer-p))
      (unless (bongo-playing-p)
        (with-current-buffer (bongo-playlist-buffer)
          (bongo-play-random)
          (bongo-random-playback-mode 1)
          (bongo-recenter)))))

  (defun cunene/bongo-playlist-random-toggle ()
    "Toggle `bongo-random-playback-mode' in playlist buffers."
    (interactive)
    (if (eq bongo-next-action 'bongo-play-random-or-stop)
        (bongo-progressive-playback-mode)
      (bongo-random-playback-mode)))

  (defun cunene/bongo-playlist-reset ()
    "Stop playback and reset `bongo' playlist marks.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-reset-playlist)))

  (defun cunene/bongo-playlist-terminate ()
    "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently-playing track."
    (interactive)
    (when (bongo-playlist-buffer-p)
      (bongo-stop)
      (bongo-erase-buffer)))

  (defun cunene/bongo-playlist-insert-playlist-file ()
    "Insert contents of playlist file to a `bongo' playlist.
Upon insertion, playback starts immediately, in accordance with
`cunene/bongo-play-random'.

The available options at the completion prompt point to files
that hold filesystem paths of media items.  Think of them as
'directories of directories' that mix manually selected media
items.

Also see `cunene/bongo-dired-make-playlist-file'."
    (interactive)
    (let* ((path "~/Music/playlists/")
           (dotless directory-files-no-dot-files-regexp)
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files path nil dotless)))
           (choice (completing-read "Insert playlist: " playlists nil t)))
      (if (bongo-playlist-buffer-p)
          (progn
            (save-excursion
              (goto-char (point-max))
              (bongo-insert-playlist-contents
               (format "%s%s" path choice))
              (cunene/bongo-playlist-section))
            (cunene/bongo-playlist-play-random))
        (user-error "Not in a `bongo' playlist buffer"))))

;;; Bongo + Dired (bongo library buffer)
  (defmacro cunene/bongo-dired-library (name doc val)
    "Create `bongo' library function NAME with DOC and VAL."
    `(defun ,name ()
       ,doc
       (when (string-match-p "\\`~/Music/" default-directory)
         (bongo-dired-library-mode ,val))))

  (cunene/bongo-dired-library
   cunene/bongo-dired-library-enable
   "Set `bongo-dired-library-mode' when accessing ~/Music.

Add this to `dired-mode-hook'.  Upon activation, the directory
and all its sub-directories become a valid library buffer for
Bongo, from where we can, among others, add tracks to playlists.
The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Note, though, that this will interfere with `wdired-mode'.  See
`cunene/bongo-dired-library-disable'."
   1)

  ;; NOTE `cunene/bongo-dired-library-enable' does not get reactivated
  ;; upon exiting `wdired-mode'.
  ;;
  ;; TODO reactivate bongo dired library upon wdired exit
  (cunene/bongo-dired-library
   cunene/bongo-dired-library-disable
   "Unset `bongo-dired-library-mode' when accessing ~/Music.
This should be added `wdired-mode-hook'.  For more, refer to
`cunene/bongo-dired-library-enable'."
   -1)

  (defun cunene/bongo-dired-insert-files ()
    "Add files in a `dired' buffer to the `bongo' playlist."
    (let ((media (dired-get-marked-files)))
      (with-current-buffer (bongo-playlist-buffer)
        (goto-char (point-max))
        (mapc 'bongo-insert-file media)
        (cunene/bongo-playlist-section))
      (with-current-buffer (bongo-library-buffer)
        (dired-next-line 1))))

  (defun cunene/bongo-dired-insert ()
    "Add `dired' item at point or marks to `bongo' playlist.

The playlist is created, if necessary, while some other tweaks
are introduced.  See `cunene/bongo-dired-insert-files' as well as
`cunene/bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `cunene/bongo-dired-library')."
    (interactive)
    (when (bongo-library-buffer-p)
      (unless (bongo-playlist-buffer-p)
        (bongo-playlist-buffer))
      (cunene/bongo-dired-insert-files)
      (cunene/bongo-playlist-play-random)))

  (defun cunene/bongo-dired-make-playlist-file ()
    "Add `dired' marked items to playlist file using completion.

These files are meant to reference filesystem paths.  They ease
the task of playing media from closely related directory trees,
without having to interfere with the user's directory
structure (e.g. a playlist file 'rock' can include the paths of
~/Music/Scorpions and ~/Music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no marks are available, the
item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches user input.  Depending on the completion
framework, such as with `icomplete-mode', this may require a
forced exit (e.g. \\[exit-minibuffer] to parse the input without
further questions).

Also see `cunene/bongo-playlist-insert-playlist-file'."
    (interactive)
    (let* ((dotless directory-files-no-dot-files-regexp)
           (pldir "~/Music/playlists")
           (playlists (mapcar
                       'abbreviate-file-name
                       (directory-files pldir nil dotless)))
           (plname (completing-read "Select playlist: " playlists nil nil))
           (plfile (format "%s/%s" pldir plname))
           (media-paths
            (if (derived-mode-p 'dired-mode)
                ;; TODO more efficient way to do ensure newline ending?
                ;;
                ;; The issue is that we need to have a newline at the
                ;; end of the file, so that when we append again we
                ;; start on an empty line.
                (concat
                 (mapconcat #'identity
                            (dired-get-marked-files)
                            "\n")
                 "\n")
              (user-error "Not in a `dired' buffer"))))
      ;; The following `when' just checks for an empty string.  If we
      ;; wanted to make this more robust we should also check for names
      ;; that contain only spaces and/or invalid characters…  This is
      ;; good enough for me.
      (when (string-empty-p plname)
        (user-error "No playlist file has been specified"))
      (unless (file-directory-p pldir)
        (make-directory pldir))
      (unless (and (file-exists-p plfile)
                   (file-readable-p plfile)
                   (not (file-directory-p plfile)))
        (make-empty-file plfile))
      (append-to-file media-paths nil plfile)
      (with-current-buffer (find-file-noselect plfile)
        (delete-duplicate-lines (point-min) (point-max))
        (sort-lines nil (point-min) (point-max))
        (save-buffer)
        (kill-buffer))))

  :hook ((dired-mode-hook . cunene/bongo-dired-library-enable)
         (wdired-mode-hook . cunene/bongo-dired-library-disable))
  :bind (("<C-XF86AudioPlay>" . bongo-pause/resume)
         ("<C-XF86AudioNext>" . bongo-next)
         ("<C-XF86AudioPrev>" . bongo-previous)
         ("<M-XF86AudioPlay>" . bongo-show)
         ("<S-XF86AudioNext>" . bongo-seek-forward-10)
         ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
         :map bongo-playlist-mode-map
         ("n" . bongo-next-object)
         ("p" . bongo-previous-object)
         ("M-n" . cunene/bongo-paylist-section-next)
         ("M-p" . cunene/bongo-paylist-section-previous)
         ("M-h" . cunene/bongo-playlist-mark-section)
         ("M-d" . cunene/bongo-playlist-kill-section)
         ("g" . cunene/bongo-playlist-reset)
         ("D" . cunene/bongo-playlist-terminate)
         ("r" . cunene/bongo-playlist-random-toggle)
         ("R" . bongo-rename-line)
         ("j" . bongo-dired-line)       ; Jump to dir of file at point
         ("J" . dired-jump)             ; Jump to library buffer
         ("i" . cunene/bongo-playlist-insert-playlist-file)
         ("I" . bongo-insert-special)
         :map bongo-dired-library-mode-map
         ("<C-return>" . cunene/bongo-dired-insert)
         ("C-c SPC" . cunene/bongo-dired-insert)
         ("C-c +" . cunene/bongo-dired-make-playlist-file)))

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

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

(use-package ssh
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package rg
  :ensure t)

(setq cunene/browsers
      '(("Firefox" . browse-url-firefox)
        ("Chrome" . browse-url-chrome)
        ("EWW" . eww-browse-url)))

(defun cunene/browse-url (&rest args)
  "Select the prefered browser from a menu before opening the URL."
  (interactive)
  (let ((browser (completing-read "WWW browser: " cunene/browsers nil t "")))
    (apply (cdr (assoc browser cunene/browsers)) args)))

(setq browse-url-browser-function #'cunene/browse-url)

(use-package ssh
  :config  (add-hook 'ssh-mode-hook
                     (lambda ()
                       (setq ssh-directory-tracking-mode t)
                       (shell-dirtrack-mode t)
                       (setq dirtrackp nil))))

;;; cunene.el ends here
