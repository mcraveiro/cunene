;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Clever buffer menu
(require 'ibuffer)

;; group buffers
(setq ibuffer-saved-filter-groups
      (quote (("home"
               ("c++" (or
                       (mode . c++-mode)
                       (name . "^\\*compilation\\*$")))
               ("csharp" (mode . csharp-mode))
               ("make" (mode . cmake-mode))
               ("ruby" (mode . ruby-mode))
               ("Text Templates" (name . ".tt$"))
               ("XML" (mode . nxml-mode))
               ("sql" (or
                       (mode . sql-mode)
                       (name . "^\\*SQL")))
               ("powershell" (or
                              (mode . powershell-mode)
                              (name . "^\\*PowerShell")))
               ("shell" (or
                         (name . "^\\*Shell Command Output\\*$")
                         (mode . grep-mode)
                         (mode . shell-mode)))
               ("file management" (mode . dired-mode))
               ("emacs-lisp" (or
                              (mode . emacs-lisp-mode)
                              (name . "^\\*Compile-Log\\*$")))
               ("version control" (or
                                    (name . "^\\*svn-")
                                    (name . "^\\*vc")
                                    (name . "^\\*cvs")
                                    (name . "^\\*magit")))
                ("system buffers" (or
                          (name . "^\\*WoMan-Log\\*$")
                          (name . "^\\*Apropos\\*$")
                          (name . "^\\*Completions\\*$")
                          (name . "^\\*Help\\*$")
                          (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")))
                ("web browsing" (name . "^\\*w3m"))
                ("web development" (or
                                    (mode . html-mode)
                                    (mode . css-mode)))
                ("documentation" (or
                                  (mode . Info-mode)
                                  (mode . apropos-mode)
                                  (mode . woman-mode)
                                  (mode . help-mode)
                                  (mode . Man-mode)))
))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "home")))

;; (setq ibuffer-formats '((mark modified read-only " " (name 30 30) " "
;;                               (mode 16 16)  " " filename)
;;                         (mark " " (name 16 -1) " " filename))
;;       ibuffer-elide-long-columns t)
;; (setq ibuffer-default-sorting-mode 'major-mode)

;; (defadvice ibuffer-update-title-and-summary (after remove-column-titles)
;;   (save-excursion
;;     (set-buffer "*Ibuffer*")
;;     (toggle-read-only 0)
;;     (goto-char 1)
;;     (search-forward "-\n" nil t)
;;     (delete-region 1 (point))
;;     (let ((window-min-height 1))
;;       ;; save a little screen estate
;;       (shrink-window-if-larger-than-buffer))
;;     (toggle-read-only)))

;; (ad-activate 'ibuffer-update-title-and-summary)

;; (define-ibuffer-sorter filename-or-dired
;;   "Sort the buffers by their pathname."
;;   (:description "filenames plus dired")
;;   (string-lessp
;;    (with-current-buffer (car a)
;;      (or buffer-file-name
;;          (if (eq major-mode 'dired-mode)
;;              (expand-file-name dired-directory))
;;          ;; so that all non pathnames are at the end
;;          "~"))
;;    (with-current-buffer (car b)
;;      (or buffer-file-name
;;          (if (eq major-mode 'dired-mode)
;;              (expand-file-name dired-directory))
;;          ;; so that all non pathnames are at the end
;;          "~"))))

;; (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-filename-or-dired)

;; Key bindings
(global-set-key (kbd "<f5>") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))
