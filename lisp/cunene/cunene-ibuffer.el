;;; Marco's .emacs, copied largely from starterkit and Alex Tot's.

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
               ("c++" (mode . c++-mode))
               ("fsharp" (or
                          (mode . inferior-fsharp-mode)
                          (mode . fsharp-mode)))
               ("logs" (mode . log4j-mode))
               ("csharp" (mode . csharp-mode))
               ("java" (mode . java-mode))
               ("make" (or
                        (mode . cmake-mode)
                        (mode . makefile-mode)
                        (mode . makefile-gmake-mode)))
               ("ruby" (mode . ruby-mode))
               ("t4" (name . ".tt$"))
               ("perl" (mode . perl-mode))
               ("javascript" (or
                              (mode . javascript-mode)
                              (mode . js2-mode)
                              (mode . js-mode)))
               ("php" (mode . php-mode))
               ("xml" (mode . nxml-mode))
               ("sql" (or
                       (mode . sql-mode)
                       (name . "^\\*SQL")))
               ("patches" (or
                           (name . "^\\*Assoc file dif")
                           (mode . diff-mode)))
               ("bash" (mode . sh-mode))
               ("awk" (mode . awk-mode))
               ("latex" (or
                         (name . ".tex$")
                         (mode . tex-mode)
                         (mode . latex-mode)))
               ("emacs-lisp" (or
                              (mode . emacs-lisp-mode)
                              (name . "^\\*Compile-Log\\*$")))
               ("powershell" (or
                              (mode . powershell-mode)
                              (name . "^\\*PowerShell")))
               ("grep" (or
                         (name . "^\\*Occur\\*$")
                         (name . "^\\*Moccur\\*$")
                         (mode . grep-mode)))
               ("shell" (or
                         (name . "^\\*Shell Command Output\\*$")
                         (mode . shell-mode)
                         (mode . ssh-mode)
                         (name . "^\\*compilation\\*$")))
               ("file management" (or
                                   (mode . dired-mode)
                                   (mode . tar-mode)))
               ("org" (mode . org-mode))
               ("msdos" (mode . dos-mode))
               ("version control" (or
                                   (name . "^\\*svn-")
                                   (name . "^\\*vc")
                                   (name . "^\\*cvs")
                                   (name . "^\\*magit")))
               ("snippets" (mode . snippet-mode))
               ("system" (or
                                  (name . "^\\*tramp")
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
                                  (name . "^\\*Messages\\*$")))
               ("semantic" (or
                            (mode . data-debug-mode)
                            (name . "^\\*Parser Output\\*$")
                            (name . "^\\*Lexer Output\\*$")))
               ("web browsing" (mode . w3m-mode))
               ("music" (or
                         (mode . bongo-playlist-mode)
                         (mode . bongo-library-mode)))
               ("mail" (or
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\*Bongo Playlist\\*$")
                        (name . "^\\*imap log\\*$")
                        (name . "^\\*gnus trace\\*$")
                        (name . "^\\*nnimap imap.")
                        ))
               ("web development" (or
                                   (mode . html-mode)
                                   (mode . css-mode)))
               ("documentation" (or
                                 (mode . Info-mode)
                                 (mode . apropos-mode)
                                 (mode . woman-mode)
                                 (mode . help-mode)
                                 (mode . Man-mode)))
               ("text files" (or
                              (mode . conf-unix-mode)
                              (mode . conf-space-mode)
                              (mode . text-mode)))
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
