;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; init.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Cunene is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with init.el.  If not, see <http://www.gnu.org/licenses/>.

;; Clever buffer menu
(require 'ibuffer)

;; group buffers
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("c++" (or
                       (mode . c++-mode)
                       (name . "^\\*compilation\\*$")))
               ("sql" (or
                       (mode . sql-mode)
                       (name . "^\\*SQL")))
               ("file management" (or
                                   (mode . dired-mode)
                                   (mode . grep-mode)
                                   (mode . shell-mode)))
                ("emacs-lisp" (or
                               (mode . emacs-lisp-mode)
                               (name . "^\\*Compile-Log\\*$")))
                ("version control" (or
                                    (name . "^\\*vc-")
                                    (name . "^\\*magit")))
                ("emacs" (or
                          (name . "^\\*Completions\\*$")
                          (name . "^\\*Help\\*$")
                          (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")))
                ("web" (name . "^\\*w3m"))
                ("documentation" (or
                                  (mode . Info-mode)
                                  (mode . apropos-mode)
                                  (mode . woman-mode)
                                  (mode . help-mode)
                                  (mode . Man-mode)))
))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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
