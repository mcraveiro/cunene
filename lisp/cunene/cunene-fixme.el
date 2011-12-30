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

;; Make FIXME stand out
(setq fixme-modes
      '(latex-mode makefile-mode c++-mode emacs-lisp-mode sh-mode text-mode
                   org-mode))
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\|TODO\\)" 1 'font-lock-fixme-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" "Yellow" nil t nil t nil nil)
