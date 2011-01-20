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

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; British dictionary
(setq ispell-dictionary "british")

;; Hooks
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg. For use in hooks."
  (interactive)
  (flyspell-mode 1))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'sql-mode-hook 'flyspell-prog-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'cmake-mode-hook 'flyspell-prog-mode)
