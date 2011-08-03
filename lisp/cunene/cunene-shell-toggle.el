;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2010  Marco Craveiro
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

(autoload 'shell-toggle "shell-toggle"
  "Toggles between the *shell* buffer and whatever buffer you are editing." t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd \" command." t)

;; Start a shell
;; (global-set-key (kbd "C-x m") (lambda () (interactive) (shell)))

(global-set-key (kbd "C-x m") 'shell-toggle)
(global-set-key (kbd "C-x C-m") 'shell-toggle-cd)
