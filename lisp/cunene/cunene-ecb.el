;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; cunene is free software; you can redistribute it and/or modify it
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
;; along with cunene.  If not, see <http://www.gnu.org/licenses/>.

;; Add ecb to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/ecb"))

;; Emacs code browser
(require 'ecb)
(require 'ecb-autoloads)

;; Version
(setq ecb-options-version "2.40")

;; Do not show tip of the day
(setq ecb-tip-of-the-day nil)

(defun toggle-ecb ()
  "Toggles ECB"
  (interactive)
  (if (and (boundp 'ecb-activated-window-configuration)
           ecb-activated-window-configuration)
      (ecb-deactivate) (ecb-activate)))

(global-set-key (kbd "C-c b") 'toggle-ecb)
