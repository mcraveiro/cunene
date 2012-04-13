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

;; hack for emacs 24
(setq stack-trace-on-error nil)

;; Add ecb to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/ecb"))

;; Version
(setq ecb-options-version "2.40")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Emacs code browser
(require 'ecb)

;; Do not show tip of the day
(setq ecb-tip-of-the-day nil)

(defun toggle-ecb ()
  "Toggles ECB"
  (interactive)
  (if (and (boundp 'ecb-activated-window-configuration)
           ecb-activated-window-configuration)
      (ecb-deactivate) (ecb-activate)))

(global-set-key (kbd "C-c b") 'toggle-ecb)
