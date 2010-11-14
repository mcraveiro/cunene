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

;; Highlight current line.
(require 'highline)

;; Fonts
(setq highline-face 'highlight)
(setq highline-whole-line nil)

;; Turn on local highlighting for a bunch of modes
(add-hook 'dired-mode-hook 'highline-on)
(add-hook 'ibuffer-hooks #'highline-on)
(add-hook 'grep-setup-hook #'highline-on)
(add-hook 'compilation-mode-hook #'highline-on)
(add-hook 'magit-mode-hook #'highline-on)
(add-hook 'vc-git-log-view-mode-hook #'highline-on)
;; (add-hook 'log-view-hook #'highline-mode-on)
(add-hook 'find-dired-mode-hook #'highline-mode-on)

;; Turn on local highlighting for list-buffers
(defadvice list-buffers (after highlight-line activate)
  (save-excursion
    (set-buffer "*Buffer List*")
    (highline-on)))
