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

;; Highlight current line.
(require 'highline)

(defun highline-mode-on () (highline-mode 1))

;;
;; Turn on local highlighting for list-buffers (C-x C-b)
;;
(defadvice list-buffers (after highlight-line activate)
  (save-excursion
    ;; FIXME: can we make this a list? add grep, find, find-dired
    (set-buffer "*Buffer List*")
    (highline-mode-on)))

;; Fonts
(setq highline-face 'highlight)
(setq highline-whole-line nil)

;; Turn on local highlighting for a bunch of modes
(add-hook 'dired-after-readin-hook #'highline-mode-on)
(add-hook 'ibuffer-hooks #'highline-mode-on)
(add-hook 'grep-setup-hook #'highline-mode-on)
(add-hook 'compilation-mode-hook #'highline-mode-on)
;; (add-hook 'log-view-hook #'highline-mode-on)
;; (add-hook 'find-dired-hook #'highline-mode-on)
