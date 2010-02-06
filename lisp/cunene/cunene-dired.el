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

;; Turn on local highlighting
(add-hook 'dired-after-readin-hook #'highline-mode-on)

;; Dired switches
(setq dired-listing-switches "-lX")
(setq list-directory-brief-switches "-CF")

;; if dired's already loaded, then the keymap will be bound
;; (if (boundp 'dired-mode-map)
;;     ;; we're good to go; just add our bindings
;;     (my-dired-init)
;;   ;; it's not loaded yet, so add our bindings to the load-hook
;;   (add-hook 'dired-load-hook 'my-dired-init))

;; (require 'dired-single)

;; Dired: FIXME
;; (defun my-dired-init ()
;;   "Bunch of stuff to run for dired, either immediately or when it's loaded."
;;   ;; <add other stuff here>
;;   (define-key dired-mode-map [return] 'joc-dired-single-buffer)
;;   (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
;;   (define-key dired-mode-map "^"
;;     (function
;;      (lambda nil (interactive) (joc-dired-single-buffer "..")))))
