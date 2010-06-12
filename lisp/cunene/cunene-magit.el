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

;; Add magit to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/magit"))

;; Cating for git
(setenv "PAGER" "cat")

;; Change the diff colours.
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Change the regular diff colours
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground diff-added-face "Green")
     (set-face-foreground diff-removed-face "Red")))

(autoload 'magit-status "magit" "git mode" t)

;;
;; Key bindings
;;

;; git status
(global-set-key (kbd "C-x g") 'magit-status)
