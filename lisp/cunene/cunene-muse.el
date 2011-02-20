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

;; Add muse to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/muse-el/lisp"))

;; load authoring mode
(require 'muse-mode)

;; load publishing styles I use
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)

;; publish files in projects
(require 'muse-project)

;; hooks
(defun muse-minor-modes ()
  (longlines-mode 1)
  (flyspell-mode 1)
  (font-lock-mode 1))

(add-hook 'muse-mode-hook 'muse-minor-modes)

;; publishing
(defun my-muse-publish ()
  (interactive)
  (muse-publish-file (buffer-file-name) "html"))

(define-key muse-mode-map (kbd "C-c c") 'muse-project-publish)

;; dogen project
(setq muse-project-alist
      '(("website"
         ("~/code/kitanda/super/modules/dogen/doc/wiki" :default "index")
         (:base "html" :path "~/code/kitanda/super/modules/website/dogen")
         )))

