;; -*- mode: emacs-lisp; tab-width: 4; indent-tabs-mode: nil -*-
;;
;; Copyright (C) 2012 Marco Craveiro <marco.craveiro@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(require 'ede-ibuffer)

(define-key ibuffer-mode-map (kbd "/ e") 'ibuffer-filter-by-ede-project)
(define-key ibuffer-mode-map (kbd "% e") 'ibuffer-mark-by-ede-project-regexp)
(define-key ibuffer-mode-map (kbd "s e") 'ibuffer-do-sort-by-ede-project)

;; Below is an example value for `ibuffer-formats'.  When set, the
;; second format configuration replaces the filename-and-process
;; column with the new ede-project column (formats can be cycled with
;; `ibuffer-switch-format'):
;;
(setq ibuffer-formats
      '((mark modified read-only " "
          (name 18 18 :left :elide)
          " "
          (size 9 -1 :right)
          " "
          (mode 16 16 :left :elide)
          " " filename-and-process)
    (mark modified read-only " "
          (name 18 18 :left :elide)
          " "
          (size 9 -1 :right)
          " "
          (mode 16 16 :left :elide)
          " " ede-project)
    (mark " "
          (name 16 -1)
          " " filename)))
