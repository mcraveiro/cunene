;; -*- mode: emacs-lisp; tab-width: 4; indent-tabs-mode: nil -*-
;;
;; Copyright (C) 2012 Marco Craveiro <marco.craveiro@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU  General Public License as published by
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

(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;; do not auto-revert by default
(setq log4j-auto-revert-flag nil)

(add-hook
 'log4j-mode-hook
 (lambda ()
    (define-key log4j-mode-local-map [(control down)] 'log4j-forward-record)
    (define-key log4j-mode-local-map [(control up)] 'log4j-backward-record)))
