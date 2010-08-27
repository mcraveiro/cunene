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

;; Add elpa to load path.
(setq elpa-dir (concat dotfiles-dir "/elpa"))
(add-to-list 'load-path elpa-dir)

;; Location for installed packages
(setq package-user-dir elpa-dir)

;; Initialise elpa.
(when
    (load (concat elpa-dir "/package.el"))
  (package-initialize))

(require 'package)
