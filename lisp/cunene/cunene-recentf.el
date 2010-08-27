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

;; File to save the recent list into
(setq recentf-save-file (concat datafiles-dir "/recentf"))

;; Maximum number of items in the recentf menu
(setq recentf-max-menu-items 30)

;; Maximum number of files to remember
(setq recentf-max-saved-items 100)

;; to protect from TRAMP -- FIXME not correctly supported (yet) under Win32
;; (setq recentf-auto-cleanup 'never)

;; Remember recently opened files.
(require 'recentf)
(recentf-mode 1)
