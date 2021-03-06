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

;; Add yas to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/yasnippet"))

(require 'yasnippet)

;; Personal snippet directory
(setq yas/root-directory (concat datafiles-dir "/yasnippets"))
(setq yas/snippet-dirs (concat datafiles-dir "/yasnippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)

;; for GPL snippet
(setq user-full-name "Marco Craveiro")
(setq user-mail-address "marco.craveiro@gmail.com")

(yas/initialize)