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

;; Windows key bindings for selections, with support for rectangular regions.
(cua-mode t)

;; Changes mark behaviour to emulate Motif, MAC or MS-Windows cut and
;; paste style
(setq pc-selection-mode t)

;; Windows like behaviour for mark and selections. We don't need this
;; because pc-select already enabled it.
;; (setq transient-mark-mode t)

;; Region can be deleted by pressing a key. We don't need this
;; because pc-select already enabled it.
;; (setq delete-selection-mode t)

;; Don't tabify after rectangle commands
(setq cua-auto-tabify-rectangles nil)

;; Mark related options
(setq pc-select-meta-moves-sexps t)
(setq pc-select-selection-keys-only t)

;; time in seconds to delay before overriding prefix key. bump it up
;; by quite a bit as we don't really use the typical windows copy and
;; paste shortcuts (Ctrl-C, Ctrl-V, etc) and these overrides cause a
;; lot of pain on modes such as org-mode, etc.
(setq cua-prefix-override-inhibit-delay 1)
