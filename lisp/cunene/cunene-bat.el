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

(autoload 'bat-mode "bat-mode" "DOS and WIndows BAT files" t)

;; Extensions for bat mode
(add-to-list 'auto-mode-alist '("\\.[bB][aA][tT]$" . bat-mode))
(add-to-list 'auto-mode-alist '("CONFIG\\." . bat-mode))
(add-to-list 'auto-mode-alist '("AUTOEXEC\\." . bat-mode))
