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

;; directory for all the auto save data.
(setq autosave-dir (concat datafiles-dir "/autosave/"))

;; file with information about auto saves for a session.
(setq auto-save-list-file-prefix (concat autosave-dir "saves-"))

;; actual auto save file
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Auto-save every 100 input events
(setq auto-save-interval 100)

;; Auto-save after 15 seconds idle time
(setq auto-save-timeout 15)
