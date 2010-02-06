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

;; Backup directory
(setq backup-dir (concat datafiles-dir "/backups/"))

;; Move the backup files to a sensible place
(setq backup-directory-alist `((".*" . ,backup-dir)))

;; Always use copying to create backup files (don't clobber symlinks)
(setq backup-by-copying t)

;; Make numeric backup versions
(setq version-control t)

;; Number of oldest versions to keep when a new numbered backup is made
(setq kept-old-versions 2)

;; Number of newest versions to keep when a new numbered backup is made
(setq kept-new-versions 10)

;; Delete excess backup versions silently
(setq delete-old-versions t)

;; Force a backup for each save
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; Delete old backups
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files backup-dir t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file))))
