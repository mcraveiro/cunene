;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; Cunene is free software; you can redistribute it and/or modify it
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
;; along with Cunene.  If not, see <http://www.gnu.org/licenses/>.

;;
;; Create a new scratch buffer
;;
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(autoload 'scratch "scratch" nil t)

(defvar persistent-scratch-filename
  (concat datafiles-dir "/persistent-scratch/persistent-scratch.el")
  "Location of *scratch* file contents for persistent-scratch.")

(defvar persistent-scratch-backup-directory
  (concat datafiles-dir "/persistent-scratch/")
  "Location of backups of the *scratch* buffer contents for
    persistent-scratch.")

(if (not (file-accessible-directory-p persistent-scratch-backup-directory))
    (make-directory persistent-scratch-backup-directory))

(defun make-persistent-scratch-backup-name ()
  "Create a filename to backup the current scratch file by
  concatenating PERSISTENT-SCRATCH-BACKUP-DIRECTORY with the
  current date and time."
  (concat
   persistent-scratch-backup-directory
   (replace-regexp-in-string
    (regexp-quote " ") "-" (current-time-string))))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (with-current-buffer (get-buffer "*scratch*")
    (if (file-exists-p persistent-scratch-filename)
        (copy-file persistent-scratch-filename
                   (make-persistent-scratch-backup-name)))
    (write-region (point-min) (point-max)
                  persistent-scratch-filename)))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (shell-command (format "cat %s" persistent-scratch-filename)
                       (current-buffer)))))

(load-persistent-scratch)
(push #'save-persistent-scratch kill-emacs-hook)
