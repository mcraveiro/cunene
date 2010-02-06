;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; init.el is free software; you can redistribute it and/or modify it
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
;; along with init.el.  If not, see <http://www.gnu.org/licenses/>.

;; File to remember ido directories
;; FIXME: IDO doesn't seem to be using this file
(setq ido-save-directory-list-file (concat datafiles-dir "/ido/ido.last"))

;; Intelligent completion
(require 'ido)

;; Enable it
(ido-mode t)

;; FIXME: explain these
(setq ido-enable-prefix nil)
(setq ido-execute-command-cache nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point t)
(setq ido-max-prospects 10)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq svn-status-use-ido-completion 1)

;; Hooks

;;
;; Command completion with ido
;;
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s)
                                   ido-execute-command-cache))))))
       ido-execute-command-cache)))))

;;
;; recentf with ido.
;;
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(add-hook 'ido-setup-hook
          (lambda ()
            (setq ido-enable-flex-matching t)
            (global-set-key "\M-x" 'ido-execute-command)))


;;
;; Key bindings
;;

;; Command completion with ido
(eval-after-load 'ido-setup-hook
  '(progn
     (global-set-key (kbd "M-x") 'ido-execute-command)))

;; Recent files with ido
(global-set-key (kbd "C-x w") 'recentf-ido-find-file)

;; Buffer switching with ido
(global-set-key (kbd "C-b") 'ido-switch-buffer)
