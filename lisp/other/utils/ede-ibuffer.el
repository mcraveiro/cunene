;;; ede-ibuffer.el --- EDE project Ibuffer integration functions

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Niels Widger <niels.widger@...>

;; This file is not part of GNU Emacs.

;; ede-ibuffer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; EDE project Ibuffer integration functions.
;;
;; This file defines functions to allow `ibuffer' to filter
;; (`ibuffer-filter-by-ede-project'), mark
;; (`ibuffer-mark-by-ede-project-regexp'), and sort
;; (`ibuffer-do-sort-by-ede-project') the list of buffers based on
;; their EDE project root directory.
;;
;; This file also defines a new column whose symbol 'ede-project can
;; be used in the value of `ibuffer-formats' to display the EDE
;; project directory of each buffer and summarize the total active EDE
;; projects.  See below for an example usage of the new column symbol
;; in `ibuffer-formats'.
;;
;; Finally, `ede-set-ibuffer-saved-filter-groups' can be used to
;; generate a filter group for each active EDE project in
;; `ede-projects' and save them to `ibuffer-saved-filter-groups'.  See
;; below for an example addition to `ibuffer-hook' which regenerates
;; and loads these filter groups automatically.
;;
;; Installation:
;;
;; Put ede-buffer.el somewhere in your load-path and add the following
;; in your init file:
;;
;; (require 'ede-ibuffer)
;;
;; Below are example keybindings for the filter/mark/sort commands:
;;
;; (define-key ibuffer-mode-map (kbd "/ e") 'ibuffer-filter-by-ede-project)
;; (define-key ibuffer-mode-map (kbd "% e") 'ibuffer-mark-by-ede-project-regexp)
;; (define-key ibuffer-mode-map (kbd "s e") 'ibuffer-do-sort-by-ede-project)
;;
;; Below is an example value for `ibuffer-formats'.  When set, the
;; second format configuration replaces the filename-and-process
;; column with the new ede-project column (formats can be cycled with
;; `ibuffer-switch-format'):
;;
;; (setq ibuffer-formats
;;       '((mark modified read-only " "
;;           (name 18 18 :left :elide)
;;           " "
;;           (size 9 -1 :right)
;;           " "
;;           (mode 16 16 :left :elide)
;;           " " filename-and-process)
;;     (mark modified read-only " "
;;           (name 18 18 :left :elide)
;;           " "
;;           (size 9 -1 :right)
;;           " "
;;           (mode 16 16 :left :elide)
;;           " " ede-project)
;;     (mark " "
;;           (name 16 -1)
;;           " " filename)))
;;
;; Finally, below we use `ibuffer-hook' to call
;; `ede-set-ibuffer-saved-filter-groups' which regenerates the filter
;; groups for all active EDE projects and then runs
;; `ibuffer-switch-to-saved-filter-groups' in order to load the filter
;; groups each time we load or update Ibuffer:
;;
;; (add-hook 'ibuffer-hook
;;   (lambda ()
;;     (ede-set-ibuffer-saved-filter-groups)
;;     (ibuffer-switch-to-saved-filter-groups "ede-projects")))

;;; Code:

(require 'ede)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'ibuf-macs)

(define-ibuffer-filter ede-project
     "Toggle current view to buffers with EDE project root
directory matching QUALIFIER."
   (:description "ede-project"
         :reader (read-from-minibuffer "Filter by EDE project root
directory (regexp): "))
   (let* ((file (buffer-local-value 'buffer-file-name buf))
      (proj (cond (file (ede-current-project (file-name-directory file)))))
      (root (cond (proj (ede-project-root-directory proj)))))
     (when root
       (string-match qualifier root))))

(define-ibuffer-sorter ede-project
   "Sort buffers by EDE project root directory."
   (:description "ede-project")
   (let* ((fa (buffer-local-value 'buffer-file-name (car a)))
      (pa (cond (fa (ede-current-project (file-name-directory fa)))))
      (ra (cond (pa (ede-project-root-directory pa))))
      (ba (buffer-name (car a)))
      (fb (buffer-local-value 'buffer-file-name (car b)))
      (pb (cond (fb (ede-current-project (file-name-directory fb)))))
      (rb (cond (pb (ede-project-root-directory pb))))
      (bb (buffer-name (car b))))
     (cond ((and ra (not rb)) t)
       ((and (not ra) rb) nil)
       ((and (not ra) (not rb)) (string< ba bb))
       ((and ra rb) (cond ((string= ra rb) (string< ba bb)) (string< ra
rb))))))

(defun ede-ibuffer-column-summarizer (list)
   "Return summary indicating number of unique EDE projects in LIST."
   (interactive)
   (let ((l (length (delete-if (lambda (a) (or (not a) (string-match "^[\t]*$" a)))
                   (delete-dups list)))))
     (format "%d %s" l (cond (= l 1) ("project") ("projects")))))

(define-ibuffer-column ede-project
   (:name "EDE Project" :inline t :summarizer ede-ibuffer-column-summarizer)
   (let* ((file (buffer-local-value 'buffer-file-name buffer))
      (proj (cond (file (ede-current-project (file-name-directory file)))))
      (root (cond (proj (ede-project-root-directory proj)))))
     (if (not root) "" root)))

(defun ibuffer-mark-by-ede-project-regexp (regexp)
   "Mark all buffers whose EDE project root directory match REGEXP."
   (interactive "sMark by EDE project root directory (regexp): ")
   (ibuffer-mark-on-buffer
    #'(lambda (buf)
        (let* ((file (buffer-local-value 'buffer-file-name buf))
           (proj (cond (file (ede-current-project (file-name-directory
file)))))
           (root (cond (proj (ede-project-root-directory proj)))))
      (when root
        (string-match regexp root))))))

(defun ede-set-ibuffer-saved-filter-groups ()
   "Generate filter group for each project in `ede-projects'.
Store result in `ibuffer-saved-filter-groups."
   (interactive)
   (let ((projs ede-projects) (group '("ede-projects")) d x)
     (while projs
       (setq d (ede-project-root-directory (car projs)))
       (setq x (cons ede-project d))
       (add-to-list 'group (list d x) t)
       (setq projs (cdr projs)))
     (setq ibuffer-saved-filter-groups (list group))))

(provide 'ede-ibuffer)

;;; ede-ibuffer.el ends here