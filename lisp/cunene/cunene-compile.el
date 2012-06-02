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

(require 'compile)

;; Compilation command
(setq compile-command "make ")

;; Make the compilation and grep windows smaller.
(if (not window-system)
    (setq compilation-window-height 8)
  (setq compilation-window-height 14))

;; Scroll the compilation buffer automatically.
(setq compilation-scroll-output t)

;; Key bindings
(global-set-key (kbd "C-c c") 'compile)

;; Only go to error messages
(setq compilation-skip-threshold 2)

;; If a compilation buffer is already open, use it
(setq-default display-buffer-reuse-frames t)

;;
;; support for microsoft and mono compilation errors
;;

;; microsoft visual C/C++ errors
(add-to-list
 'compilation-error-regexp-alist-alist
 '(msvc-error
   "\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
: \\(error\\|warning\\) C[0-9]+:" 1 3))

;; mono and microsoft's C# errors
(add-to-list
 'compilation-error-regexp-alist-alist
 '(mcs-error
   "^\\(..*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 3 2))

;; mono and microsoft's C# warnings
(add-to-list
 'compilation-error-regexp-alist-alist
 '(mcs-warning
   "^\\(..*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 3 1))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(t4-error
   "^\\(..*\\)(\\([0-9]+\\),\\([0-9]+\\)): ERROR" 1 2 3))

(add-to-list 'compilation-error-regexp-alist 'msvc-error)
(add-to-list 'compilation-error-regexp-alist 'mcs-error)
(add-to-list 'compilation-error-regexp-alist 'mcs-warning)
(add-to-list 'compilation-error-regexp-alist 't4-error)


;; (defun my-compile ()
;;   "Run compile and resize the compile window"
;;   (interactive)
;;   (progn
;;     (call-interactively 'compile)
;;     (setq cur (selected-window))
;;     (setq w (get-buffer-window "*compilation*"))
;;     (select-window w)
;;     (setq h (window-height w))
;;     (shrink-window (- h 8))
;;     (select-window cur)
;;     )
;;   )

;; (defun my-compilation-hook ()
;;   "Make sure that the compile window is splitting vertically"
;;   (progn
;;     (if (not (get-buffer-window "*compilation*"))
;;         (progn
;;           (split-window-vertically)
;;           )
;;       )
;;     )
;;   )

;; (add-hook 'compilation-mode-hook 'my-compilation-hook)

;; Some code that will make it so the background color of the lines
;; that gcc found errors on, should be in another color.

;; (require 'custom)

;; (defvar all-overlays ())
;; (defun delete-this-overlay(overlay is-after begin end &optional len)
;;   (delete-overlay overlay)
;;   )

;; (defun highlight-current-line()
;;   (interactive)
;;   (setq current-point (point))
;;   (beginning-of-line)
;;   (setq beg (point))
;;   (forward-line 1)
;;   (setq end (point))
;;   ;; Create and place the overlay
;;   (setq error-line-overlay (make-overlay 1 1))

;;   ;; Append to list of all overlays
;;   (setq all-overlays (cons error-line-overlay all-overlays))

;;   (overlay-put error-line-overlay
;;                'face '(background-color . "pink"))
;;   (overlay-put error-line-overlay
;;                'modification-hooks (list 'delete-this-overlay))
;;   (move-overlay error-line-overlay beg end)
;;   (goto-char current-point)
;;   )

;; (defun delete-all-overlays()
;;   (while all-overlays
;;     (delete-overlay (car all-overlays))
;;     (setq all-overlays (cdr all-overlays))
;;     )
;;   )

;; (defun highlight-error-lines(compilation-buffer, process-result)
;;   (interactive)
;;   (delete-all-overlays)
;;   (condition-case nil
;;       (while t
;;         (next-error)
;;         (highlight-current-line)
;;         )
;;     (error nil))
;;   )

;; (setq compilation-finish-functions 'highlight-error-lines)
