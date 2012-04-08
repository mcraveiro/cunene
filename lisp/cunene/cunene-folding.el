;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2012  Marco Craveiro
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

;; do not warn when enabling folding mode
(add-to-list 'safe-local-variable-values '(folded-file . t))

(defvar fringe-face 'fringe)
(custom-set-variables
 '(folding-font-lock-begin-mark 'fringe-face)
 '(folding-font-lock-end-mark 'fringe-face))
(defface collapsed-face '((t (:background "#e0cf9f" :foreground "#5f5f5f"))) "Collapsed Overlay")
(defvar collapsed-face 'collapsed-face)

(require 'folding)
(setq folding-check-folded-file-function 'iy-folding-check-folded)
(folding-mode-add-find-file-hook)
(global-set-key (kbd "M-s i") folding-mode-prefix-map)
(folding-add-to-marks-list 'ruby-mode "# {{{" "# }}}" nil t)
(folding-add-to-marks-list 'sh-mode "# {{{" "# }}}")
(define-key folding-mode-prefix-map (kbd "i") 'folding-shift-in)
(define-key folding-mode-prefix-map (kbd "o") 'folding-shift-out)
(define-key folding-mode-prefix-map (kbd "<SPC>") 'folding-context-next-action)
(define-key folding-mode-prefix-map (kbd "j") 'folding-next-visible-heading)
(define-key folding-mode-prefix-map (kbd "n") 'folding-next-visible-heading)
(define-key folding-mode-prefix-map (kbd "k") 'folding-previous-visible-heading)
(define-key folding-mode-prefix-map (kbd "p") 'folding-previous-visible-heading)

;; why open-fold is defined but not called in this function?
(defun folding-shift-in (&optional noerror)
  (interactive)
  (labels
      ((open-fold nil
                  (let ((data (folding-show-current-entry noerror t)))
                    (and data
                         (progn
                           (when folding-narrow-by-default
                             (setq folding-stack
                                   (if folding-stack
                                       (cons (cons (point-min-marker)
                                                   (point-max-marker))
                                             folding-stack)
                                     '(folded)))
                             (folding-set-mode-line))
                           (folding-narrow-to-region (car data) (nth 1 data)))))))
    (let ((goal (point)))
      (while (folding-skip-ellipsis-backward)
        (beginning-of-line)
        (open-fold)
        (goto-char goal))
      (if folding-narrow-by-default
          (open-fold)
        (widen)))))

;; add keywords to current buffer directly, overwrite the original
;; function in folding.el
(defun folding-font-lock-support ()
  "Add font lock support."
  (ignore-errors
    (font-lock-add-keywords nil (folding-font-lock-keywords major-mode))))

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(require 'fold-dwim)
(require 'fold-dwim-org)

(defun iy-folding-check-folded ()
  "Function to determine if this file is in folded form."
  (let ((folding-re1 "^.?.?.?{{{")
        (folding-re2 "[\r\n].*}}}"))
    (save-excursion
      (goto-char (point-min))
      ;;  If we found both, we assume file is folded
      (and (assq major-mode folding-mode-marks-alist)
           (< (point-max) 10000)
           (re-search-forward folding-re1 nil t)
           ;; if file is folded, there are \r's
           (re-search-forward "[\r\n]" nil t)
           (re-search-forward folding-re2 nil t)))))

(defun folding-marker-p (&optional pos)
  (eq (get-char-property (or pos (point)) 'face) 'fringe))

(defadvice fold-dwim-toggle (around toggle-folding-on-folding-marker activate)
  (if (folding-marker-p)
      (folding-toggle-show-hide)
    ad-do-it))

(defadvice forward-comment (around stop-at-folding-header (count) activate)
  (if (= 0 (ad-get-arg 0))
      (progn ad-do-it)
    (if (folding-marker-p)
        (setq ad-return-value nil)
      (let ((loop-times (abs count))
            (direction (/ count (abs count))))
        (ad-set-arg 0 direction)
        (setq ad-return-value t)
        (while (and (> loop-times 0) ad-return-value)
          ad-do-it
          (when ad-return-value
            (if (> direction 0)
              (if (folding-marker-p)
                  (setq ad-return-value nil)
                (when (folding-marker-p (- (point) 2))
                  (setq ad-return-value nil)
                  (forward-char -2)
                  (beginning-of-line)))
              (when (folding-marker-p)
                (end-of-line)
                (setq ad-return-value nil)))
            (setq loop-times (1- loop-times))))))))

(defadvice fold-dwim-hide-all (around folding-open-first activate)
  (if (and (boundp 'folding-mode) folding-mode)
      (progn
        (folding-uninstall)
        (let ((hs-hide-comments-when-hiding-all nil))
          ad-do-it)
        (folding-mode))
    ad-do-it))

(require 'hideshow)

;; http://code.google.com/p/bamanzi-misc/source/browse/trunk/_emacs.d/site-lisp/common/fold_/hideshow-fringe.el?r=122&spec=svn448
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format " (%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. M-s <SPC> to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 1 (length display-string) 'face 'collapsed-face display-string)
      (overlay-put ov 'display display-string))))
(setq hs-set-up-overlay 'display-code-line-counts)

(defadvice folding-subst-regions (around toggle-fringe (list find replace) activate)
  ad-do-it
  (save-excursion
    (while list
      (let* ((begin (car list))
             (end (cadr list))
             bol eol
             (marker-string "*fringe-dummy*")
             (marker-length (length marker-string)))
        (dolist (ov (overlays-in begin end))
          (when (overlay-get ov 'fringe-folding-p)
            (delete-overlay ov)))
        (when (and (eq find ?\n) (eq replace ?\r))
          ;; \\n -> \\r add fringe
          (goto-char begin)
          (search-forward "\r")
          (forward-char -1)
          (let* ((ov (make-overlay (point) end))
                 (display-string (format " (%d)..." (count-lines begin end))))
            (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
            (overlay-put ov 'before-string marker-string)
            (put-text-property 1 (length display-string) 'face 'collapsed-face display-string)
            (overlay-put ov 'display display-string)
            (overlay-put ov 'priority 9999)
            (overlay-put ov 'fringe-folding-p t))))
      (setq list (cdr (cdr list))))))

;; (global-set-key (kbd "M-s <SPC>") 'fold-dwim-toggle)
;; (global-set-key (kbd "M-s M-<SPC>") 'fold-dwim-toggle)
;; (global-set-key (kbd "M-s C-<SPC>") 'fold-dwim-hide-all)
;; (global-set-key (kbd "M-s M-<SPC>") 'fold-dwim-show-all)