;; Author: Pavel Hlavnicka <pavel@gingerall.cz>
;; Maintainer: Pavel Hlavnicka <pavel@gingerall.cz>
;; Created: 16 May 2002
;; Version: 0.1

(provide 'gprof)

(require 'font-lock)

(defvar gprof-mode-syntax-table nil
  "Syntax table used while in gprof mode.")

;;; --------------------------------------------------
;;;setup the syntax table
;;; --------------------------------------------------
(unless gprof-mode-syntax-table
  (setq gprof-mode-syntax-table (make-syntax-table))
  ;;misc
  (modify-syntax-entry ?\_   "w"       gprof-mode-syntax-table)
  (modify-syntax-entry ?\.   "."       gprof-mode-syntax-table))

(defvar gprof-mode-font-lock-keywords nil
  "GProf mode keywords")

;;;------------------------------------------------------------
;;; local keymap
;;;------------------------------------------------------------

(defvar gprof-local-keymap nil)

(unless gprof-local-keymap
  (setq gprof-local-keymap (make-keymap))
  (define-key gprof-local-keymap [(mouse-3)] 'gprof-pop-link-history))

;;; --------------------------------------------------
;;; set keywords
;;; --------------------------------------------------
(if gprof-mode-font-lock-keywords
    ()
  (let ((gprof-function-line "^\\(\\[.+\\) \\[")
    (gprof-function-reference "\\(\\[[[:digit:]]+\\]\\)")
    (gprof-cycle "\\(<\\(sponta\\).*?>\\)"))

    (setq gprof-mode-font-lock-keywords
      (list (list gprof-function-line 1 'font-lock-function-name-face)
        ;;(list gprof-function-reference 1 'font-lock-constant-face)
        ;;(list gprof-cycle 1 'font-lock-type-face)
))))

(defvar gprof-link-keymap nil
  "The special keymap for GProf mode links")

(defvar gprof-link-history nil)

(if gprof-link-keymap
    ()
  (setq gprof-link-keymap (make-keymap))
  (define-key gprof-link-keymap [(mouse-1)] 'gprof-follow-link)
  (define-key gprof-link-keymap [(mouse-3)] 'gprof-pop-link-history))

(defun gprof-pop-link-history()
  (interactive)
  (let ((pos (pop gprof-link-history)))
    (if pos
    (progn
      (goto-char pos)
      (beginning-of-line))
      (error "No more links in history"))))


(defun gprof-follow-link()
  (interactive)
  (let* ((event last-command-event) pos olist rex)
    (cond ((eq (car event) 'mouse-1)
       (setq pos (nth 1 (cadr event)))))
    (when pos
      (push pos gprof-link-history)
      (setq olist (overlays-at pos))
      (while olist
    (when (overlay-get (car olist) 'gprof-link)
      (setq rex (overlay-get (car olist) 'gprof-href-rex))
      (goto-char 1)
      (re-search-forward rex)
      (beginning-of-line))
    (setq olist (cdr olist))))
))

(defvar gprof-overlay-category-href nil)
(unless gprof-overlay-category-href
  (put 'gprof-overlay-category-href 'gprof-link t)
  (put 'gprof-overlay-category-href 'local-map gprof-link-keymap)
  (put 'gprof-overlay-category-href 'face 'font-lock-constant-face)
  (put 'gprof-overlay-category-href 'mouse-face 'highlight))

(defvar gprof-overlay-category-cycle nil)
(unless gprof-overlay-category-cycle
  (put 'gprof-overlay-category-cycle 'gprof-link t)
  (put 'gprof-overlay-category-cycle 'local-map gprof-link-keymap)
  (put 'gprof-overlay-category-cycle 'face 'font-lock-type-face)
  (put 'gprof-overlay-category-cycle 'mouse-face 'highlight))

(defun gprof-make-link-overlays()
  (interactive)
  (message "Making hyperlinks")
  (save-excursion
    (goto-char 1)
    ;;delete old overlays
    (let ((olist (overlays-in (point-min) (point-max))))
      (mapc '(lambda(x)
           (when (overlay-get x 'gprof-link)
         (delete-overlay x)))
        olist))
    ;;find [ddd] links
    (while (re-search-forward "\\[[[:digit:]]+\\]$" nil t)
      (let* ((md (match-data))
         (beg (marker-position (nth 0 md)))
         (end (marker-position (nth 1 md)))
         (over (make-overlay beg end))
         (str (buffer-substring beg end)))
    (overlay-put over 'category 'gprof-overlay-category-href)
    (overlay-put over 'gprof-href-rex
             (format "^%s" (regexp-quote str)))))
    (goto-char 1)
    (while (re-search-forward "<cycle [[:digit:]]+>" nil t)
      (let* ((md (match-data))
         (beg (marker-position (nth 0 md)))
         (end (marker-position (nth 1 md)))
         (over (make-overlay beg end))
         (str (buffer-substring beg (1- end))))
    (overlay-put over 'category 'gprof-overlay-category-cycle)
    (overlay-put over 'gprof-href-rex (concat str " as a whole>"))))
    (message "Making hyperlinks ... done"))
)

;;; --------------------------------------------------
;;; major mode function
;;; --------------------------------------------------
(defun gprof-mode ()
  "Switch to the gprof major mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gprof-mode)
  (setq mode-name "GProf")

  ;; handle keymap
  (use-local-map gprof-local-keymap)

  ;;handle syntax table
  (set-syntax-table gprof-mode-syntax-table)
  ;;handle fontlock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gprof-mode-font-lock-keywords
                 nil t))

  ;;make link overlays
  (gprof-make-link-overlays)

  ;;miscellaneous
  (or buffer-read-only (toggle-read-only))

  ;;handle hook
  (run-hooks 'gprof-mode-hook))
