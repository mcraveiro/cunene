;;; wclock.el --- world clock

;;; Commentary:

;; Display a world clock in a buffer. The time is updated every minute.

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;; M-x wclock

;;; Code:

(defvar wclock-list '(("America/Los_Angeles" "Seattle")
                      ("America/New_York" "New York")
                      ("Europe/London" "London")
                      ("Europe/Paris" "Paris")
                      ("Asia/Calcutta" "Bangalore")
                      ("Asia/Tokyo" "Tokyo"))
  "alist containing timezone associated with the display name.")

(defvar wclock-time-format "%A %m %B %R %Z"
  "Format of the time displayed, see `format-time-string'.")

(defvar wclock-buffer-name "*wclock*"
  "Name of the wclock buffer")

(defvar wclock-timer-enable t
  "When set to t a timer will update the clock")

(defvar wclock-timer-second 60
  "Interval in seconds that the timer will run")

(defvar wclock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'kill-this-buffer)
    map)
  "Keymap of the wclock mode")

(defun wclock-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq
   major-mode 'wclock-mode
   mode-name "World clock")
  (use-local-map wclock-mode-map))

(defun wclock-display (list)
  (setq buffer-read-only nil
        buffer-undo-list t)
  (erase-buffer)
  (let ((max-width 0)
        (result ()))
    (while list
      (let* ((zone (pop list))
             (label (cadr zone))
             (width (string-width label)))
        (set-time-zone-rule (car zone))
        (setq result
              (append result
                      (list
                       label width (format-time-string wclock-time-format))))
        (when (> width max-width)
          (setq max-width width))))
    (set-time-zone-rule nil)
    (while result
      (insert (pop result)
              (make-string (1+ (- max-width (pop result))) ? )
              (pop result) "\n")))
  (delete-backward-char 1))

;;;###autoload
(defun wclock ()
  (interactive)
  (when (and wclock-timer-enable
             (not (get-buffer wclock-buffer-name)))
    (run-at-time t wclock-timer-second 'wclock-timer))
  (with-current-buffer (get-buffer-create wclock-buffer-name)
    (wclock-display wclock-list))
  (pop-to-buffer wclock-buffer-name)
  (fit-window-to-buffer)
  (wclock-mode))

(defun wclock-timer ()
  (if (get-buffer wclock-buffer-name)
      (with-current-buffer (get-buffer wclock-buffer-name)
        (wclock-display wclock-list))
    ;; cancel timer
    (let ((list timer-list))
      (while list
        (let ((elt (pop list)))
          (when (equal (symbol-name (aref elt 5)) "wclock-timer")
            (cancel-timer elt)))))))

(provide 'wclock)

;; Copyright (C) 2007 Ivan Kanis
;; Author: Ivan Kanis
;; $Id: wclock.el 1908 2007-06-21 14:29:37Z ivan $
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 