;; swbuff.el --- Quick switch between Emacs buffers.

;; Copyright (C) 1998, 2000, 2001 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 12 Nov 1998
;; Version: 3.1
;; Keywords: extensions convenience
;; VC: $Id: swbuff.el,v 1.18 2002/01/09 11:45:23 ponce Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides the commands `swbuff-switch-to-next-buffer'
;; and `swbuff-switch-to-previous-buffer' to respectively switch to
;; the next or previous buffer in the buffer list.

;; The `swbuff-exclude-buffer-regexps' defines a list of regular
;; expressions for excluded buffers. The default setting excludes
;; buffers whose name begin with a blank character. To exclude all the
;; internal buffers (that is *scratch*, *Message*, etc...) you could
;; use the following regexps '("^ .*" "^\\*.*\\*").

;; Switching buffers pops-up a status window at the bottom of the
;; selected window. The status window shows the list of switchable
;; buffers where the switched one is hilighted using
;; `swbuff-current-buffer-face'. This window is automatically
;; discarded after any command is executed or after the delay
;; specified by `swbuff-clear-delay'.

;; Installation

;; Put this file on your Emacs-Lisp load path and add following into
;; your ~/.emacs startup file
;;
;;   (require 'swbuff)

;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce <david@dponce.com>.

;;; Change Log:

;; $Log: swbuff.el,v $
;; Revision 1.18  2002/01/09 11:45:23  ponce
;; Version 3.1.
;;
;; Revision 1.17  2001/09/19 11:13:00  ponce
;; (swbuff-window-lines): Use `swbuff-count-lines' instead of
;; `count-lines'.
;;
;; (swbuff-ignore): Alias of `ignore' used to prevent discarding the
;; status window on some mouse event.
;;
;; In compatibility code: Use mouse-1, mouse-3 on mode line buffer
;; identification to respectively switch to previous or next buffer.  And
;; mouse-2 to kill the current buffer.
;;
;; (swbuff-timer): New variable.  Timer used to discard the status
;; window.
;;
;; (swbuff-show-status-window, swbuff-pre-command-hook):  Use timer to
;; discard the status window.
;;
;; (swbuff-pre-command-hook):  Added `swbuff-kill-this-buffer' and
;; `swbuff-ignore' to the list of commands that do not discard the status
;; window.
;;
;; (swbuff-kill-this-buffer): New command to kill the current buffer.
;;
;; Revision 1.16  2001/03/26 06:09:56  ponce
;; `swbuff-layout-status-line' hide Emacs 21 header line.
;;
;; Revision 1.15  2001/03/06 12:16:24  ponce
;; New major version 3.0.
;; Now works with FSF Emacs 20.7, 21.1 and XEmacs 21.1.
;; New `swbuff-window-min-text-height' option to specify the minimum text
;; height of the status window.
;; New default face `swbuff-default-face' used for buffer names not
;; highlighted.
;; Simplified faces definitions.
;; New improved status window layout code.
;;
;; Revision 1.14  2000/05/12 15:23:59  david_ponce
;; Version 2.1.
;;
;; New options to customize the buffer list display (suggested by
;; "Shan-leung Maverick WOO" <sw77@cornell.edu>):
;;
;; - `swbuff-separator' defines a string used to separate buffer names.
;; - `swbuff-header' and `swbuff-trailer' define strings to enclose
;;   the buffer names list.
;; - `swbuff-separator-face' defines the face used to display the above
;;   separators.
;;
;; The default `swbuff-current-buffer-face' is now underlined.
;; Minor code improvements.
;;
;; Revision 1.13  2000/04/21 10:32:08  david_ponce
;; Version 2.0 released.
;;
;; Revision 1.12  2000/04/20 16:00:24  david_ponce
;; Added a new customizable variable `swbuff-status-window-layout' to
;; control the method used to ensure the switched buffer is always
;; visible when the buffer list is larger than the status window width.
;;
;; Revision 1.11  2000/04/19 14:00:03  david_ponce
;; Updated to use standard Emacs conventions for comments.
;;
;; Revision 1.10  2000/04/18 14:05:26  david_ponce
;; New major version.
;;  * swbuff now uses its own status window to display the buffer list.
;;    This fixes problem using the minibuffer with multiple frames.
;;  * Code cleanup:
;;    No more require cl.
;;    `swbuff-display-version' removed (use C-hv swbuff-version instead)
;;  * Documentation update.
;;
;; Revision 1.9  2000/01/17 10:56:57  ebat311
;; Fixed a little problem when switching to next buffer and current buffer
;; is excluded from the list of ones eligible for switching.
;;
;; Thanks to "Joe Casadonte" <joc@netaxs.com> who has reported this.
;;
;; Revision 1.8  1999-07-26 18:54:30+02  ebat311
;; Use Emacs/XEmacs compatible key mapping in `swbuff-default-load-hook'.
;;
;; Revision 1.7  1999-05-17 11:28:45+02  ebat311
;; Improved buffer list display:
;;   - The current highlighted buffer name is always visible.
;;     Previously, when the buffer list exceeded the size
;;     of the mini-buffer window the highlighted buffer name
;;     could be outside the displayed area.
;;   - New buffer name regexp handling to avoid bad highlighting
;;     of buffers which have a common part in their names.
;;
;; Revision 1.6  1999-05-07 13:48:31+02  ebat311
;; Removed a message displayed for debugging purpose.
;;
;; Revision 1.5  1999-05-07 13:45:33+02  ebat311
;; Improved buffer list display - the filenames list is kept static
;; (i.e., not shifting), while the current buffer highlight moves in
;; response to `swbuff-switch-to-next-buffer' and
;; `swbuff-switch-to-previous-buffer'.
;; No switching occurs if the eligible buffer list is empty.
;; Minor typo changes.
;;
;; Revision 1.4  1999-05-07 09:43:02+02  ebat311
;; Fixed a problem when no buffers are eligible for switching.
;; Added (require 'cl) to avoid problem using `mapcan' and
;; `notany' from `cl-extra'.
;; Simplified default exclude regexp from "^ .*" to "^ "
;; Thank you so much to "Paul Ford" <pford@chi.navtech.com>
;; for these fixes.
;;
;; Revision 1.3  1999-05-06 12:13:09+02  ebat311
;; Added a new customisable feature to exclude buffers whose
;; name matches a given list of regular expressions.
;;
;; Fixed - default key binding now works with XEmacs.
;;
;; Revision 1.2  1999-02-01 12:30:30+01  ebat311
;; No more use of `other-buffer' and `bury-buffer' so it
;; can now switch to any buffer in the `buffer-list'.
;;
;; Revision 1.1  1998/11/27 09:12:12  ebat311
;; Initial revision
;;

;;; Code:
(require 'timer)

(defconst swbuff-version "3.0 $Date: 2002/01/09 11:45:23 $"
  "swbuff version information.")

(defconst swbuff-status-buffer-name "*swbuff*"
  "Name of the working buffer used to display the buffer list.")

(defgroup swbuff nil
  "Quick switch between Emacs buffers."
  :group 'extensions
  :group 'convenience
  :prefix "swbuff-")

(defcustom swbuff-status-window-layout nil
  "*Method used to ensure the switched buffer is always visible.
This occurs when the buffer list is larger than the status window
width. The possible choices are:

- - 'Default' If there is only one window in the frame (ignoring the
              minibuffer one and the status window itself) the status
              window height is adjusted.
              Otherwise horizontal scrolling is used.
- - 'Scroll'  Horizontal scrolling is always used.
- - 'Adjust'  Only adjust the window height."
  :group 'swbuff
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Scroll"  scroll)
                 (const :tag "Adjust"  adjust)))

(defcustom swbuff-clear-delay 3
  "*Time in seconds to delay before discarding the status window."
  :group 'swbuff
  :type '(number :tag "seconds")) 

(defcustom swbuff-separator ", "
  "*String used to separate buffer names in the status line."
  :group 'swbuff
  :type 'string)

(defcustom swbuff-header ""
  "*Status line header string."
  :group 'swbuff
  :type 'string)

(defcustom swbuff-trailer ""
  "*Status line trailer string."
  :group 'swbuff
  :type 'string)

(defcustom swbuff-window-min-text-height 1
  "*Minimum text height of the status window."
  :group 'swbuff
  :type 'integer)

(defface swbuff-default-face '((t nil))
  "*Default face used for buffer names."
  :group 'swbuff)

(defface swbuff-current-buffer-face '((t (:foreground "red" :bold t :underline t)))
  "*Face used to highlight the current buffer name."
  :group 'swbuff)

(defface swbuff-separator-face '((t (:foreground "blue")))
  "*Face used for separators."
  :group 'swbuff)

(defcustom swbuff-exclude-buffer-regexps '("^ ")
  "*List of regular expressions for excluded buffers.
The default setting excludes buffers whose name begin with a blank
character.  To exclude all the internal buffers (that is *scratch*,
*Message*, etc...) you could use the following regexps:
  (\"^ \" \"^\\*.*\\*\")."
  :group 'swbuff
  :type '(repeat (regexp :format "%v")))

(defcustom swbuff-load-hook '(swbuff-default-load-hook)
  "*Hook run when package has been loaded.
See also `swbuff-default-load-hook'."
  :group 'swbuff
  :type 'hook)

(defun swbuff-include-p (name)
  "Return non-nil if buffer NAME can be included.
That is if NAME matches none of the `swbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote swbuff-status-buffer-name)
                  swbuff-exclude-buffer-regexps)))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun swbuff-buffer-list ()
  "Return the list of switchable buffers.
That is without the ones whose name matches
`swbuff-exclude-buffer-regexps'."
  (let ((blist (delq nil
                     (mapcar (function
                              (lambda (buf)
                                (and (swbuff-include-p (buffer-name buf))
                                     buf)))
                             (buffer-list)))))
    (or (memq (current-buffer) blist)
        (setq blist (cons (current-buffer) blist)))
    blist))

(if (fboundp 'count-lines)
    (defalias 'swbuff-count-lines 'count-lines)
  (defun swbuff-count-lines (start end)
    "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (if (eq selective-display t)
            (save-match-data
              (let ((done 0))
                (while (re-search-forward "[\n\C-m]" nil t 40)
                  (setq done (+ 40 done)))
                (while (re-search-forward "[\n\C-m]" nil t 1)
                  (setq done (+ 1 done)))
                (goto-char (point-max))
                (if (and (/= start end)
                         (not (bolp)))
                    (1+ done)
                  done)))
          (- (buffer-size) (forward-line (buffer-size))))))))

(defun swbuff-window-lines ()
  "Return the number of lines in current buffer.
This number may be greater than the number of actual lines in the
buffer if any wrap on the display due to their length."
  (swbuff-count-lines (point-min) (point-max)))

(defun swbuff-adjust-window (&optional text-height)
  "Adjust window height to fit its buffer contents.
If optional TEXT-HEIGHT is non-nil adjust window height to this
value."
  (setq text-height (max swbuff-window-min-text-height
                         (or text-height
                             (swbuff-window-lines))))
  (if (fboundp 'set-window-text-height)
      (set-window-text-height nil text-height)
    (let ((height (window-height))
          (lines  (+ 2 text-height)))
      (enlarge-window (- lines height))))
  (goto-char (point-min)))

;; Used to prevent discarding the status window on some mouse event.
(defalias 'swbuff-ignore 'ignore)

;;; Compatibility
(cond
 ;; GNU Emacs 21
 ((and (not (featurep 'xemacs))
       (> emacs-major-version 20))
  
  (defun swbuff-scroll-window (position)
    "Adjust horizontal scrolling to ensure that POSITION is visible."
    (setq truncate-lines t)
    (let ((automatic-hscrolling t))
      (goto-char position)))

  ;; Use mouse-1, mouse-3 on mode line buffer identification to
  ;; respectively switch to previous or next buffer.  And mouse-2 to
  ;; kill the current buffer.
  (let ((map mode-line-buffer-identification-keymap))
    (define-key map [mode-line mouse-1]
      'swbuff-switch-to-previous-buffer)
    (define-key map [mode-line drag-mouse-1]
      'swbuff-ignore)
    (define-key map [mode-line down-mouse-1]
      'swbuff-ignore)
    (define-key map [mode-line mouse-2]
      'swbuff-kill-this-buffer)
    (define-key map [mode-line mouse-3]
      'swbuff-switch-to-next-buffer))

  )
 
 ;; GNU Emacs 20 or XEmacs
 (t

  (defconst swbuff-extra-space 3
    "Extra space left in a line of the status window.
The default value correspond to the truncated glyphs + one space.")
  
  (defun swbuff-scroll-window (position)
    "Adjust horizontal scrolling to ensure that POSITION is visible."
    (setq truncate-lines t)
    ;; Don't display the XEmacs horizontal scrollbar
    (let* ((window (selected-window))
           (wdth (window-width window))
           (hscr (window-hscroll window)))
      (if (featurep 'xemacs)
          (set-specifier horizontal-scrollbar-visible-p nil window))
      (if (>= position (+ wdth hscr))
          (set-window-hscroll window (- (+ position swbuff-extra-space) wdth))
        (if (< position hscr)
            (set-window-hscroll window (- position swbuff-extra-space))))))
  
  ))

(defun swbuff-one-window-p (window)
  "Return non-nil if there is only one window in this frame ignoring
WINDOW and the minibuffer window."
  (let ((count 0))
    (walk-windows (function
                   (lambda (w)
                     (or (eq w window)
                         (setq count (1+ count))))))
    (= count 1)))

(defvar swbuff-buffer-list-holder nil
  "Hold the current displayed buffer list.")

(defun swbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let ((blist  swbuff-buffer-list-holder)
        (head   (or swbuff-header    "" ))
        (separ  (or swbuff-separator " "))
        (trail  (or swbuff-trailer   "" ))
        (width  (window-width window))
        (lines  0)
        (adjust (or (eq swbuff-status-window-layout 'adjust)
                    (swbuff-one-window-p window)))
        start end buffer bname fillr)
    (save-selected-window
      (select-window window)
      (setq header-line-format nil) ;; Hide Emacs 21 header line.
      (erase-buffer)
      (setq start (point))
      (insert head)
      (if (> (point) start)
          (set-text-properties
           start (point) '(face swbuff-separator-face)))
      (while blist
        (setq buffer (car blist)
              blist  (cdr blist))
        (when (buffer-live-p buffer)
          (setq bname (buffer-name buffer)
                start (point)
                fillr (if blist separ trail))
          (when (and adjust
                     (> (- (+ start (length bname) (length fillr))
                           (* lines width))
                        width))
            (newline)
            (setq start (point)
                  lines (1+ lines)))
          (insert bname)
          (cond
           ((string-equal bname bcurr)
            (setq end (point))
            (set-text-properties
             start end '(face swbuff-current-buffer-face)))
           (t
            (set-text-properties
             start (point) '(face swbuff-default-face))))
          (setq start (point))
          (insert fillr)
          (if (> (point) start)
              (set-text-properties
               start (point) '(face swbuff-separator-face)))))
      (if adjust
          (swbuff-adjust-window)
        (swbuff-adjust-window 1)
        (swbuff-scroll-window end)))))

(defvar swbuff-timer nil
  "Timer used to discard the status window.")

(defun swbuff-show-status-window ()
  "Pop-up a status window at the bottom of the selected window. The
status window shows the list of switchable buffers where the switched
one is hilighted using `swbuff-current-buffer-face'. It is
automatically discarded after any command is executed or after the
delay specified by `swbuff-clear-delay'."
  (if (or swbuff-buffer-list-holder
          (setq swbuff-buffer-list-holder (swbuff-buffer-list)))
      (let ((bcurr (buffer-name))
            (window-min-height 1)
            cursor-in-non-selected-windows)
        (with-current-buffer (get-buffer-create swbuff-status-buffer-name)
          (let ((w (or (get-buffer-window swbuff-status-buffer-name)
                       (split-window-vertically -2))))
            (set-window-buffer w (current-buffer))
            (swbuff-layout-status-line w bcurr)
            (add-hook 'pre-command-hook 'swbuff-pre-command-hook)
            (if (timerp swbuff-timer)
                (cancel-timer swbuff-timer))
            (setq swbuff-timer (run-with-timer
                                swbuff-clear-delay nil
                                #'swbuff-discard-status-window)))))
    (message "No buffers eligible for switching.")))

(defun swbuff-discard-status-window ()
  "Discard the status window."
  (let ((w (get-buffer-window swbuff-status-buffer-name))
        (b (get-buffer swbuff-status-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))

(defun swbuff-pre-command-hook ()
  "`pre-command-hook' used to track successive calls to switch commands."
  (if (memq this-command '(swbuff-switch-to-previous-buffer
                           swbuff-switch-to-next-buffer
                           swbuff-kill-this-buffer
                           swbuff-ignore))
      nil
    (swbuff-discard-status-window)
    (setq swbuff-buffer-list-holder nil))
  (if (timerp swbuff-timer)
      (cancel-timer swbuff-timer))
  (setq swbuff-timer nil)
  (remove-hook 'pre-command-hook 'swbuff-pre-command-hook))

(defun swbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((l (swbuff-buffer-list)))
    (and l (switch-to-buffer (nth (1- (length l)) l)))))

(defun swbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((l (nreverse (swbuff-buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l)))))

;;;###autoload
(defun swbuff-customize ()
  "Show the swbuff customization options panel."
  (interactive)
  (customize-group "swbuff"))

;;;###autoload
(defun swbuff-switch-to-previous-buffer ()
  "\\[swbuff-switch-to-previous-buffer] switch to the previous buffer
in the buffer list."
  (interactive)
  (swbuff-previous-buffer)
  (swbuff-show-status-window))

;;;###autoload
(defun swbuff-switch-to-next-buffer ()
  "\\[swbuff-switch-to-next-buffer] switch to the next buffer in the
buffer list."
  (interactive)
  (swbuff-next-buffer)
  (swbuff-show-status-window))

;;;###autoload
(defun swbuff-kill-this-buffer ()
  "Kill the current buffer.
And update the status window if showing."
  (interactive)
  (kill-buffer (current-buffer))
  (and (get-buffer-window swbuff-status-buffer-name)
       (swbuff-show-status-window)))

(defun swbuff-default-load-hook ()
  "Default hook run when package has been loaded.  Map the global keys
`C-f6' and `C-S-f6' respectively to the `swbuff-switch-to-next-buffer'
and `swbuff-switch-to-previous-buffer' commands."
  (global-set-key [(control f6)]       'swbuff-switch-to-next-buffer)
  (global-set-key [(control shift f6)] 'swbuff-switch-to-previous-buffer))

(provide 'swbuff)
(run-hooks 'swbuff-load-hook)

;;; swbuff.el ends here
