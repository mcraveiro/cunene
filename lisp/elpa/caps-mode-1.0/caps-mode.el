;;; caps-mode.el --- (minor mode) letters are inserted capitalized

;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>,
;; Christoph Conrad <nospam@spamgourmet.com>, Kevin Rodgers
;; <ihs_4664@yahoo.com>, Kim F. Storm <no-spam@cua.dk>,
;; Stefan Monnier <monnier@iro.umontreal.ca>

;; Version: 1.0

;; Time-stamp: <jac -- Thu Oct  7 13:09:00 CDT 2004>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a simple minor mode in which all letters are inserted in
;; captialized form.  It works for Latin-1 as well as ASCII.

;; Note that this mode has absolutely nothing to do with the toggle
;; state of your capslock.

;;; Code:

(defun caps-mode-self-insert-command (&optional n)
  "Like `self-insert-command', but uppercase the the typed character."
  (interactive "p")
  (insert-char (upcase last-command-char) n))

(defvar caps-mode-map
  (let ((map (make-keymap)))
    ;; Or with Emacs-CVS:
    ;; (define-key map [remap self-insert-command] 'caps-mode-self-insert-command)
    (substitute-key-definition 'self-insert-command
                                   'caps-mode-self-insert-command
                                   map global-map)
    map))

;;;###autoload
(define-minor-mode caps-mode
  "Toggle caps mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When caps mode is enabled, all letters are inserted in their
capitalized form."
  :init-value nil
  :lighter " Caps")

;;; caps-mode.el ends here
