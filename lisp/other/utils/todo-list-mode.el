;;; todo-list-mode.el -- Major mode for highlighting a numbered todo list.

;; Copyright (C) 2009 Billy Lamberta

;; Author: Billy Lamberta <billy@lamberta.org>
;; Created: Jan 2009
;; Keywords: todo
;; URL: http://www.lamberta.org/blog/todo-list-mode
;; $Revision: 0.01 $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Description:

;; Major mode for highlighting a numbered todo list.

;; This is a simple mode that uses a number of regexp's to
;; determine the color of a particular line based on the assigned
;; priority number.

;; The format for a given line is:
;; number (0-4), [optional lower case letter], tab, task text
;; It will also recognize hash style comments to the end of the line.

;; Example:
;; 0  This is a header line that will always be on top.
;; 1  Run around the block.
;; 1  Wash behind my ears.
;; 2a Learn some more Lisp.
;; 2b Fly a kite.
;; 3  Say I'm sorry.
;; 4  Peace to all mankind.

;; Faces are defined in this file below. Feel free to add more or
;; change the colors to suit your own style.

;; Sort on the assigned priority number:
;; Select a region (C-x h for entire buffer), M-x sort-lines

;;; Code:

(setq todo-list-highlight-regexps '(
   ;;regexp keyword matches:
   ;;beg line|number|letter?|tab|any char|any chars|end line
   ("^0[a-z]?\t.*$" 0 todo-list-zero-face t)
   ("^1[a-z]?\t.*$" 0 todo-list-one-face t)
   ("^2[a-z]?\t.*$" 0 todo-list-two-face t)
   ("^3[a-z]?\t.*$" 0 todo-list-three-face t)
   ("^4[a-z]?\t.*$" 0 todo-list-four-face t)
   ("^C\t.*$"0	todo-list-complete-face t)
   ("#.*$" 0 font-lock-comment-face t)
))

(define-derived-mode todo-list-mode fundamental-mode
  "todo-list-mode"
  "Major mode for syntax color highlighting of a numbered todo list."

  (setq font-lock-defaults '(todo-list-highlight-regexps))
)

;;
;;define custom faces for todo-list mode
;;
(defface todo-list-zero-face'(
  (((class color) (background dark)) (:foreground "white" :slant italic))
  (((class color) (background light)) (:foreground "black" :slant italic))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 0 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-zero-face 'todo-list-zero-face
  "Todo-List mode face used for level 0 task.")

(defface todo-list-one-face'(
  (((class color) (background dark))	(:foreground "IndianRed1"))
  (((class color) (background light))	(:foreground "Red3"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 1 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-one-face 'todo-list-one-face
  "Todo-List mode face used for level 1 task.")

(defface todo-list-two-face'(
  (((class color) (background dark))	(:foreground "CadetBlue2"))
  (((class color) (background light))	(:foreground "Blue3"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 2 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-two-face 'todo-list-two-face
  "Todo-List mode face used for level 2 task.")

(defface todo-list-three-face'(
  (((class color) (background dark))	(:foreground "MistyRose2"))
  (((class color) (background light))	(:foreground "DarkOrange3"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 3 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-three-face 'todo-list-three-face
  "Todo-List mode face used for level 3 task.")

(defface todo-list-four-face'(
  (((class color) (background dark))	(:foreground "LightSteelBlue1"))
  (((class color) (background light))	(:foreground "DodgerBlue2"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for level 4 task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-four-face 'todo-list-four-face
  "Todo-List mode face used for level 4 task.")

(defface todo-list-complete-face'(
  (((class color) (background dark))	(:foreground "gray22"))
  (((class color) (background light))	(:foreground "gray75"))
  (t (:bold t :italic t)))
  "Todo-List mode face used for completed task."
  :group 'todo-list-mode-highlighting-faces)
(defvar todo-list-complete-face 'todo-list-complete-face
  "Todo-List mode face used for completed task.")


(provide 'todo-list-mode)
