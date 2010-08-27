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

(require 'generic-x)

(define-generic-mode
  'edef-mode
  '()
  '("class" "name" "comment" "key_attribute" "class_attribute"
    "type" "begin" "end")
  '(
    ("\\(#.*\\)" 1 'font-lock-comment-face)
    ("unsigned" . 'font-lock-type-face)
    ("int" . 'font-lock-type-face)
    ("std::string" . 'font-lock-type-face)
    )
  '("\\edef$")
  nil
  "A mode for kitanda's EDEF files."
  )
