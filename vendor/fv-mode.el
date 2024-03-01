;;; fv-mode --- JSON like file format.
;;;
;;; $Id: xml-parse.el,v 1.4 2001/05/12 22:36:13 ryants Exp $

;; Copyright (C) 2001 John Wiegley.

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Version: 0.0.1
;; Created: 2024
;; Keywords: exotic file formats

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;; Code:
(require 'rx)

(defconst fv-mode-standard-file-ext '(".fv")
  "List of FV file extensions.")

(defconst fv-mode-quoted-string-re
  (rx (group (char ?\")
             (zero-or-more (or (seq ?\\ ?\\)
                               (seq ?\\ ?\")
                               (seq ?\\ (not (any ?\" ?\\)))
                               (not (any ?\" ?\\))))
             (char ?\"))))

(defconst fv-mode-number-re (rx (group (one-or-more digit)
                                       (optional ?\. (one-or-more digit)))))

(defconst fv-mode-keyword-re (rx (group (or "#MISSING" "TRUE"))))

(defconst fv-mode-equal-key-re
  (rx (group (one-or-more alphanumeric)
             (optional ?\.)
             (optional ?:)
             (optional ?_)
             (one-or-more alphanumeric)
             (char ?=))))

(defconst fv-font-lock-keywords-1
  (list
   (list fv-mode-quoted-string-re 1 font-lock-keyword-face)
   (list fv-mode-number-re 1 font-lock-constant-face)
   (list fv-mode-keyword-re 1 font-lock-keyword-face)
   (list fv-mode-equal-key-re 1 font-lock-keyword-face))
  "Level one font lock.")

(define-derived-mode fv-mode prog-mode "FV"
  "Major mode for editing FV files."
  (set (make-local-variable 'font-lock-defaults) '(fv-font-lock-keywords-1 t))
  (set (make-local-variable 'comment-start) "#"))
(add-to-list 'auto-mode-alist '("\\.fv\\'" . fv-mode))

(provide 'fv-mode)
;;; fv-mode.el ends here
