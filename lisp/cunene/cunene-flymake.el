;; -*- mode: emacs-lisp; tab-width: 4; indent-tabs-mode: nil -*-
;;
;; Copyright (C) 2012 Marco Craveiro <marco.craveiro@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(require 'flymake)
(require 'flymake-settings)
(require 'flymake-extension)

;; display errors as a show tip
(setq flymake-extension-use-showtip t)

;; ensure we can check C++ header files
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.hpp\\'" flymake-c++-init))

;; insure flymake errors get plopped into the *Messages* buffer
(setq flymake-log-level 3)

;; change fonts
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))

;; Overwrite flymake-display-warning so that no annoying dialog box is
;; used.

;; This version uses lwarn instead of message-box in the original version.
;; lwarn will open another window, and display the warning in there.
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (lwarn 'flymake :warning warning))

;; Using lwarn might be kind of annoying on its own, popping up windows and
;; what not. If you prefer to recieve the warnings in the mini-buffer, use:
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (message warning))
