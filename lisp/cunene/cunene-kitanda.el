;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; cunene is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Cunene is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cunene.  If not, see <http://www.gnu.org/licenses/>.

;; copied from quant lib.
(defvar kitanda-file-variables
  "/* vim: set sw=4: -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
")

(defvar kitanda-license
"
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
")

(defun kitanda-new-header ()
  "skeleton for a new header file"
  (interactive)
  (insert kitanda-file-variables)
  (kitanda-add-license)
  (kitanda-add-file-description)
  (insert "\n")
  (kitanda-add-include-guard)
  (kitanda-add-sample-header)
  (insert "\n")
  (kitanda-add-namespace))

(defun kitanda-new-source ()
  "skeleton for a new source file"
  (interactive)
  (insert kitanda-file-variables)
  (kitanda-add-license)
  (insert "\n")
  (kitanda-add-sample-header)
  (insert "\n")
  (kitanda-add-namespace))

;; key bindings
(global-set-key (kbd "C-c C-,") 'kitanda-new-header)
(global-set-key (kbd "C-c C-.") 'kitanda-new-source)

(require 'cc-mode)
(defvar c++-font-lock-extra-types)

;; a few types for syntax-highlighting from kitanda
(setq c++-font-lock-extra-types
      (append c++-font-lock-extra-types
              '("kitanda"
                "domain" "currency" "repository" "version")))

;; types from boost:
(setq c++-font-lock-extra-types
      (append c++-font-lock-extra-types
              '("boost"
                "shared_ptr" "format")))

(defun kitanda-add-license ()
  (let ((holder (read-from-minibuffer "Copyright holder? "
                                      "Marco Craveiro")))
    (let ((copyright-notice
           (apply 'string (append "/*\n"
                                  " * Copyright (C) "
                                  (substring (current-time-string) -4)
                                  " "
                                  holder
                                  "\n"
                                  ()))))
      (insert "\n"
              copyright-notice
              kitanda-license
              "*/\n\n"))))

(defun kitanda-add-file-description ()
  (let ((filename (buffer-name))
        (description (read-from-minibuffer "Short file description? ")))
    (insert "/** @file " filename "\n"
            " *  @brief " description "\n"
            " */\n")))

(defun kitanda-add-include-guard ()
  (let ((guard (read-from-minibuffer "Include guard? " "KITANDA_")))
    (insert "#ifndef " guard "\n"
            "#define " guard "\n"
            "\n\n\n"
            "#endif\n"))
  (previous-line 3))

(defun kitanda-add-sample-header ()
  (insert "#include <kitanda/>\n"))


(defun kitanda-add-namespace ()
  (insert "namespace kitanda {\n"
          "\n\n\n"
          "}\n")
  (previous-line 3)
  (c-indent-command))

