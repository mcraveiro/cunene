;;; valgrind-suppression-mode.el --- Major mode for editing Valgrind suppression files -*- lexical-binding: t -*-

;; Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A major mode for editing Valgrind suppression files.
;; Provides syntax highlighting for suppression blocks, Memcheck types,
;; function/object patterns, and specific keywords like "fun:" and "...".

;;; Code:

(defvar valgrind-suppression-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat '#' as a comment starter
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Treat '{' and '}' as punctuation
    (modify-syntax-entry ?{ "." table)
    (modify-syntax-entry ?} "." table)
    table)
  "Syntax table for `valgrind-suppression-mode'.")

(defvar valgrind-suppression-font-lock-keywords
  `(("^\\s-*#.*$" . font-lock-comment-face) ; Comments
    ("^{\\s-*\\([^}\n]*\\)\\s-*}" . (1 font-lock-function-name-face)) ; Suppression block name
    ("\\<Memcheck:[A-Za-z]+\\>" . font-lock-keyword-face) ; Memcheck types
    ("\\<match-leak-kinds:[A-Za-z, ]+\\>" . font-lock-type-face) ; match-leak-kinds
    ("\\<fun:\\([A-Za-z0-9:_]+\\)\\>" . ((1 font-lock-keyword-face) (2 font-lock-function-name-face))) ; "fun:" and function name
    ("\\<obj:[A-Za-z0-9*/._-]+\\>" . font-lock-variable-name-face) ; Object patterns
    ("\\<\\(\\.\\.\\.\\)\\>" . font-lock-constant-face)) ; Ellipsis
  "Font lock keywords for `valgrind-suppression-mode'.")

(define-derived-mode valgrind-suppression-mode fundamental-mode "ValgrindSupp"
  "Major mode for editing Valgrind suppression files."
  :syntax-table valgrind-suppression-mode-syntax-table
  (setq font-lock-defaults '(valgrind-suppression-font-lock-keywords))
  (setq comment-start "#")
  (setq comment-end "")
  ;; Set tab width to 4 and disable tabs
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.supp\\'" . valgrind-suppression-mode))

(provide 'valgrind-suppression-mode)

;;; valgrind-suppression-mode.el ends here
