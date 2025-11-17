;;; valgrind-leak-mode.el --- Major mode for editing Valgrind leak output files -*- lexical-binding: t; -*-

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
;; A major mode for editing Valgrind memcheck leak output files.
;; Provides syntax highlighting for process IDs, memory sizes, stack traces,
;; function names, and source file references.

;;; Code:

(defvar valgrind-leak-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat '=' as punctuation for process IDs
    (modify-syntax-entry ?= "." table)
    ;; Treat ':' as punctuation for addresses and line numbers
    (modify-syntax-entry ?: "." table)
    table)
  "Syntax table for `valgrind-leak-mode'.")

(defvar valgrind-leak-font-lock-keywords
  `(("==[0-9]+==" . font-lock-constant-face) ; Process IDs
    ("\\<\\([0-9]+\\) bytes in \\([0-9]+\\) blocks\\>" 1 font-lock-warning-face 2 font-lock-warning-face) ; Memory sizes and block counts
    ("\\<still reachable in loss record [0-9]+ of [0-9]+\\>" . font-lock-warning-face) ; Loss record
    ("\\<at 0x[0-9A-F]+: \\([A-Za-z0-9_]+\\) " 1 font-lock-function-name-face) ; Function names in stack trace
    ("\\<by 0x[0-9A-F]+: \\([A-Za-z0-9_]+\\) " 1 font-lock-function-name-face) ; Function names in stack trace
    ("(\\([A-Za-z0-9_./-]+\\):\\([0-9]+\\))" 1 font-lock-string-face 2 font-lock-constant-face) ; Source file and line number
    ("in \\(/[A-Za-z0-9_./-]+\\)" . font-lock-string-face) ; Library or executable paths
    ("<b>MPK</b>" . font-lock-keyword-face)) ; Bold MPK tag
  "Font lock keywords for `valgrind-leak-mode'.")

(define-derived-mode valgrind-leak-mode fundamental-mode "ValgrindLeak"
  "Major mode for editing Valgrind leak output files."
  :syntax-table valgrind-leak-mode-syntax-table
  (setq font-lock-defaults '(valgrind-leak-font-lock-keywords))
  (setq comment-start "#")
  (setq comment-end "")
  ;; Set tab width to 4 and disable tabs
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.valgrind-leak\\'" . valgrind-leak-mode))

(provide 'valgrind-leak-mode)

;;; valgrind-leak-mode.el ends here
