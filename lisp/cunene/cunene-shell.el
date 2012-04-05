;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2011  Marco Craveiro
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
;;(require 'shell)

;; use pcomplete for completions
;; (define-key shell-mode-map (kbd "TAB") 'pcomplete)
;; (add-hook 'shell-mode-hook 'pcomplete-shell-setup)

;; add path to mini-buffer
(defun add-mode-line-dirtrack ()
   (add-to-list 'mode-line-buffer-identification
                '(:propertize (" " default-directory " "))))

(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

(eval-after-load 'shell
  '(progn
     (defadvice comint-send-input (before expand-input activate)
       "Expand input before sending"
       (expand-abbrev))
     (add-hook 'shell-mode-hook
               (lambda ()
                 (setq shell-dirtrackp nil)
                 (dirtrack-mode t)
                 (setq show-trailing-whitespace nil)))))
