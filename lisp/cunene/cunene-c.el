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

;; Default these extensions to c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

;; Hook
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'innamespace 0) ;; Do not indent namespaces.
            (c-set-offset 'arglist-intro '+) ;; indent function args properly
            (c-set-offset 'arglist-cont-nonempty '+)
            (c-toggle-hungry-state 1)          ;; use hungry delete.
            (auto-fill-mode 1)                 ;; auto fill comments
            (set (make-local-variable 'comment-auto-fill-only-comments))
            (setq c-basic-offset tab-width)
            (setq c-default-style "stroustrup")))

;; Key bindings
(eval-after-load 'cc-mode
  '(progn
     ;; Ident when moving to a new line
     (define-key c-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     ;; Switch between header and implementation.
     (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
     ;; List methods.
     (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
     ;; Complete the current symbol via a menu based at point.
     (define-key c-mode-base-map (kbd "C-x C-m ?")
       'semantic-ia-complete-symbol-menu)
     ;; Open include file at point.
     (define-key c-mode-base-map (kbd "M-i")
       'semantic-decoration-include-visit)
     ;; Perform prompt completion to do in buffer completion.
     (define-key c-mode-base-map (kbd "C-c >")
       'semantic-complete-analyze-inline)
     ;; Toggle between the implementation, and a prototype function/class.
     (define-key c-mode-base-map (kbd "C-c p")
       'semantic-analyze-proto-impl-toggle)
     ;; show documentation for method at point
     (define-key c-mode-base-map (kbd "C-c C-d") 'semantic-ia-show-doc)
     ;; jump to definition of symbol at point
     (define-key c-mode-base-map (kbd "C-c C-j") 'semantic-ia-fast-jump)
     ;;   (local-set-key (kbd ".") 'semantic-complete-self-insert)
     ;;   (local-set-key (kbd ">") 'semantic-complete-self-insert))
     ))
