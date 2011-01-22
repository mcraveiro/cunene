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
(setq org-directory (concat datafiles-dir "/org"))
(setq org-default-notes-file (concat datafiles-dir "/org/todo.org"))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))

;; (add-hook 'remember-mode-hook 'org-remember-apply-template)
;; (setq org-remember-templates
;;       '(("Bug" ?b "* BUG %?\n  %i\n  %a" "~/org/BUGS.org" "Bugs" (emacs-lisp-mode))
;;         ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/JOURNAL.org" "X" my-check)
;;         ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/JOURNAL.org" "New Ideas")))

;; quick note taking.
(global-set-key (kbd "C-x /") 'org-remember)

;; use enter to follow links
;; (setq org-return-follows-link t)
;; (setq org-tab-follows-link t)

;; provide org-mode link functionality for all buffers.
(global-set-key (kbd "C-c l") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)

;; disable keys already taken by other modes
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-replace-disputed-keys t)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)))

;; make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
