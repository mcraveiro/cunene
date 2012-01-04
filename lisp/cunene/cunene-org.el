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

(add-to-list 'load-path (concat dotfiles-dir "/other/org-mode"))
(load-file (concat dotfiles-dir "/other/org-mode/lisp/org-install.el"))

(require 'ob-gnuplot)

(setq org-directory (concat datafiles-dir "/org"))
(setq org-default-notes-file (concat datafiles-dir "/org/todo.org"))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))

;; preserve source code indentation on code blocks
(setq org-src-preserve-indentation t)

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
            (setq org-CUA-compatible t)
            (org-set-local 'yas/trigger-key [tab])
            ))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "VERIFY" "|" "DONE")))

;; avoid using keys already taken by other modes such as pc-select
(setq org-replace-disputed-keys t)

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

;; Number of clock tasks to remember in history.
(setq org-clock-history-length 100)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; log time when marking task as done
(setq org-log-done 'time)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

;; Change task to STARTED when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)

;; Separate drawers for clocking and logs
;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes
;; clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Save the running clock and all clock history when exiting Emacs,
;; load it on startup
(setq org-clock-persist (quote history))

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(defun insert-org-timestamp()
  "Insert a time-stamp in org mode format"
  (interactive)
  (insert (format-time-string "[%Y-%m-%e %H:%M:%S]" (current-time))))

;; let babel evaluate without asking
(setq org-confirm-babel-evaluate nil)

;; enabled languages for org-babel code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (gnuplot . t)
   (R . t)))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))
