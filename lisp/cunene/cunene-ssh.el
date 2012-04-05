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

(autoload 'ssh "ssh" "SSH mode." t)

;; (add-hook 'ssh-mode-hook 'ssh-directory-tracking-mode)
;; (setq ssh-directory-tracking-mode t)

(eval-after-load 'ssh
  '(progn
     (add-hook 'ssh-mode-hook
               (lambda ()
                 (shell-dirtrack-mode t)
                 (setq dirtrackp nil)
                 (setq show-trailing-whitespace nil)))

     (defadvice ssh (around ssh-always-local first activate)
       (let ((default-directory "~/"))
         ad-do-it))))
