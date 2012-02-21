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

;; location of desktop files
(setq desktop-dirname (concat datafiles-dir "/desktop/")
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "lock"
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-files-not-to-save "^$" ; reload tramp paths
      desktop-restore-eager 0
      desktop-lazy-idle-delay 0
      desktop-lazy-verbose nil
      desktop-save-buffer t  ; saves buffer "status" (point, mark, etc) too
      desktop-load-locked-desktop t
      desktop-load-locked-desktop nil)

;; enable desktop mode
(desktop-save-mode 1)

;; what to save
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (kill-ring                . 20)
                (shell-command-history    . 50)
                register-alist)))

;; only use desktop mode and timers on server
(when (and (>= emacs-major-version 23) (daemonp))
  ;; save history and desktop periodically, since emacs is often killed,
  ;; not quite nicely.
  (run-with-timer 300 300
                  (lambda () (desktop-save-in-desktop-dir)
                    (savehist-save)
                    (message nil))))  ; clear the "Desktop saved in..." message
