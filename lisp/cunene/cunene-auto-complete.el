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

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                           (auto-complete-mode 1))))
(add-to-list 'load-path (concat dotfiles-dir "/other/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat dotfiles-dir "/other/auto-complete/dict"))
(ac-config-default)
(setq ac-comphist-file  "~/.emacs.d/data/ac-comphist.dat")
(real-global-auto-complete-mode t)

(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; ac-omni-completion-sources is made buffer local so
             ;; you need to add it to a mode hook to activate on
             ;; whatever buffer you want to use it with.  This
             ;; example uses C mode (as you probably surmised).
             ;; auto-complete.el expects ac-omni-completion-sources to be
             ;; a list of cons cells where each cell's car is a regex
             ;; that describes the syntactical bits you want AutoComplete
             ;; to be aware of. The cdr of each cell is the source that will
             ;; supply the completion data.  The following tells autocomplete
             ;; to begin completion when you type in a . or a ->
             (add-to-list 'ac-omni-completion-sources
                          (cons "\\." '(ac-source-semantic)))
             (add-to-list 'ac-omni-completion-sources
                          (cons "->" '(ac-source-semantic)))
             ;; ac-sources was also made buffer local in new versions of
             ;; autocomplete.  In my case, I want AutoComplete to use
             ;; semantic and yasnippet (order matters, if reversed snippets
             ;; will appear before semantic tag completions).
             (setq ac-sources '(ac-source-semantic ac-source-yasnippet))))
