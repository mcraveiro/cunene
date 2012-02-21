;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; init.el is free software; you can redistribute it and/or modify it
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
;; along with init.el.  If not, see <http://www.gnu.org/licenses/>.

;; Top level directory
(setq toplevel-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; Top level directory for all the lisp code.
(setq dotfiles-dir (concat toplevel-dir "lisp"))

;; Top level data directory
(setq datafiles-dir (concat toplevel-dir "data"))
(if (not (file-accessible-directory-p datafiles-dir))
    (make-directory datafiles-dir))

;; Directory for all of my customisations.
(setq cunene-dir (concat dotfiles-dir "/cunene/"))

;; Add other modes to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/utils"))

;; List of customisation files
;; FIXME: use load directory.
(setq cunene-files '(
                     "cunene-uniquify"
                     "cunene-auto-complete"
                     "cunene-elpa"
                     "cunene-misc"
                     "cunene-abbrev"
                     "cunene-ansi-color"
                     "cunene-anything"
                     "cunene-ascii-table"
                     "cunene-auto-save"
                     "cunene-autopair"
                     "cunene-backup"
                     "cunene-dos"
                     "cunene-bm"
                     "cunene-bongo"
                     "cunene-browse-kill-ring"
                     "cunene-c"
                     "cunene-camel-case"
                     "cunene-cdb-gud"
                     "cunene-cedet"
                     "cunene-color-theme"
                     "cunene-clearcase"
                     "cunene-cl"
                     "cunene-chm-view"
                     "cunene-cmake"
                     "cunene-compile"
                     "cunene-csharp"
                     "cunene-cua"
                     "cunene-desktop-save"
                     "cunene-diff"
                     "cunene-dired"
                     "cunene-doxymacs"
                     "cunene-delim-kill.el"
                     "cunene-eassist"
                     "cunene-ecb"
                     "cunene-ede"
                     "cunene-ediff"
                     "cunene-edef"
                     "cunene-eiffel"
                     "cunene-expand-region"
                     "cunene-ffap"
                     "cunene-fixme"
                     "cunene-flyspell"
                     "cunene-full-ack"
                     "cunene-fullscreen"
                     "cunene-fsharp"
                     "cunene-grep"
                     "cunene-gnus"
                     "cunene-gprof"
                     "cunene-gnuplot"
                     "cunene-hs"
                     "cunene-highlight-symbol"
                     "cunene-highline"
                     "cunene-ibuffer"
                     "cunene-ido"
                     "cunene-iedit"
                     "cunene-folding"
                     "cunene-jira"
                     "cunene-javascript"
                     "cunene-lisp"
                     "cunene-kitanda"
                     "cunene-magit"
                     "cunene-markdown"
                     "cunene-mo-git-blame"
                     "cunene-mic-paren"
                     "cunene-moccur"
                     "cunene-mpg123"
                     "cunene-multi-terminal"
                     "cunene-muse"
                     "cunene-nxhtml"
                     "cunene-nxml"
                     "cunene-org"
                     "cunene-pretty"
                     "cunene-point-stack"
                     "cunene-powershell"
                     "cunene-psvn"
                     "cunene-rainbow"
                     "cunene-recentf"
                     "cunene-ruby"
                     "cunene-savehist"
                     "cunene-saveplace"
                     "cunene-scratch"
                     "cunene-semantic"
                     "cunene-smooth-scrolling"
                     "cunene-smex"
                     "cunene-shell"
                     "cunene-shell-toggle"
                     "cunene-sql"
                     "cunene-ssh"
                     "cunene-swbuff"
                     "cunene-t4"
                     "cunene-text"
                     "cunene-tramp"
                     "cunene-visual-basic"
                     "cunene-w3m"
                     "cunene-winner"
                     "cunene-whitespace"
                     "cunene-yasnippet"
                     )
      )

;; Load the customisation files
(while cunene-files
  (load (concat cunene-dir (car cunene-files)))
  (setq cunene-files (cdr cunene-files)))

;; Make sure fortune and fullscreen are the last thing to execute
(fortune)
(fullscreen)
(message "Cunene v0.0.1 - Emacs configuration done.")
