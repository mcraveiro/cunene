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

;; Add w3m to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/emacs-w3m"))

;; Web browsing
(require 'w3m-load)

;; Allow cookies
(setq w3m-use-cookies t)

;; Web browsing
(global-set-key (kbd "C-c w") 'w3m-find-file)

(setq w3m-default-display-inline-images t
      w3m-default-save-directory (concat datafiles-dir "/browser/downloads")
      ;; w3m-home-page "http://localhost/"
      ;; w3m-init-file "~/.emacs.d/.emacs-w3m"
      ;;       w3m-command-arguments
      ;;       (nconc w3m-command-arguments
      ;;             ;; '("-o" "http_proxy=http://webcache.prc.sun.com:8080/"))
      ;;             ;; '("-o" "http_proxy=http://222.43.34.94:3128/"))
      ;;             '("-o" "http_proxy="))
      ;;       w3m-no-proxy-domains '(".edu.cn,166.111.,162.105.,net9.org"))
      )
