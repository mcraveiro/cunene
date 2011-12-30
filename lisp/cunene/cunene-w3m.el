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

;; add w3m to load path.
(add-to-list 'load-path (concat dotfiles-dir "/other/emacs-w3m"))
(require 'w3m)
(require 'w3m-extension)

;; enable normal behaviour for function keys
(define-key w3m-mode-map [down] 'next-line)
(define-key w3m-mode-map [up] 'previous-line)
(define-key w3m-mode-map [right] 'forward-char)
(define-key w3m-mode-map [left] 'backward-char)

;; find html files with w3m
(global-set-key (kbd "C-c w") 'w3m-find-file)
(global-set-key (kbd "C-c C-s") 'w3m-search)

;; allow cookies
(setq w3m-use-cookies t)

;; other useful w3m variables
(setq w3m-default-display-inline-images t
      w3m-default-save-directory (concat datafiles-dir "/browser/downloads")
      w3m-home-page "http://www.google.co.uk/"
      w3m-init-file (concat datafiles-dir "/browser/emacs-w3m")
      ;;       w3m-command-arguments
      ;;       (nconc w3m-command-arguments
      ;;             ;; '("-o" "http_proxy=http://webcache.prc.sun.com:8080/"))
      ;;             ;; '("-o" "http_proxy=http://222.43.34.94:3128/"))
      ;;             '("-o" "http_proxy="))
      ;;       w3m-no-proxy-domains '(".edu.cn,166.111.,162.105.,net9.org"))
      )

;;
;; use w3m to open web pages in emacs, creating a new tab.
;;
(defun w3m-new-tab ()
  (interactive)
  (w3m-copy-buffer nil nil nil t))

(defun w3m-browse-url-new-tab (url &optional new-session)
  (interactive)
  (w3m-new-tab)
  (w3m-browse-url url))

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq browse-url-browser-function 'w3m-browse-url-new-tab)

;; open URL at point. disabled as we're relying on orgmode instead.
;; (global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; when working with html, see preview of the buffer.
(defun w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

(defun w3m-copy-url-at-point ()
  (interactive)
  (let ((url (w3m-anchor)))
    (if (w3m-url-valid url)
        (kill-new (w3m-anchor))
      (message "No URL at point!"))))

;; (add-hook 'w3m-mode-hook
;;           (lambda ()
;;             (local-set-key "\M-W" 'w3m-copy-url-at-point)))

;; save state
(defun w3m-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the current URL."
  (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

(add-hook 'w3m-mode-hook 'w3m-register-desktop-save)

(defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `w3m' buffer on `desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
    (let ((url d-b-misc))
      (when url
        (require 'w3m)
        (if (string-match "^file" url)
            (w3m-find-file (substring url 7))
          (w3m-goto-url-new-session url))
        (current-buffer)))))

(add-to-list 'desktop-buffer-mode-handlers
             '(w3m-mode . w3m-restore-desktop-buffer))
