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
(require 'gnus)
(require 'nnir)

(setq user-full-name "Marco Craveiro")
(setq user-mail-address "marco.craveiro@gmail.com")
(setq message-from-style 'angles)
(setq message-syntax-checks '((sender . disabled)))
(setq message-generate-headers-first t)
(setq mail-self-blind t)

(setq gnus-use-cache t)
(setq gnus-home-directory "~/.emacs.d/data/gnus")
(setq message-directory "~/.emacs.d/data/gnus/mail")
(setq gnus-cache-directory "~/.emacs.d/data/gnus/cache/")
(setq gnus-dribble-directory "~/.emacs.d/data/gnus/")
(setq nndraft-directory "~/.emacs.d/data/gnus/drafts")
(setq gnus-cache-enter-articles '(ticked dormant read unread))
(setq gnus-cache-remove-articles nil)
(setq gnus-cacheable-groups "^nnimap")

;; disable threaded display
(setq gnus-show-threads nil)
(setq gnus-article-sort-functions '(not gnus-summary-sort-by-most-recent-date))

;; set buttons to normal
(setq gnus-article-button-face 'widget-button)

;; Sort unthreaded article by date
(setq gnus-article-sort-functions '(gnus-article-sort-by-date))

;; always read dribble file
(setq gnus-always-read-dribble-file t)

;; Prettier line format
(setq gnus-group-line-format "%p%M%B%S%P%(%G: %y%)\n")

;; quit quietly
(setq gnus-interactive-exit nil)

;; make large newsgroups really large.
(setq gnus-large-newsgroup 300)

;; do not use .newsrsc
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

;; use lower case suffix for score files
(setq gnus-score-file-suffix "score")

;; go offline withouth asking
(setq gnus-server-unopen-status 'offline)

;; put group name in full in mode line
(setq gnus-summary-mode-line-format "Gnus: %G %Z")

;; defaut summary line format
(setq gnus-summary-line-format "%(%U%R%z%&user-date; %-20,20f %B%s%)\n")

;; suppress duplicate articles
(setq gnus-suppress-duplicates t)

;; Makes presentation more compact by hiding thread subtree
;; (setq gnus-thread-hide-subtree t)

;; Default thread sorting
;; (setq gnus-thread-sort-functions '(gnus-thread-sort-by-total-score))

;; Headers in the groups buffer
(setq gnus-topic-line-format "%i%(%{%n%}%)%v\n")

;; prevent save dribble file message
(setq gnus-use-dribble-file nil)

;; Fancy date format
(setq gnus-user-date-format-alist '(((gnus-seconds-today) . "%k:%M    ")
                                    (604800 . "%a %k:%M") ; one week
                                    ((gnus-seconds-month) . "%a %d   ")
                                    ((gnus-seconds-year) . "%b %d   ")
                                    (t . "%b %d '%y"))) ; fall through

;; html renderer
(setq  mm-text-html-renderer 'w3m)

;; display picture
(setq mm-inline-text-html-with-images t)
(setq mm-w3m-safe-url-regexp "\\(\\.*cid\\|\\.\\(jpg\\|gif\\|png\\)\\)$")


;; (setq gnus-thread-sort-functions '(gnus-thread-sort-by-number
;;                                    (not gnus-thread-sort-by-most-recent-date)))
;;      gnus-large-newsgroup nil)

;; (setq gnus-article-sort-functions '(gnus-summary-sort-by-most-recent-date))
;; (setq gnus-article-sort-functions '(gnus-article-sort-by-score))
;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-date gnus-thread-sort-by-total-score))
;; (setq gnus-thread-score-function 'max)

;; Save gnus score files
(setq gnus-save-score t)
;; Set this to t if you've got a slow connection
(setq gnus-asynchronous nil)

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-authinfo-file "~/.authinfo")
                                  (nnimap-nov-is-evil t)
                                  (nnir-search-engine imap)
                                  ))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "marco.craveiro@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "yourcompany.com")

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; (defvar my-mails "marco.craveiro@google\\.com")

;; (defun gnus-user-format-function-j (headers)
;;   (let ((to (gnus-extra-header 'To headers)))
;;     (if (string-match my-mails to)
;;         (if (string-match "," to) "~" "»")
;;       (if (or (string-match my-mails
;;                             (gnus-extra-header 'Cc headers))
;;              (string-match my-mails
;;                            (gnus-extra-header 'BCc headers)))
;;           "~"
;;         " "))))

;; (setq gnus-user-date-format-alist
;;       '(((gnus-seconds-today) . "Today, %H:%M")
;;         ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
;;         (604800 . "%A %H:%M") ;;that's one week
;;         ((gnus-seconds-month) . "%A %d")
;;         ((gnus-seconds-year) . "%B %d")
;;         (t . "%B %d '%y"))) ;;this one is used when no other does match

;; (setq gnus-summary-line-format
;;       (concat "%U%R %~(pad-right 2)t%* %uj %B%~(max-right 30)~(pad-right 30)n  "
;;               "%~(max-right 90)~(pad-right 90)s %-135=%&user-date;\n"))
;; (gnus-demon-add-handler `gnus-demon-scan-news 5 5)
;; (gnus-demon-init)

(setq gnus-summary-same-subject "")

(copy-face 'default 'mysubject)
(setq gnus-face-1 'mysubject)
(copy-face 'default 'mytime)

(set-face-foreground 'mytime "lavender")
(setq gnus-face-2 'mytime)

(copy-face 'default 'mythreads)
(set-face-foreground 'mythreads "dark sea green")
(setq gnus-face-3 'mythreads)

(copy-face 'default 'mygrey)
(set-face-foreground 'mygrey "grey")
(setq gnus-face-4 'mygrey)

(copy-face 'default 'myblack)
(set-face-foreground 'myblack "grey60")
(setq gnus-face-5 'myblack)

(copy-face 'default 'mybiggernumbers)
(set-face-foreground 'mybiggernumbers "olive drab")
(setq gnus-face-6 'mybiggernumbers)
