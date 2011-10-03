;; Note there are two themes in here.
;;
;; color-theme-tango -
;;  Color theme based on Tango Palette. Created by danranx@gmail.com
;;  http://www.emacswiki.org/cgi-bin/emacs/color-theme-tango.el
;;  Removed italics
;;
;; color-theme-tango-light -
;;  Original theme by matt harrison, for those who prefer a light
;;  background
;;
;; Installation
;;  Copy this file into your .emacs

;; size note: hardcode height ":height 100" is 100% note that doesn't respond to
;; ctl - +
;; Float numbers are relative sizes


(defvar butter1 "#fce94f") (defvar butter2  "#edd400") (defvar butter3 "#c4a000")
(defvar orange1 "#fcaf3e" ) (defvar orange2 "#f57900") (defvar orange3 "#ce5c00")
(defvar choc1 "#e9b96e") (defvar choc2 "#c17d11") (defvar choc3  "#8f5902")
(defvar cham1 "#8ae234") (defvar cham2 "#73d216") (defvar cham3 "#4e9a06")
(defvar blue1 "#729fcf") (defvar blue2 "#3465a4") (defvar blue3 "#204a87")
(defvar plum1 "#ad7fa8") (defvar plum2 "#75507b") (defvar plum3 "#5c3566")
(defvar red1 "#ef2929")  (defvar red2 "#cc0000")  (defvar red3 "#a40000")
(defvar alum1 "#eeeeec")  (defvar alum2 "#d3d7cf")  (defvar alum3 "#babdb6")
(defvar alum4 "#888a85")  (defvar alum5 "#555753")
(defvar alum6 "#2e3436")
;; since '(color-theme-tango is quoted below we need to unquote the
;; colors so the actual values get put in.  To be able to unquote
;; (with ,) change the '  to a `
;; ie `(color-theme-tango
;; list-faces-display will show all font faces

(defun color-theme-tango ()
  "A color theme based on Tango Palette."
  (interactive)
  (color-theme-install
   `(color-theme-tango
     ((background-color . ,alum6)
      (background-mode . dark)
      (border-color . ,alum4)
      (cursor-color . ,cham1)
      (foreground-color . ,alum1)
      (mouse-color . ,choc1))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (border ((t (:background ,alum4)))) ;; not usually visible unless
     ;; (scroll-bar-mode -1)
     (fringe ((t (:background ,alum5))))
     (mode-line ((t (:foreground ,alum1 :background ,alum5))))
     (region ((t (:background ,alum5))))
     (font-lock-builtin-face ((t (:foreground ,blue1))))
     (font-lock-comment-face ((t (:foreground ,alum4))))
     (font-lock-constant-face ((t (:foreground ,cham1))))
     (font-lock-doc-face ((t (:foreground ,alum4))))
     (font-lock-keyword-face ((t (:foreground ,blue1 :bold t))))
     ;; remove italic from strings
     (font-lock-string-face ((t (:foreground ,plum1))))
     (font-lock-type-face ((t (:foreground ,cham1 :bold t))))
     (font-lock-variable-name-face ((t (:foreground ,alum1))))
     (font-lock-warning-face ((t (:bold t :foreground ,orange2))))
     (font-lock-function-name-face ((t (:foreground ,butter2 :bold t :italic t))))
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground ,choc1))))
     (lazy-highlight ((t (:background ,cham1))))
     (isearch ((t (:background ,orange2 :foreground ,alum6))))
     (isearch-lazy-highlight-face ((t (:foreground ,alum6 :background ,choc1))))
     ;; paren.el defines colors!!! (override background)
     (show-paren-match-face ((t (:background ,alum5 :bold t))))
     (show-paren-mismatch-face ((t (:foreground ,plum1 :bold t))))
     (minibuffer-prompt ((t (:foreground ,blue1 :bold t))))
     (info-xref ((t (:foreground ,blue1))))
     (info-xref-visited ((t (:foreground ,plum1))))
     ;; ECB - matt added
     ;; see - http://ecb.sourceforge.net/docs/ecb-faces.html
     (ecb-default-highlight-face((t (:background ,plum2))))
     ;; need to figure out mouseover font, these don't work'
     ;;(ecb-directory-face((t (:background ,plum2))))
     ;;(ecb-source-face((t (:background "#204a87"))))
     ;;(ecb-method-face((t (:background "#204a87"))))
     ;;(ecb-tag-header-face((t (:background "#204a87"))))
     (ecb-tag-header-face((t (:background ,cham3))))
     (mumamo-background-chunk-submode ((t (:background ,alum5))))
     (bold ((t (:bold t))))
     (hl-line ((t (:background ,blue2))))
     (highlight ((t (:foreground ,butter2 :bold t))))
     (match ((t (:foreground ,cham2 :bold t))))
     (link ((t (:foreground ,cham1))))
     ;;(link-visited ((t (:foreground ,plum2))))
     (whitespace-empty ((t (:foreground ,red3 :background ,butter2))))
     (whitespace-line ((t (:background ,alum5))))
     (vertical-border ((t (:foreground ,plum2 :background ,plum2))))
     (org-done ((t (:foreground ,cham1 :bold t))))
     (org-todo ((t (:foreground ,red1 :bold t))))
     (org-date ((t (:foreground ,plum1))))
     (org-level-1 ((t (:foreground ,blue3 :bold t))))
     (org-level-2 ((t (:foreground ,blue1 :bold t))))
     (org-level-3 ((t (:foreground ,alum5 :bold t))))
     (org-level-4 ((t (:foreground ,alum4 :bold t))))
     (org-level-5 ((t (:foreground ,blue3))))
     (org-level-6 ((t (:foreground ,blue1))))
     (org-level-7 ((t (:foreground ,alum5))))
     (org-level-8 ((t (:foreground ,alum4))))
     ;; rst sets background color!!! (override background)
     (rst-level-1-face ((t (:background ,alum6 :foreground ,blue3 :bold t))))
     (rst-level-2-face ((t (:background ,alum6 :foreground ,blue1 :bold t))))
     (rst-level-3-face ((t (:background ,alum6 :foreground ,alum5 :bold t))))
     (rst-level-4-face ((t (:background ,alum6 :foreground ,alum4 :bold t))))
     (rst-level-5-face ((t (:background ,alum6 :foreground ,blue3))))
     (rst-level-6-face ((t (:background ,alum6 :foreground ,blue1))))
     (flymake-errline ((t (:background ,red3))))
     (flymake-warnline ((t (:background ,plum3))))
     (flyspell-duplicate ((t (:foreground ,butter1 :underline t))))
     (flyspell-incorrect ((t (:foreground ,orange3 :underline t))))
     (anything-dir-priv ((t (:foreground ,plum1 :background ,alum6))))
     (anything-dir-heading ((t (:foreground ,cham2 :background ,alum6))))
     (anything-file-name ((t (:foreground ,blue1))))
     (anything-header ((t (:foreground ,red1 :background ,alum6 :bold t))))
     )))


(defun color-theme-tango-light ()
  "A color theme based on Tango Palette."
  (interactive)
  (color-theme-install
   `(color-theme-tango-light
     ((background-color . ,alum1)
      (background-mode . dark)
      (border-color . ,alum4)
      (cursor-color . ,cham3)
      (foreground-color . ,alum6)
      (mouse-color . ,choc1))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (border ((t (:background ,alum4))))
     (fringe ((t (:background "grey10"))))
     ;; active buffer
     (mode-line ((t (:foreground ,alum6 :background ,alum3))))
     (region ((t (:background ,alum2))))
     (font-lock-builtin-face ((t (:foreground ,blue3))))
     (font-lock-comment-face ((t (:foreground ,alum4))))
     (font-lock-constant-face ((t (:foreground ,cham3))))
     (font-lock-doc-face ((t (:foreground ,alum4))))
     (font-lock-keyword-face ((t (:foreground ,blue3 :bold t))))
     ;; remove italic from strings
     (font-lock-string-face ((t (:foreground ,red3))))
     (font-lock-type-face ((t (:foreground ,cham3 :bold t))))
     (font-lock-variable-name-face ((t (:foreground ,alum6))))
     (font-lock-warning-face ((t (:bold t :foreground ,orange2))))
     (font-lock-function-name-face ((t (:foreground ,plum3 :bold t :italic t))))
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground ,choc1))))
     (isearch ((t (:background ,orange3 :foreground ,alum6))))
     (isearch-lazy-highlight-face ((t (:foreground ,alum6 :background ,alum1))))
     ;; paren.el defines colors
     (show-paren-match-face ((t (:background ,alum2 :foreground ,cham2 :bold t))))
     (show-paren-mismatch-face ((t (:foreground ,plum3 :bold t))))
     (minibuffer-prompt ((t (:foreground ,blue3 :bold t))))
     (info-xref ((t (:foreground ,blue3))))
     (info-xref-visited ((t (:foreground ,plum3))))
     ;; ECB - matt added
     ;; see - http://ecb.sourceforge.net/docs/ecb-faces.html
     (ecb-default-highlight-face((t (:background ,plum2))))
     ;; need to figure out mouseover font, these don't work'
     ;;(ecb-directory-face((t (:background ,plum2))))
     ;;(ecb-source-face((t (:background "#204a87"))))
     ;;(ecb-method-face((t (:background "#204a87"))))
     ;;(ecb-tag-header-face((t (:background "#204a87"))))
     (ecb-tag-header-face((t (:background ,cham3))))
     (mumamo-background-chunk-submode ((t (:background ,alum2))))
     (highlight ((t (:background ,alum2))))
     (link ((t (:foreground ,cham3))))
     ;; (link-visited ((t (:foreground ,plum2))))
     (whitespace-empty ((t (:foreground ,red3 :background ,butter2))))
     (whitespace-line ((t (:background ,alum2))))
     )))

;; color theme (requires http://www.emacswiki.org/cgi-bin/wiki?ColorTheme )
(require 'color-theme)
(provide 'tango-theme)
;; (provide 'color-theme-tango-light)

