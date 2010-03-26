;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; cunene is free software; you can redistribute it and/or modify it
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
;; along with cunene.  If not, see <http://www.gnu.org/licenses/>.

;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

;; Format the title-bar to include the buffer name
(setq frame-title-format "emacs - %b")

;; Flash instead of that annoying bell
(setq visible-bell t)

;; Do not use clever window splitting algorithms.
(setq split-width-threshold nil)

;; Display column numbers
(setq column-number-mode t)

;; Display column numbers
(setq line-number-mode t)

;; Display size of buffer
(size-indication-mode t)

;; Time and date
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Enable diary (needed for time and date for some reason)
;; (add-hook 'diary-hook 'appt-make-list)
;; (diary 0)
;; (custom-set-faces) FIXME

;; No menu bar
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; No tool-tips or toolbars. Enable mouse-wheel though.
(when window-system
  (mouse-wheel-mode t)
  (tooltip-mode -1)
  (tool-bar-mode -1))

;; coding system
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; Tabs
;;
(defun build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (nreverse ls)))

;; Spaces only for indentation
(set-default 'indent-tabs-mode nil)

;; Tab size

(setq tab-width 4)
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq standard-indent 4)
(setq-default tab-stop-list (build-tab-stop-list tab-width))
(setq tab-stop-list (build-tab-stop-list tab-width))

;; Seed the random-number generator
(random t)

;;
;; clipboard
;;

;; send copied regions to the OS clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;
;; Looks
;;
;; (set-default-font "Inconsolata Bold 11")
;; (set-default-font "DejaVu Sans Mono Bold 10")
(set-frame-font "Monospace Bold 11")
(set-cursor-color "wheat")
(set-background-color "black")
(set-foreground-color "wheat")
(set-face-foreground font-lock-comment-face "Plum")

;;
;; Syntax highlighting
;;

;; Enable syntax highlighting on all modes
(setq font-lock-global-modes t)

;; Highlight as much as possible
(setq font-lock-maximum-decoration t)

;;
;; New lines
;;

;; Don't add new lines when scrolling down at the bottom of a buffer
(setq next-line-add-newlines nil)

;; Ask before adding newline to file
(setq require-final-newline 'query)

;; C-k kills whole line and newline if at beginning of line
(setq kill-whole-line t)

;;
;; Enable disabled commands
;;

;; uppercase region command C-x C-u
(put 'upcase-region 'disabled nil)

;; lowercase region command C-x C-l
(put 'downcase-region 'disabled nil)

;;
;; automatically indenting yanked text if in programming-modes
;;
(defvar yank-indent-modes '(emacs-lisp-mode
                            c-mode c++-mode
                            tcl-mode
                            perl-mode cperl-mode
                            java-mode jde-mode
                            lisp-interaction-mode
                            LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked
text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text
   (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

;;
;; Fortune
;;
(defvar fortune-file (concat datafiles-dir "/fortune/fortune.txt")
  "The file that fortunes come from.")
(defvar fortune-strings nil "The fortunes in the fortune file.")

(defun open-fortune-file (file)
  (find-file file)
  (if (null fortune-strings)
      (let ((strings nil)
            (prev 1))
        (goto-char (point-min))
        (while (re-search-forward "^%$" (point-max) t)
          (push (buffer-substring-no-properties prev (- (point) 1))
                strings)
          (setq prev (1+ (point))))
        (push (buffer-substring-no-properties prev (point-max)) strings)
        (setq fortune-strings (apply 'vector strings)))))

;; (defun fortune2 ()
;;   "Get a fortune to display."
;;   (interactive)
;;   (when (null fortune-strings)
;;     (open-fortune-file fortune-file)
;;     (kill-buffer (current-buffer)))
;;   (let* ((n (random (length fortune-strings)))
;;          (chosen-fortune (aref fortune-strings n))
;;          (commands (loop for s being the symbols
;;                           when (commandp s) collect s))
;;          (command (nth (random (length commands)) commands))
;;          (chosen-command (concat "\n"
;;                                  (string (save-excursion
;;                                            (describe-function command)
;;                                            (switch-to-buffer "*Help*")
;;                                            (buffer-string)))
;;                                  (delete-other-windows))0 0))
;;     (setq initial-scratch-message
;;           (format "%s\n%s" chosen-fortune chosen-command))))

(defun fortune ()
  "Get a fortune to display."
  (interactive)
  (when (null fortune-strings)
    (open-fortune-file fortune-file)
    (kill-buffer (current-buffer)))
  (let* ((n (random (length fortune-strings)))
         (string (aref fortune-strings n)))
    (setq initial-scratch-message (format "%s" string))))

;;
;; Create a new scratch buffer
;;
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;
;; Remove tabs from buffer
;;
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;;
;; Indent entire buffer
;;
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;;
;; White space cleanup.
;;
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

;;
;; no emacs is complete without the power of the butterfly!
;;
(defun butterfly ()
  "Use butterflies to flip the desired bit on the drive platter.
   Open hands and let the delicate wings flap once.  The
   disturbance ripples outward, changing the flow of the eddy
   currents in the upper atmosphere.  These cause momentary
   pockets of higher-pressure air to form, which act as lenses
   that deflect incoming cosmic rays, focusing them to strike the
   drive platter and flip the desired bit.  You can type `M-x
   butterfly C-M-c' to run it.  This is a permuted variation of
   `C-x M-c M-butterfly' from url `http://xkcd.com/378/'."
  (interactive)
  (if (yes-or-no-p "Do you really want to unleash the powers of the butterfly? ")
      (progn
        (switch-to-buffer (get-buffer-create "*butterfly*"))
        (erase-buffer)
        (sit-for 0)
        (setq indent-tabs-mode nil)
        (animate-string "Amazing physics going on..."
                        (/ (window-height) 2) (- (/ (window-width) 2) 12))
        (sit-for (* 5 (/ (abs (random)) (float most-positive-fixnum))))
        (message "Successfully flipped one bit!"))
    (message "Well, then go to xkcd.com!")
    (browse-url "http://xkcd.com/378/")))

(global-set-key (kbd "C-x M-c M") 'butterfly)

;;
;; Key bindings
;;

;; Go to a line given by user.
(global-set-key (kbd "M-g") 'goto-line)

;; Revert buffer if file has changed.
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; Kill buffer without asking.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Comment region with programming language syntax.
(global-set-key (kbd "C-c C-c") 'comment-region)

;; Set the mark
(global-set-key (kbd "C-x SPC") 'set-mark-command)

;; Intelligent completion based on open buffers.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Start a shell
(global-set-key (kbd "C-x m") (lambda () (interactive) (shell)))

;; Full screen
(global-set-key (kbd "<f11>") 'fullscreen)

;; Capitalize word
(global-set-key (kbd "C-x C-p") 'capitalize-word)

