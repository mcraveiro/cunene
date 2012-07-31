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

;; fix weird error during compilation
(setq warning-suppress-types nil)

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

;; indent after new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; do not ask for confirmation when creating new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; highlight incremental search
(setq search-highlight t)

;; repeat pop mark command without the need for C-u
(setq set-mark-command-repeat-pop t)

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

;; disable stack traces on errors (the annoying backtrace buffer)
(setq stack-trace-on-error nil)

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
(setq standard-indent 4)
(setq-default tab-stop-list (build-tab-stop-list tab-width))
(setq tab-stop-list (build-tab-stop-list tab-width))

;; Seed the random-number generator
(random t)

;;
;; clipboard
;;

;; send copied regions to the OS clipboard
(if (<= emacs-major-version 23)
    (progn
      (setq x-select-enable-clipboard t)
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))

;;
;; Looks
;;

;; re-display regardless of whether input is available.
(setq redisplay-dont-pause t)

(defun fontify-frame (frame)
  ;; good fonts:
  ;;
  ;; - Inconsolata Bold 17
  ;; - DejaVu Sans Mono Bold 10
  ;; - Droid Sans Mono Bold 15
  ;; - Monospace Bold 9
  (if (not (eq system-type 'windows-nt))
      (if (< (frame-width) 100)
          (set-frame-parameter frame 'font "Inconsolata Bold 12")
        (set-frame-parameter frame 'font "Inconsolata Bold 16"))
    (set-frame-parameter frame 'font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))
  (set-frame-parameter frame 'cursor-color "wheat")
  (set-frame-parameter frame 'foreground-color "wheat")
  (set-frame-parameter frame 'background-color "black")
  (set-face-foreground font-lock-comment-face "Plum")
  )

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;;
;; Syntax highlighting
;;

;; Enable syntax highlighting on all modes
(setq font-lock-global-modes t)

;; Highlight as much as possible
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)

;;
;; New lines
;;

;; Don't add new lines when scrolling down at the bottom of a buffer
(setq next-line-add-newlines nil)

;; Ask before adding newline to file
;; (setq require-final-newline 'query)

;; do not ask before adding newline to file
(setq inhibit-default-init t)

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
      (indent-region beg end nil))
  (whitespace-cleanup-region beg end)
)

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
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun delete-file-and-buffer ()
  "Deletes current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-file-and-buffer)

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
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Set the mark
(global-set-key (kbd "C-x SPC") 'set-mark-command)

;; Intelligent completion based on open buffers.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Full screen
(global-set-key (kbd "<f11>") 'fullscreen)

;; Capitalize word
(global-set-key (kbd "C-x C-p") 'capitalize-word)

;; Back to first character in line
(global-set-key (kbd "M-n") 'back-to-indentation)

;; enable compression mode
(auto-compression-mode 1)

;;
;; wind/frame move
;;
(require 'windmove)
(require 'framemove)

(global-set-key (kbd "C-c C-<left>")  'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>")    'windmove-up)
(global-set-key (kbd "C-c C-<down>")  'windmove-down)
(setq framemove-hook-into-windmove t)

;; confirm exit
(global-set-key
 (kbd "C-x C-c")
 '(lambda ()
    (interactive)
    (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ?" 4 nil)
        (save-buffers-kill-emacs))))

(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%Y-%m-%e %H:%M:%S" (current-time))))

(global-set-key "\C-cd" 'insert-date)

(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

(if (eq system-type 'windows-nt)
    (progn (setq null-device "/dev/null")
           (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
           (add-hook 'comint-output-filter-functions
                     'comint-watch-for-password-prompt nil t)
           (setq explicit-shell-file-name "bash.exe")
           ;; For subprocesses invoked via the shell
           ;; (e.g., "shell -c command")
           (setq shell-file-name explicit-shell-file-name)

           ;; (require 'cygwin-mount)
           ;; (cygwin-mount-activate)
           ))

;;
;; improvements to mark commands
;;
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark]
  'exchange-point-and-mark-no-activate)

(defun space2underscore-region (start end)
  "Replace space by underscore in region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward " " nil t) (replace-match "_"))))

(defun underscore2space-region (start end)
  "Replace underscore by space in region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "_" nil t) (replace-match " "))))

(defun replace-underscore-space-toggle ()
  "Replace underscore/space in the current region or line.
If the current line contains more “_” char than space,
then replace them to space, else replace space to _.
If there's a text selection, work on the selected text."
  (interactive)
  (let (li bds)
    (setq bds
          (if (region-active-p)
              (cons (region-beginning) (region-end))
            (bounds-of-thing-at-point 'line)))
    (setq li (buffer-substring-no-properties (car bds) (cdr bds)))
    (if (> (count 32 li) (count 95 li))
        (progn (replace-string " " "_" nil (car bds) (cdr bds)))
      (progn (replace-string "_" " " nil (car bds) (cdr bds))))))

(defun cycle-hyphen-underscore-space ()
  "Cyclically replace {underscore, space, hypen} chars current
 line or text selection.  When called repeatedly, this command
 cycles the {“ ”, “_”, “-”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0
  ;; to length of charList.
  (let (mainText charList p1 p2 currentState nextState changeFrom
             changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (progn (setq startedWithRegion-p nil )
             (setq p1 (line-beginning-position))
             (setq p2 (line-end-position)) ) )

    (setq charList (list " " "_" "-" ))

    (setq currentState
          (if (get 'cycle-hyphen-underscore-space 'state)
              (get 'cycle-hyphen-underscore-space 'state) 0))
    (setq nextState (% (+ currentState (length charList) 1) (length charList)))

    (setq changeFrom (nth currentState charList))
    (setq changeTo (nth nextState charList))

    (setq mainText
          (replace-regexp-in-string changeFrom changeTo
                                    (buffer-substring-no-properties p1 p2)))
    (delete-region p1 p2)
    (insert mainText)

    (put 'cycle-hyphen-underscore-space 'state nextState)

    (when startedWithRegion-p
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil))))

(global-set-key (kbd "C-c C--") 'cycle-hyphen-underscore-space)

(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (list ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

(global-set-key (kbd "C-c C-=") 'diff-buffer-with-associated-file)

(defun de-context-kill (arg)
  "Kill buffer"
  (interactive "p")
  (if (and (buffer-modified-p)
             buffer-file-name
             (not (string-match "\\*.*\\*" (buffer-name)))
             ;; erc buffers will be automatically saved
             (not (eq major-mode 'erc-mode))
             (= 1 arg))
    (let ((differences 't))
      (when (file-exists-p buffer-file-name)
        (setq differences (diff-buffer-with-associated-file)))

      (if (y-or-n-p (format "Buffer %s modified; Kill anyway? " buffer-file-name))
          (progn
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer)))))
    (if (and (boundp 'gnuserv-minor-mode)
           gnuserv-minor-mode)
        (gnuserv-edit)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(global-set-key (kbd "C-x k") 'de-context-kill)
