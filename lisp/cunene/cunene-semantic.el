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

;; Save semantic data in one place
(setq semanticdb-default-save-directory (concat datafiles-dir "/semantic"))
(if (not (file-accessible-directory-p semanticdb-default-save-directory))
    (make-directory semanticdb-default-save-directory))

;; This enables coding tools such as intellisense mode decoration
;; mode, and stickyfunc mode plus enables which-func-mode, that shows
;; name of current function in status line; (plus regular code
;; helpers)
(semantic-load-enable-excessive-code-helpers)

;; Disable some annoying semantic decorations
(setq semantic-decoration-styles
      (append
       '(("semantic-decoration-on-includes" . t))
       '(("semantic-decoration-on-protected-members" . nil))
       '(("semantic-decoration-on-private-members" . nil))
       '(("semantic-tag-boundary" . nil))))

;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-idle-completions-mode nil)
(require 'semantic-decorate-include)

;; gcc setup
(require 'semantic-gcc)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;; smart complitions
(require 'semantic-ia)

;; Additional include directories
;; (semantic-add-system-include "~/code/boost/boost" 'c++-mode)

;;
;; Load all include files in the specified directory
;;
(defun imply-includes-in-directory (dir)
  "Add all header files in DIR to `semanticdb-implied-include-tags'."
  (let ((files (directory-files dir t "^.+\\.h[hp]*$" t)))
    (defvar-mode-local c++-mode semanticdb-implied-include-tags
      (mapcar (lambda (header)
                (semantic-tag-new-include
                 header
                 nil
                 :filename header))
              files))))

;; Adding all include files in gtk directory
;; (imply-includes-in-directory "/YourWxWidgetsPath/include/wx/gtk")

;; Add boost directory
;; (semantic-add-system-include "~/local/include/boost-1.41" 'c++-mode)

;; enable semantic folding
(global-semantic-folding-mode)

;; Implementing my own copy of this function since it is required by
;; semantic-ia-fast-jump but this function is not defined in etags.el
;; of GNU emacs
(require 'etags)
(unless (fboundp 'push-tag-mark)
  (defun push-tag-mark ()
    "Push the current position to the ring of markers so that
    \\[pop-tag-mark] can be used to come back to current position."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    )
  )

(global-set-key (kbd "C-c C-g") 'semantic-symref)
(global-set-key (kbd "C-c C-f") 'semantic-symref-symbol)
