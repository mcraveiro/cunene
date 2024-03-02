;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Created: February 22, 2021
;; Homepage: https://github.com/mcraveiro/cunene

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(setq-default
 load-prefer-newer t                    ;; Load newest version of lisp code.
 mode-line-format nil                   ;; No mode line format
 package-enable-at-startup nil          ;; Use installed packages after init file.
 package-native-compile t               ;; Use native compilation.
 org-src-preserve-indentation t         ;; Do not add indentation to src blocks.
 message-log-max 100000                 ;; Keep a lot more messages
 )

(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)

(setq-default
 default-frame-alist
 '(
   (font . "Source Code Pro 9")         ;; Font to use
   (fullscreen . fullboth)              ;; Maximize the window by default
   (horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (left-fringe . 8)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;;
;; Org-mode configuration location
;;
(defvar cunene/org-config
  (expand-file-name "~/.emacs.d/config")
  "Location for `org-mode' configuration.")
(add-to-list 'load-path cunene/org-config)

;;
;; All packages we could not find in the repositories are stored here.
;;
(defvar cunene/vendor-packages
  (expand-file-name "~/.emacs.d/vendor")
  "Location for third-party packages.")
(add-to-list 'load-path cunene/vendor-packages)

;;
;; Any code which we do not want to push to the public repo goes under here.
;;
(defvar cunene/site-lisp
  (expand-file-name "~/.emacs.d/site-lisp")
  "Location for site specific packages.")
(add-to-list 'load-path cunene/site-lisp)

(defun cunene/compile-and-load-file (el-file-name &optional compile-only)
  "Compile EL-FILE-NAME into ELC and load it unless COMPILE-ONLY."
  (interactive)
  (let ((elc-file-name (concat (file-name-sans-extension el-file-name) ".elc")))
    (unless (file-newer-than-file-p elc-file-name el-file-name)
      (save-restriction
        (setq byte-compile-warnings
              '(not free-vars obsolete unresolved callargs redefine
                  obsolete noruntime cl-warnings interactive-only))
        (byte-compile-file el-file-name)))
    (unless compile-only
      (message (concat "Loading " elc-file-name "..."))
      (load-file elc-file-name))
    ))

(require 'org-macs)
(require 'ob-tangle)
(defun cunene/tangle-and-load-file (org-file-name)
  "Tangle and load an org file.
ORG-FILE-NAME file to operate on."
  (let ((el-file-name (concat (file-name-sans-extension org-file-name) ".el"))
        (elc-file-name (concat (file-name-sans-extension org-file-name) ".elc"))
        (modification-time
         (file-attribute-modification-time (file-attributes org-file-name))))
    (message (concat "Tangling " org-file-name "..."))
    (unless (org-file-newer-than-p el-file-name modification-time)
      (org-babel-tangle-file org-file-name el-file-name "emacs-lisp"))
    (cunene/compile-and-load-file el-file-name)))

(defun cunene/files-for-extension (dir extension)
  "Return all files at DIR that have EXTENSION, if any."
  (if (file-directory-p dir)
      (directory-files-recursively dir extension)))

;; FIXME: when troubleshooting package issues
;; (require 'package)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")))

;; (package-initialize)

(let
    (
     ;; Configuration specific to startup.
     ;; For notes on GC, see:
     ;; - https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
     (default-directory user-emacs-directory)  ;; Start in the emacs directory.
     (file-name-handler-alist nil)             ;; Remove special handlers on startup.
     (gc-cons-percentage .6)
     (gc-cons-threshold most-positive-fixnum)
     (read-process-output-max (* 1024 1024)))

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(org-babel-tangle t))))
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Compile vendor packages but do not load; Loading is done by org
  ;; configuration.
  (mapc
   (lambda (vendor-file) (cunene/compile-and-load-file vendor-file t))
   (cunene/files-for-extension cunene/vendor-packages ".el$"))

  ;; Tangle and compile if necessary, then load the configuration
  (mapc 'cunene/tangle-and-load-file
        (cunene/files-for-extension cunene/org-config ".org$"))

  ;; Load site-specific lisp code, if any exists.
  (mapc 'cunene/compile-and-load-file
        (cunene/files-for-extension cunene/site-lisp ".el$"))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))
;;; init.el ends here
