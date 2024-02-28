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

(put 'org-roam-directory 'safe-local-variable (lambda (_) t))
(put 'org-roam-db-location 'safe-local-variable (lambda (_) t))

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
;; All packages we could not find in the repositories are stored here.
;;
(defvar cunene/vendor-packages
  (expand-file-name "~/.emacs.d/vendor")
  "Location for third-party packages.")

(require 'org-macs)
(require 'ob-tangle)
(defun cunene/tangle-and-load-file (org-file-name)
  "Tangle and load an org file.
ORG-FILE-NAME file to operate on."
  (let ((el-file-name (concat (file-name-sans-extension org-file-name) ".el"))
      (modification-time
         (file-attribute-modification-time (file-attributes org-file-name))))
    (message (concat "Tangling " org-file-name "..."))
    (unless (org-file-newer-than-p el-file-name modification-time)
      (org-babel-tangle-file org-file-name el-file-name "emacs-lisp")
      (byte-compile-file el-file-name))
    (message (concat "Loading " el-file-name "..."))
    (load-file el-file-name)))

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

  ;; Tangle and compile if necessary only, then load the configuration
  (let ((org-files
         (directory-files-recursively (concat user-emacs-directory "config") ".org")))
    (dolist (org-file org-files)
      (cunene/tangle-and-load-file org-file)))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))
;;; init.el ends here
