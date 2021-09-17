;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Created: February 22, 2021
;; Homepage: https://github.com/mcraveiro/cunene

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(setq-default
 load-prefer-newer t                    ;; Load newest version of lisp code.
 ;; mode-line-format nil                ;; No mode line format
 package-enable-at-startup nil          ;; Use installed packages after init file.
 package-native-compile t               ;; Use native compilation.
 org-src-preserve-indentation t         ;; Do not add indentation to src blocks.
 )

(setq-default
 default-frame-alist
 '(
   ;; (background-color . "#101010")       ;; Default background color
   ;; (foreground-color . "#FAFAFA")       ;; Default foreground color
   (font . "Source Code Pro 10")           ;; Font to use
   (fullscreen . maximized)             ;; Maximize the window by default
   (horizontal-scroll-bars . nil)       ;; No horizontal scroll-bars
   (left-fringe . 8)                    ;; Thin left fringe
   (menu-bar-lines . 0)                 ;; No menu bar
   (right-divider-width . 1)            ;; Thin vertical window divider
   (right-fringe . 8)                   ;; Thin right fringe
   (tool-bar-lines . 0)                 ;; No tool bar
   ;; (undecorated . t)                    ;; Remove extraneous X decorations
   (vertical-scroll-bars . nil)))       ;; No vertical scroll-bars

;;
;; All packages we could not find in the repositories are stored here.
;;
(defvar cunene/vendor-packages
  (expand-file-name "~/.emacs.d/vendor")
  "Location for third-party packages.")

;;
;; Hydra method, needs to evaluate before we can load the main init.
;;
(defun cunene/hydra-heading (&rest headings)
  "Format HEADINGS to look pretty in a hydra docstring."
  (concat "\n "
          (mapconcat (lambda (heading)
                       (propertize (format "%-18s" heading) 'face 'shadow))
                     headings
                     nil)))

;;
;; Configuration specific to startup.
;;

(let
    (
     (default-directory user-emacs-directory) ;; FIXME: Start in the emacs directory.
     (file-name-handler-alist nil)      ;; Remove special handlers on startup.
     (gc-cons-percentage .6)
     (gc-cons-threshold most-positive-fixnum)
     (read-process-output-max (* 1024 1024))
     )

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(org-babel-tangle t))))
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Tangle and compile if necessary only, then load the configuration
  (let* ((.org "cunene.org")
         (.el (concat (file-name-sans-extension .org) ".el"))
         (modification-time
          (file-attribute-modification-time (file-attributes .org))))
    (require 'org-macs)
    (unless (org-file-newer-than-p .el modification-time)
      (require 'ob-tangle)
      (org-babel-tangle-file .org .el "emacs-lisp")
      (byte-compile-file .el))
    (load-file .el))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect)
  )

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(org-link-descriptive t nil nil "Customized with use-package org")
 '(package-selected-packages
   '(highline vertico-quick eshell-git-prompt doom-themes use-package))
 '(safe-local-variable-values
   '((org-roam-directory . "/work/DomainDrivenConsulting/masd/dogen/integration")
     (projectile-project-compilation-cmd . "FROZEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/frozen/master/projects DOGEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/dogen/integration/projects CPP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/cpp_ref_impl/master/projects CSHARP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/csharp_ref_impl/master/Src PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake /work/DomainDrivenConsulting/masd/dogen/integration/build/scripts/build.linux.sh Release 6 clang11")
     (projectile-project-test-cmd . "FROZEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/frozen/master/projects DOGEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/dogen/integration/projects CPP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/cpp_ref_impl/master/projects CSHARP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/csharp_ref_impl/master/Src PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake /work/DomainDrivenConsulting/masd/dogen/integration/build/scripts/build.linux.sh Release 6 clang11 rat")
     (projectile-project-run-cmd . "FROZEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/frozen/master/projects DOGEN_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/dogen/integration/projects CPP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/cpp_ref_impl/master/projects CSHARP_REF_IMPL_PROJECTS_DIRECTORY=/work/DomainDrivenConsulting/masd/csharp_ref_impl/master/Src PATH=/home/marco/local/cmake-3.15.3-Linux-x86_64/bin:$PATH CMAKE_TOOLCHAIN_FILE=/work/DomainDrivenConsulting/masd/vcpkg/masd/scripts/buildsystems/vcpkg.cmake /work/DomainDrivenConsulting/masd/dogen/integration/build/scripts/build.linux.sh Release 6 clang11 gao")
     (projectile-project-name . "dogen - integration")
     (projectile-project-type . "cmake")
     (flycheck-disabled-checkers emacs-lisp-checkdoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
