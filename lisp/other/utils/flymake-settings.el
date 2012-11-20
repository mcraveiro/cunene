;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/flymake-settings.el
;; Time-stamp: <2011-03-20 17:49:31 Sunday by taoshanwen>

;; This  file is free  software; you  can redistribute  it and/or
;; modify it under the terms of the GNU General Public License as
;; published by  the Free Software Foundation;  either version 3,
;; or (at your option) any later version.

;; This file is  distributed in the hope that  it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You  should have  received a  copy of  the GNU  General Public
;; License along with  GNU Emacs; see the file  COPYING.  If not,
;; write  to  the Free  Software  Foundation,  Inc., 51  Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defvar flymake-mode-map (make-sparse-keymap))

;; (autoload 'flymake-find-file-hook "flymake" "" t)

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-settings ()
  "Settings for `flymake'."
  (setq flymake-gui-warnings-enabled nil)

  (defvar flymake-makefile-filenames '("Makefile" "makefile" "GNUmakefile") "File names for make.")

  (defun flymake-get-make-gcc-cmdline (source base-dir)
    (let (found)
      (dolist (makefile flymake-makefile-filenames)
        (if (file-readable-p (concat base-dir "/" makefile))
            (setq found t)))
      (if found
          (list "make"
                (list "-s"
                      "-C"
                      base-dir
                      (concat "CHK_SOURCES=" source)
                      "SYNTAX_CHECK_MODE=1"
                      "check-syntax"))
        (list
         (if (string= (file-name-extension source) "c") "gcc" "/usr/local/pfh/bin/g++-4.7")
              (list "-o"
                    "/dev/null"
                    "-fsyntax-only"
                    "-Wall"
                    "-isystem /usr/include/libxml2"
                    "-isystem /usr/include/gtkmm-2.4"
                    "-isystem /usr/include/glibmm-2.4"
                    "-isystem /usr/local/pfh/include"
                    "-std=c++11"
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/dia/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/sml/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/generator/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/driver/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/all_primitives/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/class_in_a_package/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/class_without_attributes/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/class_without_package/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/stand_alone_class/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/classes_in_a_package/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/classes_inout_package/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/classes_without_package/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/compressed/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/disable_cmakelists/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/disable_full_ctor/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/disable_facet_folders/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/disable_model_package/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/enable_facet_domain/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/enable_facet_hash/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/enable_facet_io/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/enable_facet_serialization/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/two_layers_with_objects/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/config/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/dmp/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/split_project/dir/inc"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/prototype/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/trivial_inheritance/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/trivial_association/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/std_model/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/boost_model/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/comments/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/enumeration/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/exception/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/config/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/comms/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/driver/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/daemon/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/protocol/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/risk/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/creris/projects/credit_risk/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/creris/projects/wire/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/creris/projects/processor/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/creris/projects/batcher/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/creris/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/creris/projects/disruptor/include"))
                    (concat "-I" (file-truename "~/Development/kitanda/output/creris/stage/include/"))
                    source)))))

  (defun flymake-simple-make-gcc-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
    "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
    (let* ((args nil)
           (source-file-name buffer-file-name)
           (buildfile-dir (file-name-directory source-file-name)))
      (if buildfile-dir
          (let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
            (setq args
                  (flymake-get-syntax-check-program-args
                   temp-source-file-name
                   buildfile-dir
                   use-relative-base-dir
                   use-relative-source
                   get-cmdline-f))))
      args))

  (defun flymake-simple-make-gcc-init ()
    (flymake-simple-make-gcc-init-impl 'flymake-create-temp-intemp t t "Makefile" 'flymake-get-make-gcc-cmdline))

  (setq flymake-allowed-file-name-masks
        '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-gcc-init)
          ("\\.\\(?:h\\(?:pp\\|xx\\|\\+\\+\\)?\\|HH\\)\\'" flymake-simple-make-gcc-init)
          ("\\.xml\\'" flymake-xml-init)
          ("\\.html?\\'" flymake-xml-init)
          ("\\.cs\\'" flymake-simple-make-init)
          ("\\.p[ml]\\'" flymake-perl-init)
          ("\\.php[345]?\\'" flymake-php-init)
          ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
          ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
          ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
          ("\\.tex\\'" flymake-simple-tex-init)
          ("\\.idl\\'" flymake-simple-make-init)))

  (defun flymake-display-current-warning/error ()
    "Display warning/error under cursor."
    (interactive)
    (let ((ovs (overlays-in (point) (1+ (point)))))
      (dolist (ov ovs)
        (catch 'found
          (when (flymake-overlay-p ov)
            (message (overlay-get ov 'help-echo))
            (throw 'found t))))))

  (defun flymake-goto-next-error-disp ()
    "Go to next error in err ring, and then display warning/error."
    (interactive)
    (flymake-goto-next-error)
    (flymake-display-current-warning/error))

  (defun flymake-goto-prev-error-disp ()
    "Go to previous error in err ring, and then display warning/error."
    (interactive)
    (flymake-goto-prev-error)
    (flymake-display-current-warning/error))

  (defun flymake-settings-4-emaci ()
    "`flymake' settings for `emaci'."
    (emaci-add-key-definition
     "z" 'flymake-display-current-warning/error
     '(memq major-mode dev-modes)))

  (eval-after-load "emaci"
    `(flymake-settings-4-emaci)))

(eval-after-load "flymake"
  `(flymake-settings))

(define-key flymake-mode-map (kbd "C-c N")
  'flymake-goto-next-error-disp)
(define-key flymake-mode-map (kbd "C-c P")
  'flymake-goto-prev-error-disp)
(define-key flymake-mode-map (kbd "C-c M-w")
  'flymake-display-current-warning/error)

(provide 'flymake-settings)
