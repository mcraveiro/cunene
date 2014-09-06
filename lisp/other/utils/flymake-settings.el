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
         (if (string= (file-name-extension source) "c") "gcc" "g++")
              (list "-o"
                    "/dev/null"
                    "-fsyntax-only"
                    "-Wall"
                    "-isystem/usr/include/i386-linux-gnu"
                    "-isystem/usr/include/libxml2"
                    "-isystem/usr/include/sigc++-2.0"
                    "-isystem/usr/lib/x86_64-linux-gnu/sigc++-2.0/include"
                    "-isystem/usr/include/gtkmm-2.4"
                    "-isystem/usr/include/glibmm-2.4"
                    "-isystem/usr/local/pfh/include"
                    "-isystem/usr/local/personal/include"
                    "-isystem/usr/include/libxml2"
                    "-isystem/usr/include/sigc++-2.0"
                    "-isystem/usr/lib/x86_64-linux-gnu/sigc++-2.0/include"
                    "-isystem/usr/include/gtkmm-2.4"
                    "-isystem/usr/include/glibmm-2.4"
                    "-isystem/usr/include/glib-2.0"
                    "-isystem/usr/lib/x86_64-linux-gnu/glib-2.0/include"
                    "-isystem/usr/lib/x86_64-linux-gnu/glibmm-2.4/include"
                    "-isystem/usr/lib/gtkmm-2.4/include/"
                    "-isystem/usr/lib/gdkmm-2.4/include/"
                    "-isystem/usr/lib/pangomm-1.4/include"
                    "-isystem/usr/include/gdkmm-2.4"
                    "-isystem/usr/include/gtk-3.0"
                    "-isystem/usr/include/pango-1.0/"
                    "-isystem/usr/include/cairo/"
                    "-isystem/usr/include/gdk-pixbuf-2.0"
                    "-isystem/usr/include/cairomm-1.0/"
                    "-isystem/usr/lib/cairomm-1.0/"
                    "-isystem/usr/include/libxml2"
                    "-isystem/usr/include/gtkmm-2.4"
                    "-isystem/usr/include/glibmm-2.4"
                    "-std=c++11"
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/dia/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/sml/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/config/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/cpp/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/cpp_formatters/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/sml_to_cpp/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/dia_to_sml/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/formatters/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/knit/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/knitter/include"))

                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/all_primitives/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/class_in_a_package/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/class_without_attributes/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/class_without_package/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/stand_alone_class/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/classes_in_a_package/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/classes_inout_package/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/classes_without_package/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/compressed/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/disable_cmakelists/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/disable_full_ctor/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/disable_facet_folders/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/disable_model_package/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/enable_facet_domain/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/enable_facet_hash/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/enable_facet_io/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/enable_facet_serialization/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/two_layers_with_objects/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/dmp/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/split_project/dir/inc"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/trivial_inheritance/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/trivial_association/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/std_model/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/boost_model/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/comments/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/enumeration/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/exception/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/stereotypes/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/eos_serialization/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/test_model_sanitizer/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dogen/projects/test_models/database/include"))

                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/config/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/comms/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/driver/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/daemon/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/protocol/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/risk/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/dirien/projects/risk/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/rating/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/wire/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/processor/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/batcher/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/utility/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/disruptor/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/conjure/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/creris/projects/conjure/src"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/output/creris/stage/include/"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/hedgr/projects/controller/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/hedgr/projects/gtk/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/hedgr/projects/presentation/include"))
                    (concat "-I" (file-truename "~/Development/DomainDrivenConsulting/hedgr/projects/utility/include"))
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
