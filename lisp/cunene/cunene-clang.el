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

(setq ac-comphist-file  "~/.emacs.d/data/ac-comphist.dat")

(add-to-list 'load-path (concat dotfiles-dir "/other/auto-complete"))

(setq ac-quick-help-delay 0.5)
(setq ac-clang-flags
      (list
       "-isystem /usr/include/libxml2"
       "-isystem /usr/include/gtkmm-2.4"
       "-isystem /usr/include/glibmm-2.4"
       "-isystem /usr/local/pfh/include"
       "-isystem /usr/include/c++/4.7"
       "-isystem /usr/include/c++/4.7/x86_64-linux-gnu"
       "-isystem /usr/include/c++/4.7/backward"
       "-isystem /usr/lib/gcc/x86_64-linux-gnu/4.7/include"
       "-isystem /usr/local/include"
       "-isystem /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed"
       "-isystem /usr/include/x86_64-linux-gnu"
       "-isystem /usr/include"
       "-isystem /usr/include/libxml2"
       "-isystem /usr/local/pfh/include"
       "-isystem ~/Development/kitanda/kitanda/code/cpp/include"
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
       (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/test_model_sanitizer/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/entity_service_value/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dogen/projects/odb/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/config/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/comms/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/driver/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/daemon/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/protocol/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/utility/include"))
       (concat "-I" (file-truename "~/Development/kitanda/dirien/projects/risk/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/rating/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/wire/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/processor/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/batcher/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/utility/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/disruptor/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/conjure/include"))
       (concat "-I" (file-truename "~/Development/kitanda/creris/projects/conjure/src"))
       (concat "-I" (file-truename "~/Development/kitanda/output/creris/stage/include/"))
       ))

(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-clang)

(ac-set-trigger-key "TAB")
(defun my-ac-clang-mode-common-hook()
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
  )

(add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook)
