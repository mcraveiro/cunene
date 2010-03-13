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

;; Enable Emacs Development Environment project management features
(global-ede-mode 1)


;;
;; Projects
;;

;; Risk
(ede-cpp-root-project "Kitanda"
                      :name "Kitanda Project"
                      :file "~/code/kitanda/CMakeLists.txt"
                      :include-path '("/cpp/include" "/output/cpp/include")
                      :system-include-path '("/usr/include/gtkmm-2.4"
                                             "/usr/include/glibmm-2.4")
                      :spp-table '(("isUnix" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))

;; Quantlib
(ede-cpp-root-project "Turbo"
                      :name "Turbo Project"
                      :file "/home/marco/code/ql/trunk/QuantLib/Makefile.am"
                      :include-path '("/cpp/include")
                      :spp-table '(("isUnix" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))
