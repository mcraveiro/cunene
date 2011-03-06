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

;; Enable Emacs Development Environment project management features
(global-ede-mode 1)

;;
;; Projects
;;

;; Risk
(ede-cpp-root-project "Kitanda"
                      :name "Kitanda Project"
                      :file "~/code/kitanda/git/CMakeLists.txt"
                      :include-path '("/code/cpp/include")
                      :system-include-path
                      '("/usr/include/gtkmm-2.4"
                        "/usr/include/glibmm-2.4"
                        "~/code/kitanda/output/code/cpp/include")
                      :spp-table
                      '(("isUnix" . "")
                        ("BOOST_TEST_DYN_LINK" . "")
                        ("_MSC_VER" . "")))

(ede-cpp-root-project "CMakeGit"
                      :name "CMake Git Project"
                      :file "~/code/cmake_git/CMakeLists.txt"
                      :include-path
                      '("/Source" "/Source/ctest" "/output/Source")
                      :system-include-path '("/usr/include")
                      :spp-table
                      '(("isUnix" . "")
                        ("BOOST_TEST_DYN_LINK" . "")
                        ("_MSC_VER" . "")))

(ede-cpp-root-project "Modeling"
                      :name "Modeling Project"
                      :file "~/code/kitanda/modeling/CMakeLists.txt"
                      :include-path '("/cpp/include")
                      :system-include-path
                      '("~/code/kitanda/output/cpp/include")
                      :spp-table
                      '(("isUnix" . "")
                        ("BOOST_TEST_DYN_LINK" . "")
                        ("_MSC_VER" . "")))
