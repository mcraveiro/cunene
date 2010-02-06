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

;; Risk project
(ede-cpp-root-project "Risk"
                      :name "Risk Project"
                      :file "~/code/kitanda/risk/CMakeLists.txt"
                      :include-path '("/cpp/include")
                      :system-include-path '("~/code/kitanda/turbo/cpp/include")
                      :spp-table '(("isUnix" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))

;; Turbo project
(ede-cpp-root-project "Turbo"
                      :name "Turbo Project"
                      :file "~/code/kitanda/turbo/CMakeLists.txt"
                      :include-path '("/cpp/include")
                      :spp-table '(("isUnix" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))

;; Trading project
(ede-cpp-root-project "Trading"
                      :name "Trading Project"
                      :file "~/code/kitanda/trading/CMakeLists.txt"
                      :include-path '("/cpp/include")
                      :system-include-path '("~/code/kitanda/turbo/include")
                      :spp-table '(("isUnix" . "")
                                   ("BOOST_TEST_DYN_LINK" . "")))
