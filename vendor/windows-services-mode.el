;;; windows-services-mode --- Manage windows services.

;; Copyright (C) 2024 Marco Craveiro.

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Version: 0.0.1
;; Created: 2024
;; Keywords: windows services

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;; Code:
(require 'rx)

(defconst windows-services/key-value-regex
  (rx bol
      (zero-or-more blank)
      (one-or-more alphanumeric)
      "_"
      (one-or-more alphanumeric)
      (zero-or-more blank)
      ":"
      (zero-or-more any)))

(defvar windows-services/sc-command "~/.emacs.d/vendor/sc.exe"
  "Binary for the SC command. Expected to be in the path.")

(defvar windows-services/sc-args "queryex type= all state= all"
  "Command line arguments for the SC command.")

(defun windows-services ()
  "Lists windows services on a box."
  (interactive)
  (pop-to-buffer "*Windows Services*" nil)
  (windows-services/windows-services-mode)
  (setq tabulated-list-entries (list
                                (list "1" ["1" "2" "3" "4"])
                                (list "2" ["a" "b" "c" "d"])))
  (tabulated-list-print t))

(defun windows-services/split-into-records (input)
  "Process an input string with records separated by new lines.
INPUT contains the output of sc command."
  (split-string input "\n\n" t))

(defun windows-services/parse-sc-record (output)
  "Parse the output of sc.exe command and put it into separate variables.
OUTPUT is the result of running the sc command."
  (let ((lines (split-string output "\n" t)) ; Split the output into lines
        service-name
        type
        start-type
        error-control
        binary-path
        load-order-group
        tag
        display-name
        dependencies
        service-start-name)
    (dolist (line lines)
      (when (string-match windows-services/key-value-regex line)
        (let* ((kvp-split (split-string line ":" t))
              (key (cl-first kvp-split))
              (value (cl-second kvp-split)))
          (setq key (downcase key))
          (setq value (string-trim value))
          (pcase key
            ("service_name" (setq service-name value))
            ("type" (setq type value))
            ("start_type" (setq start-type value))
            ("error_control" (setq error-control value))
            ("binary_path_name" (setq binary-path value))
            ("load_order_group" (setq load-order-group value))
            ("tag" (setq tag value))
            ("display_name" (setq display-name value))
            ("dependencies" (setq dependencies value))
            ("service_start_name" (setq service-start-name value))))))
    (list :service-name service-name
          :service-type type
          :start-type start-type
          :error-control error-control
          :binary-path binary-path
          :load-order-group load-order-group
          :tag tag
          :display-name display-name
          :dependencies dependencies
          :service-start-name service-start-name)))

(defun windows-services/run-sc ()
  "Execute the SC command."
  (let (
         (output-buffer (get-buffer-create "*Windows Services - sc.exe*"))
         (error-buffer (get-buffer-create "*Windows Services - sc.exe errors*"))
         (command
          (concat windows-services/sc-command " " windows-services/sc-args))
         )
    (with-current-buffer output-buffer
      (shell-command command output-buffer error-buffer)
      (buffer-string))))

(define-derived-mode windows-services/windows-services-mode
  tabulated-list-mode
  "windows-services-mode"
  "Major mode to manage windows services."
  (setq tabulated-list-format [("Col1" 18 t)
                               ("Col2" 12 nil)
                               ("Col3" 10 t)
                               ("Col4" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Col3" nil))
  (tabulated-list-init-header))

(defun windows-services-mode/print-current-line-id ()
  "Test."
  (interactive)
  (message (concat "current line ID is: " (tabulated-list-get-id))))


(provide 'windows-services-mode)
;;; windows-services-mode.el ends here
