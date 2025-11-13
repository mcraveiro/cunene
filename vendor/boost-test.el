;; boost-test.el --- an interface to the Boost.Test framework.
;;
;; Copyright (C) 2012 Sven Goericke <sven.goericke * gmail com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "boost-test--<descriptive-name>".

(eval-when-compile (require 'cl))
(require 'xml)

(defgroup boost-test nil
  "Mode for boost-unittest framework"
  :group 'tools)

(defface boost-test-error-face '((t (:foreground "red" :bold t)))
  "*Face used to highlight errors or failures..."
  :group 'boost-test :group 'Help :group 'faces)

(defface boost-test-success-face '((t (:foreground "green")))
  "*Face used to highlight successful conditions..."
  :group 'boost-test :group 'Help :group 'faces)

(defface boost-test-warning-face '((t (:foreground "gold")))
  "*Face used to highlight warning conditions..."
  :group 'boost-test :group 'Help :group 'faces)

(defface boost-test-common-face '((t (:inherit 'default)))
  "*Face used to highlight non-specific stuff..."
  :group 'boost-test :group 'Help :group 'faces)

(defvar boost-test-mode-hook nil)

(defvar boost-test-program nil)

(defcustom boost-test-env-vars nil
  "List of additional environment variables which need to be set prior running the tests"
  :type '(alist :key-type string :value-type string)
  :group 'boost-test)

(defvar boost-test-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'boost-test-quit-window)
    map))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test-quit-window ()
  (interactive)
  (quit-window t))


(defun boost-test--set-env ()
  (loop for var in boost-test-env-vars do
        (setenv (car var) (cdr var))))

(defvar boost-test--command-history nil)

(defun boost-test--read-test-command (command)
  "Read which program to start"
  (read-shell-command "Run test program: " command
                      (if (equal (car boost-test--command-history) command)
                          '(boost-test--command-history . 1)
                        'boost-test--command-history)))


;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test-run ()
  "Run the test specified."
  (interactive)
  (boost-test--set-env)

  (when (equal boost-test-program nil)
    (setq boost-test-program (read-file-name "Specify test program: ")))

  (let ((test-prog (boost-test--read-test-command (eval boost-test-program)))
        (buf (get-buffer-create (generate-new-buffer-name "*test-result*"))))

    (setq default-directory (file-name-directory test-prog))

    (message "starting test(s)...")
    (let ((proc (start-process test-prog buf test-prog
                               "--output_format=XML" "--log_level=all" "--report_level=detailed")))
      (let ((sentinel (lambda (process signal)
                        (unwind-protect
                            (with-current-buffer (process-buffer process)
                              (boost-test--process-result (buffer-substring 1 (point-max))))
                          (kill-buffer (process-buffer process))))))
        (set-process-sentinel proc sentinel)))))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--process-result (result)

  (let* ((buf (get-buffer-create "boost::test results"))
         (root (with-temp-buffer
                 (insert (concat "<Root>" result "</Root>"))
                 (xml-parse-region (point-min) (point-max))))

         (result_root (car root))
         (test_log (car (xml-get-children result_root 'TestLog)))
         (test_result (car (xml-get-children result_root 'TestResult))))

    (set-buffer buf)
    (erase-buffer)
    (switch-to-buffer buf)
    ;;    (insert (format "%s\n\n" test_log))

    (boost-test--parse-log-node test_log buf)
    (insert "\n")
    (boost-test--parse-result-node test_result buf)

    (boost-test-mode))
  (message "test(s) finished..."))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--parse-log-node (node buffer)
  "Parse the TestLog part of the xml"
  (cond ((null node) nil)
        ((listp node) (let ((elem (xml-node-name node))
                            (children (xml-node-children node)))

                        (cond ((string= "TestLog" elem) nil)

                              ((string= "Exception" elem)
                               (let ((attr (xml-node-attributes node)))
                                 (when (stringp (car (last node)))
                                   (insert (propertize (format "%s: " elem)
                                                       'face 'boost-test-error-face))
                                   (insert (format "%s "(car (last node))))
                                   (insert (format "[%s:%s:]\n"
                                                   (cdr (assq 'file attr))
                                                   (cdr (assq 'line attr)))))
                                 (when (listp (car (last node)))
                                   (insert (propertize (format "%s: " elem)
                                                       'face 'boost-test-error-face))
                                   (insert (format "%s "(cdr (last node))))
                                   (insert (format "[%s:%s:]\n"
                                                   (cdr (assq 'file attr))
                                                   (cdr (assq 'line attr)))))))

                              ((string= "Error" elem)
                               (let ((attr (xml-node-attributes node)))
                                 (insert (propertize (format "%s: " elem)
                                                     'face 'boost-test-error-face))
                                 (insert (propertize (format "%s "(car (last node)))
                                                     'face 'default))
                                 (insert (propertize (format "[%s:%s]\n"
                                                             (cdr (assq 'file attr))
                                                             (cdr (assq 'line attr)))
                                                     'face 'default))))

                              ((string= "FatalError" elem)
                               (let ((attr (xml-node-attributes node)))
                                 (insert (propertize (format "%s: " elem)
                                                     'face 'boost-test-error-face))
                                 (insert (propertize (format "%s "(car (last node)))
                                                     'face 'default))
                                 (insert (propertize (format "[%s:%s]\n"
                                                             (cdr (assq 'file attr))
                                                             (cdr (assq 'line attr)))
                                                     'face 'default))))

                              ((string= "Info" elem) nil)
                               ;; (let ((attr (xml-node-attributes node)))
                               ;;   (insert (propertize (format "%s: " elem)
                               ;;                       'face 'font-lock-type-face))
                               ;;   (insert (propertize (format "%s "(car (last node)))
                               ;;                       'face 'default))
                               ;;   (insert (propertize (format "[%s:%s]\n"
                               ;;                               (cdr (assq 'file attr))
                               ;;                               (cdr (assq 'line attr)))
                               ;;                       'face 'default))))


                              ((string= "LastCheckpoint" elem)
                               (let ((attr (xml-node-attributes node)))
                                 (insert (propertize (format "%s: " elem)
                                                     'face 'font-lock-type-face))
                                 (insert (format "%s "(car (last node))))
                                 (insert (format "[%s:%s]\n"
                                                 (cdr (assq 'file attr))
                                                 (cdr (assq 'line attr))))))

                              ((string= "Message" elem) nil)
                               ;; (let ((attr (xml-node-attributes node)))
                               ;;   (insert (propertize (format "\n%s: " elem)
                               ;;                       'face 'font-lock-type-face))
                               ;;   (insert (propertize (format "%s\n" (cdr (assq 'name attr)))
                               ;;                       'face 'default))))

                              ((string= "TestCase" elem)
                               (let ((attr (xml-node-attributes node)))
                                 (insert (propertize (format "\n%s: " elem)
                                                     'face 'font-lock-type-face))
                                 (insert (propertize (format "%s\n" (cdr (assq 'name attr)))
                                                     'face 'default))))

                              ((string= "Warning" elem)
                               (let ((attr (xml-node-attributes node)))
                                 (insert (propertize (format "%s: " elem)
                                                     'face 'boost-test-warning-face))
                                 (insert (propertize (format "%s "(car (last node)))
                                                     'face 'default))
                                 (insert (propertize (format "[%s:%s]\n"
                                                             (cdr (assq 'file attr))
                                                             (cdr (assq 'line attr)))
                                                     'face 'default))))


                              (t
                               (cond ((stringp (car (last node)))
                                      (insert (propertize (format "%s: " elem)
                                                          'face 'font-lock-type-face))
                                      (insert (propertize (format "%s\n" (car (last node)))
                                                          'face 'default)))
                                     (t
                                      (let ((attr (xml-node-attributes node)))
                                        (insert (propertize (format "%s: " elem)
                                                            'face 'font-lock-type-face))
                                        (insert (propertize (format "%s\n" (cdr (assq 'name attr)))
                                                            'face 'default)))))))

                        (mapcar (lambda (x) (boost-test--parse-log-node x buffer)) children)))))


;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--parse-result-node (node buffer)
  "Parse the TestResult part of the xml"

  (when (listp node)
    (setq fill-column 60)
    (let ((root (car (xml-get-children node 'TestSuite)))
          (attr (xml-node-attributes (car (xml-get-children node 'TestSuite))))
          (attr-count 0))

      (insert (propertize
               (format "%s %S result summary:" (car root) (cdr (assq 'name attr)))
               'face 'bold))
      (center-line)
      (insert (format "\n|%s|\n" (make-string 60 ?-)))

      (while (< attr-count (length (xml-node-attributes root)))
        (let ((identifier (car (elt attr attr-count)))
              (value (cdr (elt attr attr-count))))
          (cond ((string= "name" identifier) nil)

                ((string= "result" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (string= "failed" value)
                                     (propertize (format "%s" (upcase value))
                                                 'face 'boost-test-error-face)
                                   (propertize (format "%s" (upcase value))
                                               'face 'boost-test-success-face)))))

                ((string= "assertions_passed" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (> (string-to-number value) 0)
                                     (propertize (format "%s" value)
                                                 'face 'boost-test-success-face)
                                   (format "%s" value)))))

                ((string= "assertions_failed" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (> (string-to-number value) 0)
                                     (propertize (format "%s" value)
                                                 'face 'boost-test-error-face)
                                   (format "%s" value)))))

                ((string= "expected_failures" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (> (string-to-number value) 0)
                                     (propertize (format "%s" value)
                                                 'face 'boost-test-warning-face)
                                   (format "%s" value)))))

                ((string= "test_cases_passed" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (> (string-to-number value) 0)
                                     (propertize (format "%s" value)
                                                 'face 'boost-test-success-face)
                                   (format "%s" value)))))

                ((string= "test_cases_failed" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (> (string-to-number value) 0)
                                     (propertize (format "%s" value)
                                                 'face 'boost-test-error-face)
                                   (format "%s" value)))))

                ((string= "test_cases_aborted" identifier)
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 (if (> (string-to-number value) 0)
                                     (propertize (format "%s" value)
                                                 'face 'boost-test-error-face)
                                   (format "%s" value)))))

                (t
                 (insert (format "| %-20s | %-35s |\n"
                                 (propertize (format "%s:" identifier)
                                             'face 'font-lock-type-face)
                                 value))))
          (setq attr-count (1+ attr-count))))

      (insert (format "|%s|\n" (make-string 60 ?-))))))

;; remove CR (^M) from display ----------------------------------------------------------------------------------------
(defun boost-test-mode--remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test-mode nil
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'boost-test-mode)
  (setq mode-name "Boost.Test")
  (use-local-map boost-test-mode-map)
  (boost-test-mode--remove-dos-eol)
  (run-mode-hooks 'boost-test-mode-hook))


(provide 'boost-test)
