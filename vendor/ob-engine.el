;;; ob-engine.el --- Execute search engine queries within org-mode blocks.
;; Copyright 2024 Marco Craveiro

;; License: GNU General Public License version 3, or (at your option) any later version
;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Maintainer: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: org babel engine-mode
;; URL: https://github.com/mcraveiro/cunene
;; Created: 5th Marco 2024
;; Version: 0.0.1
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; Execute search engine query on a browser.

;;; Code:
(require 'org)
(require 'ob)
(require 'engine-mode)

;;;###autoload
(defun org-babel-execute:engine (body params)
  "Org-babel engine hook."
  ;; (engine/search-google body)
  (let*
      ((engine-name
        (intern (cdr (assoc :engine params))))
       (engine-function-name
        (engine--function-name engine-name)))
    (apply engine-function-name (list body))))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("engine" . fundamental)))

(provide 'ob-engine)

;;; ob-engine.el ends here
