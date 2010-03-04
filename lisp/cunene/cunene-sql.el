;;; Marco's .emacs, copied largely from starterkit and Alex Ott's.

;; Copyright (C) 2009  Marco Craveiro
;;
;; init.el is free software; you can redistribute it and/or modify it
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
;; along with init.el.  If not, see <http://www.gnu.org/licenses/>.

;; directory for SQL data files
(setq sql-datafiles-dir (concat datafiles-dir "/sql/"))
(if (not (file-accessible-directory-p sql-datafiles-dir))
    (make-directory sql-datafiles-dir))

;;
;; Create a sensible buffer name
;;
(defun sql-make-smart-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.
   This is used to set `sql-alternate-buffer-name' within
   `sql-interactive-mode'."
  (or (and (boundp 'sql-name) sql-name)
      (concat sql-user "@"
              (if (not(string= "" sql-server))
                  (concat
                   (or (and (string-match "[0-9.]+" sql-server) sql-server)
                       (car (split-string sql-server "\\.")))
                   "/"))
              sql-database)))

;;
;; Sample database connections
;;
(setq sql-connection-alist
      '((sanzala-perlis
         (sql-product 'postgres)
         (sql-server "perlis")
         (sql-user "marco")
         (sql-password "not_needed")
         (sql-database "sanzala")
         (sql-port 5432))
        (pool-b
         (sql-product 'mysql)
         (sql-server "1.2.3.4")
         (sql-user "me")
         (sql-password "not_needed")
         (sql-database "thedb")
         (sql-port 3307))))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
           (flet ((sql-get-login (&rest what)))
             (sql-product-interactive sql-product)))))

(defun sql-sanzala-perlis ()
  (interactive)
  (sql-connect-preset 'sanzala-perlis))

;; Increase column width for SqlServer.
(setq sql-ms-options (quote ("-w" "8000" "-n")))

;; Create informative buffer names
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
            (sql-rename-buffer)))

(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat sql-datafiles-dir
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

;; truncate lines for long tables
(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      (setq truncate-lines t))))
