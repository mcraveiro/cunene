;;; .dir-locals.el --- My Emacs configuration -*- lexical-binding: t; -*-
((nil . ((eval . (setq-local
                  org-roam-directory (expand-file-name (locate-dominating-file
                                                        default-directory ".dir-locals.el"))))
         (eval . (setq-local
                  org-roam-db-location (expand-file-name ".org-roam.db"
                                                         org-roam-directory))))))
