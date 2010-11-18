;; t4-mode.el --- T4 mode highlighting. Ripped off jinja-mode by Georg Brandl
;;
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
;;
(require 'mumamo)

(defconst t4-font-lock-keywords
  (list
   ;;'("template\\|language\\|output\\|extension" . font-lock-preprocessor-face)
   '("\"\\([^\"]\\)\"[^\"]+" (1 font-lock-string-face t t))
   (cons (rx word-start
             (or "template" "import" "assembly" "output" "parameter" "include")
             word-end)
         font-lock-keyword-face)
   (cons (rx word-start
             (or "language" "extension" "name" "debug" "namespace")
             word-end)
         font-lock-builtin-face)
   ;; '("{# ?\\(.*?\\) ?#}" . (1 font-lock-comment-face))
   ;; '("{%-?\\|-?%}\\|{{\\|}}" . font-lock-preprocessor-face)
   ;; '("{#\\|#}" . font-lock-comment-delimiter-face)
   ;; first word in a block is a command
   ;; '("{%-?[ \t\n]*\\([a-zA-Z_]+\\)" . (1 font-lock-keyword-face))
   ;; variables
   ;; '("\\({{ ?\\)\\([^|]*?\\)\\(|.*?\\)? ?}}" . (1 font-lock-variable-name-face))
   ;; keywords and builtins
   ;; (cons (rx word-start
   ;;           (or "true" "false" "none" "loop" "self" "super")
   ;;           word-end)
   ;;       font-lock-builtin-face)
   ;; tests
   ;; '("\\(is\\)[ \t]*\\(not\\)[ \t]*\\([a-zA-Z_]+\\)"
   ;;   (1 font-lock-keyword-face) (2 font-lock-keyword-face)
   ;;   (3 font-lock-function-name-face))
   ;; builtin filters
   ;; (cons (rx
   ;;        "|" (* space)
   ;;        (submatch
   ;;         (or "abs" "batch" "capitalize" "capture" "center" "count" "default"
   ;;             "dformat" "dictsort" "e" "escape" "filesizeformat" "first"
   ;;             "float" "format" "getattribute" "getitem" "groupby" "indent"
   ;;             "int" "join" "jsonencode" "last" "length" "lower" "markdown"
   ;;             "pprint" "random" "replace" "reverse" "round" "rst" "slice"
   ;;             "sort" "string" "striptags" "sum" "textile" "title" "trim"
   ;;             "truncate" "upper" "urlencode" "urlize" "wordcount" "wordwrap"
   ;;             "xmlattr")))
   ;;       (list 1 font-lock-builtin-face))
   )
   "Minimal highlighting expressions for T4 mode")

(define-derived-mode t4-mode nil "T4 Mode"
  "Simple T4 mode for use with `mumamo-mode'.
This mode only provides syntax highlighting."
  (setq font-lock-defaults '(t4-font-lock-keywords)))

;; mumamo stuff

(defun mumamo-chunk-t4-directive (pos min max)
  "Find directives: <#@ ... #>.  Return range and `t4-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-t4-directive
                              'mumamo-search-bw-exc-end-t4-directive
                              'mumamo-search-fw-exc-start-t4-directive
                              'mumamo-search-fw-exc-end-t4-directive
                              ))

(defun mumamo-chunk-t4-standard-control-block (pos min max)
  "Find standard control blocks: <# ... #>.  Return range and `t4-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-t4-standard-control-block
                              'mumamo-search-bw-exc-end-t4-standard-control-block
                              'mumamo-search-fw-exc-start-t4-standard-control-block
                              'mumamo-search-fw-exc-end-t4-standard-control-block
                              ))

(defun mumamo-chunk-t4-expression-block (pos min max)
  "Find expression blocks: <#= ... #>.  Return range and `t4-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-t4-expression-block
                              'mumamo-search-bw-exc-end-t4-expression-block
                              'mumamo-search-fw-exc-start-t4-expression-block
                              'mumamo-search-fw-exc-end-t4-expression-block
                              ))

(defun mumamo-chunk-t4-class-block (pos min max)
  "Find class blocks: <#+ ... #>.  Return range and `t4-mode'.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-t4-class-block
                              'mumamo-search-bw-exc-end-t4-class-block
                              'mumamo-search-fw-exc-start-t4-class-block
                              'mumamo-search-fw-exc-end-t4-class-block
                              ))

(defun mumamo-search-bw-exc-start-t4-directive (pos min)
  "Helper for `mumamo-chunk-t4-directive'.
POS is where to start search and MIN is where to stop."
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "<#@")))
    (and exc-start
       (<= exc-start pos)
       (cons exc-start 't4-mode)))
  )

(defun mumamo-search-bw-exc-start-t4-standard-control-block (pos min)
  "Helper for `mumamo-chunk-t4-standard-control-block'.
POS is where to start search and MIN is where to stop."
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "<# ")))
    (and exc-start
       (<= exc-start pos)
       (cons exc-start 't4-mode)))
  )

(defun mumamo-search-bw-exc-start-t4-expression-block (pos min)
  "Helper for `mumamo-chunk-t4-expression-block'.
POS is where to start search and MIN is where to stop."
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "<#=")))
    (and exc-start
       (<= exc-start pos)
       (cons exc-start 't4-mode)))
  )

(defun mumamo-search-bw-exc-start-t4-class-block (pos min)
  "Helper for `mumamo-chunk-t4-class-block'.
POS is where to start search and MIN is where to stop."
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "<#+")))
    (and exc-start
       (<= exc-start pos)
       (cons exc-start 't4-mode)))
  )

(defun mumamo-search-bw-exc-end-t4-directive (pos min)
  "Helper for `mumamo-chunk-t4-directive'.
POS is where to start search and MIN is where to stop."
  (mumamo-chunk-end-bw-str-inc pos min "#>"))

(defun mumamo-search-bw-exc-end-t4-standard-control-block (pos min)
  "Helper for `mumamo-chunk-t4-standard-control-block'.
POS is where to start search and MIN is where to stop."
  (mumamo-chunk-end-bw-str-inc pos min "#>"))

(defun mumamo-search-bw-exc-end-t4-expression-block (pos min)
  "Helper for `mumamo-chunk-t4-expression-block'.
POS is where to start search and MIN is where to stop."
  (mumamo-chunk-end-bw-str-inc pos min "#>"))

(defun mumamo-search-bw-exc-end-t4-class-block (pos min)
  "Helper for `mumamo-chunk-t4-class-block'.
POS is where to start search and MIN is where to stop."
  (mumamo-chunk-end-bw-str-inc pos min "#>"))

(defun mumamo-search-fw-exc-start-t4-directive (pos max)
  "Helper for `mumamo-chunk-t4-directive'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "<#@"))

(defun mumamo-search-fw-exc-start-t4-standard-control-block (pos max)
  "Helper for `mumamo-chunk-t4-standard-control-block'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "<# "))

(defun mumamo-search-fw-exc-start-t4-expression-block(pos max)
  "Helper for `mumamo-chunk-t4-expression-block'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "<#="))

(defun mumamo-search-fw-exc-start-t4-class-block(pos max)
  "Helper for `mumamo-chunk-t4-class-block'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "<#+"))

(defun mumamo-search-fw-exc-end-t4-directive(pos max)
  "Helper for `mumamo-chunk-t4-directive'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#>"))

(defun mumamo-search-fw-exc-end-t4-standard-control-block(pos max)
  "Helper for `mumamo-chunk-t4-standard-control-block'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#>"))

(defun mumamo-search-fw-exc-end-t4-expression-block(pos max)
  "Helper for `mumamo-chunk-t4-expression-block'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#>"))

(defun mumamo-search-fw-exc-end-t4-class-block(pos max)
  "Helper for `mumamo-chunk-t4-expression-class'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#>"))

;;;###autoload
(define-mumamo-multi-major-mode t4-c++-mumamo
  "Turn on multiple major modes for T4 with main mode `c++-mode'."
  ("T4 C++ Family" c++-mode
   (mumamo-chunk-t4-directive
    mumamo-chunk-t4-class-block
    mumamo-chunk-t4-standard-control-block
    mumamo-chunk-t4-expression-block
    ;; mumamo-chunk-inlined-style
    ;; mumamo-chunk-inlined-script
    ;; mumamo-chunk-style=
    ;; mumamo-chunk-onjs=
    )))
;; )

;; (custom-set-faces
;;  '(mumamo-background-chunk-major ((t (:background "#FFFFFF"))))
;;  '(mumamo-background-chunk-submode1 ((t (:background "#FFFFFF"))))
;;  '(mumamo-background-chunk-submode2 ((t (:background "#FFFFFF"))))
;;  '(mumamo-background-chunk-submode3 ((t (:background "#FFFFFF"))))
;;  '(mumamo-background-chunk-submode4 ((t (:background "#FFFFFF"))))
;;  )

(provide 't4-mode)
