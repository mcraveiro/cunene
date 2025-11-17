;;; catch2-test-runner.el --- Catch2 runner using --list-tests + JUnit -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: c++, testing, catch2

;;; Commentary:
;; Catch2 runner with a text-based, ZTree-style, color-coded display.

;;; Code:

(require 'xml)
(require 'subr-x)
;; (require 'text-props) ; Removed: These functions are autoloaded in modern Emacs

;; ----------------------------------------------------------------------
;; Customisation
;; ----------------------------------------------------------------------
(defgroup catch2-test-runner nil
  "Catch2 test runner using --list-tests + JUnit."
  :group 'tools)

(defcustom catch2-test-runner-binary nil
  "Path to the Catch2 test binary."
  :type 'file
  :group 'catch2-test-runner)

(defcustom catch2-test-runner-junit-file (make-temp-file "catch2-junit" nil ".xml")
  "Temporary file for JUnit XML output."
  :type 'file
  :group 'catch2-test-runner)

;; ----------------------------------------------------------------------
;; Internal state
;; ----------------------------------------------------------------------
(defvar catch2-test-runner--process nil)
(defvar catch2-test-runner--test-tree nil)
(defvar catch2-test-runner--output-buffer "*Catch2 Output*")
(defvar catch2-test-runner--tree-buffer "*Catch2 Tests*")
(defvar catch2-test-runner--raw-buffer "*Catch2 Raw*")
(defvar catch2-test-runner-mode-map nil
  "Keymap for `catch2-test-runner-mode'.")
(defvar catch2-test-runner--suite-name nil)

;; ----------------------------------------------------------------------
;; Faces (Colors)
;; ----------------------------------------------------------------------
(defface catch2-test-pass '((t (:foreground "DarkGreen" :bold t)))
  "Face for passing Catch2 tests."
  :group 'catch2-test-runner)

(defface catch2-test-fail '((t (:foreground "Red" :bold t)))
  "Face for failing Catch2 tests."
  :group 'catch2-test-runner)

(defface catch2-test-pending '((t (:foreground "Yellow" :bold t)))
  "Face for Catch2 tests not yet run or currently running."
  :group 'catch2-test-runner)

;; ----------------------------------------------------------------------
;; Major mode
;; ----------------------------------------------------------------------
(defun catch2-test-runner--define-keymap ()
  "Define and set the keymap for catch2-test-runner-mode."
  (unless catch2-test-runner-mode-map
    (setq catch2-test-runner-mode-map (make-sparse-keymap))
    (define-key catch2-test-runner-mode-map "r" 'catch2-test-runner-run-at-point)))

(define-derived-mode catch2-test-runner-mode special-mode "Catch2"
  "Major mode for Catch2 test tree."
  (catch2-test-runner--define-keymap)
  (add-hook 'kill-buffer-hook #'catch2-test-runner--cleanup nil t))

;; ----------------------------------------------------------------------
;; Entry point
;; ----------------------------------------------------------------------
;;;###autoload
(defun catch2-test-runner-run ()
  "Start the Catch2 test runner."
  (interactive)
  (unless catch2-test-runner-binary
    (setq catch2-test-runner-binary
          (read-file-name "Catch2 binary: " nil nil t)))
  (setq catch2-test-runner--suite-name (file-name-base catch2-test-runner-binary))
  (dolist (b (list catch2-test-runner--tree-buffer
                   catch2-test-runner--raw-buffer
                   catch2-test-runner--output-buffer))
    (when (get-buffer b) (kill-buffer b)))
  (catch2-test-runner--discover-tests))

;; ----------------------------------------------------------------------
;; Discovery: --list-tests (plain indented text)
;; ----------------------------------------------------------------------
(defun catch2-test-runner--discover-tests ()
  "Run --list-tests and build the hierarchy."
  (let ((buf (get-buffer-create (format "*%s*" catch2-test-runner--suite-name))))
    (setq catch2-test-runner--tree-buffer (buffer-name buf))
    (with-current-buffer buf
      (catch2-test-runner-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Discovering tests for %s…\n" catch2-test-runner--suite-name))))
    (switch-to-buffer buf))

  (let ((cmd `(,catch2-test-runner-binary "--list-tests" "--colour-mode" "none")))
    (message "DISCOVERY CMD: %s" (mapconcat #'identity cmd " "))
    (catch2-test-runner--run-command
      cmd
      #'catch2-test-runner--parse-list-tests
      "Discovering…")))

(defun catch2-test-runner--parse-list-tests (raw)
  "Parse the plain-text output of --list-tests into a proper tree structure."
  (catch2-test-runner--show-raw "DISCOVERY" raw)
  (let ((lines (split-string raw "\n" t))
        test-cases tags)

    ;; First pass: collect test cases and their tags
    (let (current-test)
      (dolist (line lines)
        (let ((trimmed (string-trim line)))
          (cond
            ;; Capture test case names
            ((string-match "^\\([a-zA-Z0-9_]+\\)$" trimmed)
             (setq current-test trimmed)
             (push (list :name current-test :tags '()) test-cases))
            ;; Capture tags
            ((string-match "^\\[\\([a-zA-Z0-9_]+\\)\\]$" trimmed)
             (let ((tag (match-string 1 trimmed)))
               (when current-test
                 (setf (plist-get (car test-cases) :tags) (cons tag (plist-get (car test-cases) :tags))))))
            ;; Skip header line
            ((string-match "^All available test cases:" trimmed)))))
    (setq test-cases (nreverse test-cases))) ;; Reverse to maintain original order

    ;; Build the tree structure: suite -> tags -> test cases
    (setq catch2-test-runner--test-tree
          (list :name catch2-test-runner--suite-name
                :children
                (catch2-test-runner--build-tag-tree test-cases)))

    (catch2-test-runner--debug-tree catch2-test-runner--test-tree)
    (catch2-test-runner--render-test-tree nil)))

(defun catch2-test-runner--build-tag-tree (test-cases)
  "Build tree structure grouping test cases by tags."
  (let (tag-nodes)
    (dolist (test-case test-cases)
      (let* ((test-name (plist-get test-case :name))
             (tags (plist-get test-case :tags))
             (primary-tag (if tags (car tags) "untagged")))
        ;; Use a list with name/children structure for tests
        (let ((test-node (list :name test-name :children '())))
          (let ((tag-node (assoc primary-tag tag-nodes)))
            (if tag-node
                (setcdr tag-node (cons test-node (cdr tag-node)))
              ;; Fixed: Removed extra level of nesting
              (push (list primary-tag test-node) tag-nodes))))))

    ;; Convert alist (primary-tag . list-of-tests) to tree structure
    (mapcar (lambda (tag-pair)
              (list :name (car tag-pair)
                    :children (nreverse (cdr tag-pair))))
            (nreverse tag-nodes))))

;; ----------------------------------------------------------------------
;; Debug function to inspect parsed tree
;; ----------------------------------------------------------------------
(defun catch2-test-runner--debug-tree (tree &optional level)
  "Debug function to print the parsed tree structure."
  (setq level (or level 0))
  (dolist (node tree)
    (message "%s%s (children: %d)"
             (make-string (* level 2) ? )
             (plist-get node :name)
             (length (plist-get node :children)))
    (catch2-test-runner--debug-tree (plist-get node :children) (1+ level))))

;; ----------------------------------------------------------------------
;; Text-Based Tree Rendering (ZTree/Manual approach)
;; ----------------------------------------------------------------------
(defun catch2-test-runner--render-test-tree (results)
  "Render the test tree in text, using faces for status, and text properties for execution."
  (let ((buf (get-buffer-create catch2-test-runner--tree-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s Test Suite\n\n" catch2-test-runner--suite-name))
        ;; Fixed: Pass a list containing the tree node, not the node itself
        (catch2-test-runner--insert-tree (list catch2-test-runner--test-tree) results "" 0)
        (goto-char (point-min))
        (catch2-test-runner-mode)
        (set-buffer-modified-p nil)))
    (switch-to-buffer buf)))

(defun catch2-test-runner--get-status-face (status)
  "Return the appropriate face based on the test status."
  (cond
    ((eq status 'passed) 'catch2-test-pass)
    ((eq status 'failed) 'catch2-test-fail)
    (t 'catch2-test-pending)))

(defun catch2-test-runner--insert-tree (tree results parent-path level)
  "Recursively insert tree nodes with text formatting and test filters."
  (dolist (node tree)
    (let* ((name (plist-get node :name))
           (children (plist-get node :children))
           (full-path (if (string-empty-p parent-path)
                          name
                        (concat parent-path " " name)))
           (status (cdr (assoc full-path results)))
           (face (catch2-test-runner--get-status-face status))
           (indent (make-string (* level 2) ?\s))
           (status-symbol (cond
                           ((eq status 'passed) "✔")
                           ((eq status 'failed) "✖")
                           (t " ")))
           (text-start (point)))

      ;; 1. Insert indentation and status frame
      (let ((status-symbol-start (point)))
        (insert (format "%s[ %s ] " indent status-symbol))
        (put-text-property status-symbol-start (point) 'status-symbol-start t))

      ;; 2. Insert node name and apply face (the color)
      (let ((name-start (point)))
        (insert name)
        ;; Fixed: Use a proper property list for the face
        (add-text-properties name-start (point) (list 'face face)))

      ;; 3. Insert newline
      (insert "\n")

      ;; 4. Apply properties to the whole line for interactivity
      (let ((line-end (line-end-position)))
        (put-text-property text-start line-end 'catch2-filter full-path) ; Filter to run
        (put-text-property text-start line-end 'keymap catch2-test-runner-mode-map) ; Ensure keymap works on the line
        (put-text-property text-start line-end 'field t) ; Mark as an interactive field
        (put-text-property text-start line-end 'level level)) ; Store the level for later use

      ;; Recurse for children
      (catch2-test-runner--insert-tree children results full-path (1+ level)))))

;; ----------------------------------------------------------------------
;; Run a test/section and Interactivity
;; ----------------------------------------------------------------------
(defun catch2-test-runner-run-at-point ()
  "Run the Catch2 tests corresponding to the node at point, bound to 'r'."
  (interactive)
  (let ((filter (get-text-property (point) 'catch2-filter)))
    (if filter
        (catch2-test-runner--run-test filter)
      (message "No test filter found at point. Move to a test or tag."))))

(defun catch2-test-runner--run-test (filter)
  "Run FILTER (section or full test name) with JUnit reporter."
  ;; First, mark the filter as pending (yellow) in the tree
  (catch2-test-runner--mark-as-running filter)

  (let ((cmd `(,catch2-test-runner-binary
                ,filter
                "--reporter" "junit"
                "--out" ,catch2-test-runner-junit-file)))
    (message "RUN CMD: %s" (mapconcat #'identity cmd " "))
    (catch2-test-runner--run-command
      cmd
      #'catch2-test-runner--parse-junit-results
      (format "Running %s…" filter))))

(defun catch2-test-runner--mark-as-running (filter)
  "Update the test tree display to show the current filter being run is yellow."
  (with-current-buffer catch2-test-runner--tree-buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((current-filter (get-text-property (point) 'catch2-filter)))
            (when (equal current-filter filter)
              ;; Find the status symbol position
              (let* ((bol (line-beginning-position))
                     (eol (line-end-position))
                     (status-symbol-pos (next-single-property-change bol 'status-symbol-start nil eol)))
                (when status-symbol-pos
                  ;; Update the status symbol
                  (let ((status-symbol-end (next-single-property-change status-symbol-pos 'status-symbol-start nil eol)))
                    (delete-region status-symbol-pos status-symbol-end)
                    (goto-char status-symbol-pos)
                    (insert "⏳"))
                  ;; Update the face for the entire line
                  (add-text-properties bol eol '(face catch2-test-pending)))))
            (forward-line)))
        (redisplay)))))

(defun catch2-test-runner--parse-junit-results (_)
  "Read the JUnit XML file and refresh the tree."
  (let (results)
    (when (file-exists-p catch2-test-runner-junit-file)
      (with-temp-buffer
        (insert-file-contents catch2-test-runner-junit-file)
        (condition-case err
            (let ((xml (xml-parse-region (point-min) (point-max))))
              (dolist (suite (xml-get-children (car xml) 'testsuite))
                (dolist (tc (xml-get-children suite 'testcase))
                  (let* ((class (xml-get-attribute-or-nil tc 'classname))
                         (name  (xml-get-attribute tc 'name))
                         (full  (if class (concat class " " name) name))
                         (fail  (xml-get-children tc 'failure))
                         (status (if fail 'failed 'passed)))
                    (push (cons full status) results)))))
          (error
           (message "JUnit parse error: %s" (error-message-string err))))))
    (catch2-test-runner--render-test-tree results)
    (delete-file catch2-test-runner-junit-file)))

;; ----------------------------------------------------------------------
;; Process handling + debug output
;; ----------------------------------------------------------------------
(defun catch2-test-runner--run-command (cmd callback &optional status-msg)
  "Run CMD asynchronously, call CALLBACK with raw stdout. Output goes to *Catch2 Output*."
  (when catch2-test-runner--process
    (delete-process catch2-test-runner--process))

  (let* ((output-buf (get-buffer-create catch2-test-runner--output-buffer))
         (proc (apply #'start-process "catch2" output-buf cmd)) ; Output to this buffer
         (raw-buf (get-buffer-create "*Catch2 Process Raw*"))) ; Temp buffer for callback raw data

    (with-current-buffer output-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local major-mode 'compilation-mode)
        (insert (format "Running command: %s\n" (mapconcat #'identity cmd " ")))
        (insert "--------------------------------\n"))
      ;; Corrected: Use a standard display action
      (display-buffer output-buf '(display-buffer-in-side-window)))

    (setq catch2-test-runner--process proc)

    ;; Filter: write directly to the output buffer AND copy to a raw buffer for sentinel
    (set-process-filter proc
                        (lambda (_ out)
                          (with-current-buffer output-buf
                            (goto-char (point-max))
                            (insert out))
                          (with-current-buffer raw-buf
                            (goto-char (point-max))
                            (insert out))))

    (set-process-sentinel proc
                          (lambda (p _)
                            (when (memq (process-status p) '(exit signal))
                              (let ((out (with-current-buffer raw-buf
                                           (buffer-string)))
                                    (code (process-exit-status p)))
                                (kill-buffer raw-buf)
                                (with-current-buffer output-buf
                                  (goto-char (point-max))
                                  (insert (format "\n--- Test Run Complete (Exit Code: %d) ---\n" code)))
                                (if (zerop code)
                                    (funcall callback out)
                                  (message "Catch2 failed (exit %d): %s" code out))
                                (setq catch2-test-runner--process nil))))))
  (when status-msg (message "%s" status-msg)))


(defun catch2-test-runner--show-raw (label raw)
  "Show raw Catch2 output in *Catch2 Raw* and *Messages*."
  (let ((buf (get-buffer-create catch2-test-runner--raw-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== %s RAW OUTPUT ===\n\n" label))
        (insert raw)
        (insert "\n\n")))
    (display-buffer buf))
  (message "\n--- CATCH2 %s OUTPUT ---\n%s\n--- END ---\n" label raw))

(defun catch2-test-runner-show-raw-buffer ()
  "Switch to the raw output buffer for debugging."
  (interactive)
  (display-buffer catch2-test-runner--raw-buffer))

;; ----------------------------------------------------------------------
;; Cleanup
;; ----------------------------------------------------------------------
(defun catch2-test-runner--cleanup ()
  "Kill process and delete temp file."
  (when (and catch2-test-runner--process
             (process-live-p catch2-test-runner--process))
    (delete-process catch2-test-runner--process))
  (when (file-exists-p catch2-test-runner-junit-file)
    (delete-file catch2-test-runner-junit-file)))

(provide 'catch2-test-runner)
;;; catch2-test-runner.el ends here
