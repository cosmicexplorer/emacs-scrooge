;;; scrooge.el --- Major mode for Apache Scrooge files

;; Keywords: scrooge

;;; Commentary:

;; Adapted from thrift.el provided by the Apache Thrift repository
;; (https://thrift.apache.org/) by Danny McClanahan
;; <danieldmcclanahan@gmail.com>.

;;; Code:

(require 'font-lock)

;; compat for older emacs
(defvar jit-lock-start)
(defvar jit-lock-end)

(defvar scrooge-mode-hook nil)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . scrooge-mode))

(defvar scrooge-indent-level 2
  "Defines 2 spaces for scrooge indentation.")

;; syntax coloring
(defconst scrooge-font-lock-keywords
  (list
   '("\\<\\(include\\|struct\\|exception\\|typedef\\|const\\|enum\\|service\\|extends\\|void\\|oneway\\|throws\\|optional\\|required\\)\\>" . font-lock-keyword-face) ;; keywords
   '("\\<\\(bool\\|byte\\|i16\\|i32\\|i64\\|double\\|string\\|binary\\|map\\|list\\|set\\)\\>" . font-lock-type-face) ;; built-in types
   '("\\<\\([0-9]+\\)\\>" . font-lock-variable-name-face)     ;; ordinals
   '("\\<\\(\\w+\\)\\s-*(" (1 font-lock-function-name-face))  ;; functions
   '("^\\(\\(?:#@\\)?\\)\\(namespace\\)[ \t\v\f]+\\([^ \t\v\f]+\\)[ \t\v\f]+\\([^ \t\v\f].*\\).*?$" .
     ((1 font-lock-keyword-face)
      (2 font-lock-builtin-face)
      (3 font-lock-type-face)
      (4 font-lock-string-face))) ;; namespace decls
   '(scrooge-match-real-hash-comments . ((0 font-lock-comment-face)))
   )
  "Scrooge Keywords.")

;; indentation
(defun scrooge-indent-line ()
  "Indent current line as Scrooge code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(}\\|throws\\)")
          (if (looking-at "^[ \t]*}")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) scrooge-indent-level)))
                (if (< cur-indent 0)
                    (setq cur-indent 0)))
            (progn
              (save-excursion
                (forward-line -1)
                (if (looking-at "^[ \t]*[\\.<>[:word:]]+[ \t]+[\\.<>[:word:]]+[ \t]*(")
                    (setq cur-indent (+ (current-indentation) scrooge-indent-level))
                  (setq cur-indent (current-indentation))))))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^.*{[^}]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) scrooge-indent-level))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*throws")
                  (progn
                    (setq cur-indent (- (current-indentation) scrooge-indent-level))
                    (if (< cur-indent 0)
                        (setq cur-indent 0))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*[\\.<>[:word:]]+[ \t]+[\\.<>[:word:]]+[ \t]*([^)]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) scrooge-indent-level))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*\\/\\*")
                  (progn
                    (setq cur-indent (+ (current-indentation) 1))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*\\*\\/")
                  (progn
                    (setq cur-indent (- (current-indentation) 1))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              ))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;; C/C++- and sh-style comments; also allowing underscore in words
(defvar scrooge-mode-syntax-table
  (let ((scrooge-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" scrooge-mode-syntax-table)
    ;; #-comments removed
    (modify-syntax-entry ?/ ". 124" scrooge-mode-syntax-table)
    (modify-syntax-entry ?* ". 23b" scrooge-mode-syntax-table)
    (modify-syntax-entry ?\n ">" scrooge-mode-syntax-table)
    scrooge-mode-syntax-table)
  "Syntax table for scrooge-mode")

(defconst scrooge-comment-property 'scrooge-comment)

(defun scrooge-syntax-propertize-hash-comments (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^#" end t)
      (let* ((name-str "@namespace")
             (e (+ (point) (length name-str))))
        (unless (string= (buffer-substring-no-properties (point) e)
                       name-str)
          (backward-char)
          (let ((st (line-beginning-position))
                (end (1+ (line-end-position))))
            (put-text-property st end scrooge-comment-property
                               (list st end))
            (goto-char end)))))))

(defun scrooge-match-real-hash-comments (last)
  (let ((cur (get-text-property (point) scrooge-comment-property))
        pos)
    (unless cur
      (setq pos (next-single-char-property-change
                 (point) scrooge-comment-property nil last)
            cur (get-text-property pos scrooge-comment-property)))
    (when cur
      (set-match-data cur)
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      cur)))

(defun scrooge-syntax-propertize-extend-region (start end)
  (let* ((b (line-beginning-position))
         (e (if (and (bolp) (> (point) b)) (point)
              (min (1+ (line-end-position)) (point-max)))))
    (unless (and (= start b) (= end e)) (cons b e))))

(defun scrooge-font-lock-extend (start end _)
  (let ((res (scrooge-syntax-propertize-extend-region start end)))
    (when res
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

;;;###autoload
(defun scrooge-mode ()
  "Mode for editing Scrooge files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table scrooge-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(scrooge-font-lock-keywords))
  ;; (set (make-local-variable 'font-lock-multiline) t)
  (setq major-mode 'scrooge-mode)
  (setq mode-name "Scrooge")
  (run-hooks 'scrooge-mode-hook)
  (set (make-local-variable 'indent-line-function) 'scrooge-indent-line)
  (set (make-local-variable 'syntax-propertize-function)
       #'scrooge-syntax-propertize-hash-comments)
  (add-hook 'syntax-propertize-extend-region-functions
            'scrooge-syntax-propertize-extend-region t t)
  (add-hook 'jit-lock-after-change-extend-region-functions
            'scrooge-font-lock-extend t t)
  ;; otherwise hash comments aren't highlighted upon opening
  (scrooge-syntax-propertize-hash-comments (point-min) (point-max)))

(provide 'scrooge)
;;; scrooge.el ends here
