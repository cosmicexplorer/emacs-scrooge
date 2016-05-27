;;; scrooge.el --- Major mode for Apache Scrooge files

;; Keywords: scrooge

;;; Commentary:

;; Adapted from thrift.el provided by the Apache Thrift repository
;; (https://thrift.apache.org/) by Danny McClanahan
;; <danieldmcclanahan@gmail.com>.

;;; Code:

(require 'thrift)
(require 'font-lock)

;; compat for older emacs
(defvar jit-lock-start)
(defvar jit-lock-end)

(defvar scrooge-mode-hook nil)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . scrooge-mode))

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
   '(scrooge-match-real-hash-comments . ((0 font-lock-comment-face))))
  "Scrooge Keywords.")

;; C/C++- and sh-style comments; also allowing underscore in words
(defvar scrooge-mode-syntax-table
  (let ((scrooge-mode-syntax-table (make-syntax-table thrift-mode-syntax-table)))
    ;; #-comments removed
    (modify-syntax-entry ?# "." scrooge-mode-syntax-table)
    scrooge-mode-syntax-table)
  "Syntax table for scrooge-mode.")

(defconst scrooge-comment-property 'scrooge-comment
  "Text property used for denoting comments.")

(defun scrooge-syntax-propertize-hash-comments (beg end)
  "Propertize # line comments and not #@namespace lines between BEG and END."
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
  "Add fonts to propertized hash comments between point and LAST."
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
  "Extend region to propertize between START and END upon change."
  (let* ((b (line-beginning-position))
         (e (if (and (bolp) (> (point) b)) (point)
              (min (1+ (line-end-position)) (point-max)))))
    (unless (and (= start b) (= end e)) (cons b e))))

(defun scrooge-font-lock-extend (start end _)
  "Delegate to `scrooge-syntax-propertize-extend-region'.
Propertize between START and END."
  (let ((res (scrooge-syntax-propertize-extend-region start end)))
    (when res
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

;;;###autoload
(define-derived-mode scrooge-mode thrift-mode "Scrooge"
  "Mode for editing Scrooge files."
  :syntax-table scrooge-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(scrooge-font-lock-keywords))
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
