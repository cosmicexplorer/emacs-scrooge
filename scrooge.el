;;; scrooge.el --- Major mode for Twitter Scrooge files

;; Author: Daniel McClanahan <danieldmcclanahan@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (thrift "20140312.1348"))
;; Keywords: scrooge, thrift

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

(defconst scrooge-special-namespace-regexp
  "^\\(\\(?:#@\\)?\\)\\(namespace\\)[ \t\v\f]+\\([^ \t\v\f]+\\)[ \t\v\f]+\\([^ \t\v\f].*\\).*?$")

;; syntax coloring
(defconst scrooge-font-lock-keywords
  (list
   '("\\<\\(include\\|struct\\|exception\\|typedef\\|const\\|enum\\|service\\|extends\\|void\\|oneway\\|throws\\|optional\\|required\\)\\>" . font-lock-keyword-face) ;; keywords
   '("\\<\\(bool\\|byte\\|i16\\|i32\\|i64\\|double\\|string\\|binary\\|map\\|list\\|set\\)\\>" . font-lock-type-face) ;; built-in types
   '("\\<\\([0-9]+\\)\\>" . font-lock-variable-name-face)     ;; ordinals
   '("\\<\\(\\w+\\)\\s-*(" (1 font-lock-function-name-face))  ;; functions
   (cons scrooge-special-namespace-regexp
         '((1 font-lock-keyword-face)
           (2 font-lock-builtin-face)
           (3 font-lock-type-face)
           (4 font-lock-string-face))) ;; namespace decls
   '("^#.*\\(\n\\|\\'\\)" . (0 font-lock-comment-face)))
  "Scrooge Keywords.")

;; C/C++- and sh-style comments; also allowing underscore in words
(defvar scrooge-mode-syntax-table
  (let ((scrooge-mode-syntax-table
         (make-syntax-table thrift-mode-syntax-table)))
    ;; #-comments removed
    (modify-syntax-entry ?# "." scrooge-mode-syntax-table)
    scrooge-mode-syntax-table)
  "Syntax table for scrooge-mode.")

(defconst scrooge-comment-property 'scrooge-comment
  "Text property used for denoting comments.")

(defun scrooge-syntax-propertize-extend-region (start end)
  "Extend region to propertize between START and END upon change."
  (let* ((b (line-beginning-position))
         (e (if (and (bolp) (> (point) b)) (point)
              (min (1+ (line-end-position)) (point-max)))))
    (unless (and (= start b) (= end e)) (cons b e))))

(defun scrooge-font-lock-extend (start end _)
  "Extend font locking beyond START and END if necessary."
  (let ((res (scrooge-syntax-propertize-extend-region start end)))
    (when res
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

;;;###autoload
(define-derived-mode scrooge-mode thrift-mode "Scrooge"
  "Mode for editing Scrooge files."
  :syntax-table scrooge-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(scrooge-font-lock-keywords))
  (add-hook 'jit-lock-after-change-extend-region-functions
            'scrooge-font-lock-extend t t))

(provide 'scrooge)
;;; scrooge.el ends here
