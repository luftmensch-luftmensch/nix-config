;;; init-functions.el --- Custom generics functions -*- lexical-binding: t -*-

;;; Commentary:

;; Functiong goodies

;;; Code:

;; (replace-in-string "+unread" "READ" elfeed-search-filter)
(defun replace-in-string (what with in)
  "Wrapper around `replace-regexp-in-string' and `regexp-quote'.
WHAT is the string that witll be substituted with WITH in IN"
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun vb/toggle-continuation-fringe-indicator ()
  "Toggle displaying the continuation indicator."
  (interactive)
  (setq-default
   fringe-indicator-alist
   (if (assq 'continuation fringe-indicator-alist)
       (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)
     (cons '(continuation right-curly-arrow left-curly-arrow) fringe-indicator-alist))))

(provide 'init-functions)
;;; init-functions.el ends here
