;;; init-functions.el --- Custom generics functions -*- lexical-binding: t -*-

;;; Commentary:

;; Functiong goodies

;;; Code:

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

(defun open-pdf ()
  "Open the corresponding pdf based on the current org file opened."
  (interactive)
  (if (or (eq (length buffer-file-name) 0) (eq (file-exists-p (concat (file-name-base (buffer-file-name)) ".pdf")) nil))
      (message "Please select a valid file")
    (call-process-shell-command (concat "zathura " (file-name-base (buffer-file-name)) ".pdf > /dev/null 2>&1 & disown"))))

(provide 'init-functions)
;;; init-functions.el ends here
