;;; init-buffers.el --- Buffer navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer navigation and management

;; This package makes it easier to find a script to edit in $PATH.  The initial
;; `rehash-exes' is slow, but it's stored in `*exes*' as a caching mechanism.
;; However, I'm sure it could be improved.

;; In addition, `*exes*' currently contains /all/ executables in $PATH, which
;; ... maybe only the ones stored in some text format should be shown.


;;; Code:
;;; Unique names for buffers
(setup (:require uniquify)
  (:option uniquify-buffer-name-style 'forward
           uniquify-strip-common-suffix t
           uniquify-after-kill-buffer-p t))

;; Additional keymaps using <SPC> prefix
(setup (:if-feature general)
  (vb/leader-key
    "z"  '(kill-this-buffer :which-key "Close current buffer")
    "k"  '(kill-matching-buffers :which-key "Kill buffers (Regex)")
    "RET" '(bookmark-jump :which-key "Bookmarks")))

(setup buffers
  (defun make-executable ()
    "Mark current file executable."
    (interactive)
    (set-file-modes (buffer-file-name (current-buffer)) #o700))

  (defun vb/move-file (new-location)
    "Write this file to NEW-LOCATION, and delete the old one."
    (interactive (list (if buffer-file-name
                           (read-file-name "Move file to: ")
                         (read-file-name "Move file to: "
                                         default-directory
                                         (expand-file-name (file-name-nondirectory (buffer-name))
                                                           default-directory)))))
    (when (file-exists-p new-location)
      (delete-file new-location))
    (let ((old-location (buffer-file-name)))
      (write-file new-location t)
      (when (and old-location
                 (file-exists-p new-location))
        (delete-file old-location))))
  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "FNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (unless filename
        (error "Buffer '%s' is not visiting a file!" name))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (set-visited-file-name new-name)
        (rename-buffer new-name))))

  (defun move-buffer-file (dir)
    "Move both current buffer and file it's visiting to DIR."
    (interactive "DNew directory: ")
    (let* ((name (buffer-name))
           (filename (buffer-file-name))
           (dir
            (if (string-match dir "\\(?:/\\|\\\\)$")
                (substring dir 0 -1) dir))
           (newname (concat dir "/" name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
        (progn (copy-file filename newname 1)
               (delete-file filename)
               (set-visited-file-name newname)
               (set-buffer-modified-p nil)      t))))

  (defun delete-file-and-buffer ()
    "Deletes a buffer and the file it's visiting."
    (interactive)
    (when-let* ((file-name (buffer-file-name))
                (really (yes-or-no-p (format "Delete %s? "
                                             file-name))))
      (delete-file file-name)
      (kill-buffer)))

  (defun copy-file-name ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (if filename
          (progn
            (kill-new filename)
            (message "Copied '%s'" filename))
        (warn "Current buffer is not attached to a file!"))))

  (defun copy-buffer-path ()
    "Show and copy the full path to the current file in the minibuffer."
    (interactive)
    ;; list-buffers-directory is the variable set in dired buffers
    (if-let* ((path (or (buffer-file-name) list-buffers-directory)))
        (message (kill-new path))
      (error "Buffer not visiting a file"))

    (defconst *emacs-files* "/home/valentino/nix-config/home/valentino/modules/editors/emacs/config/lisp/"
      "All init files used by EMACS.")

    (defun emacs-init-files ()
      (interactive)
      (find-file (expand-file-name (completing-read "File> " (directory-files *emacs-files* nil directory-files-no-dot-files-regexp)) *emacs-files*))))

	(setq-default subword-mode))


(setup debug
  (:bind-into debugger-mode-map
    [remap evil-record-macro] 'top-level))

;; Regex for buffers
(defun vb/regex-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
    (kill-matching-buffers regexp)))

(provide 'init-buffers)
;;; init-buffers.el ends here
