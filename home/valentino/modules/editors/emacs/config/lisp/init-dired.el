;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:
(setup dired
  ;; 'Kay, with this I'm good, maybe
  (defun vb/dired-open-file ()
    "In Dired, open the file named on this line through xdg-open."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun vb/dired-open-marked-files (&optional file)
    "Open the current FILE or Dired marked files in Emacs."
    (interactive)
    (let (doIt (myFileList
                (cond
                 ((eq major-mode 'dired-mode)
                  (dired-get-marked-files))
                 ((not file) (list (buffer-file-name)))
                 (file (list file)))))
      (setq doIt (if (<= (length myFileList) 30) t
                   (y-or-n-p "Open more than 30 files? ")))
      (mapc (lambda (fPath)
              (let ((process-connection-type nil))
                (start-process "" nil "emacsclient" fPath))) myFileList)))

  ;; Kill the current Dired buffer, then visit the file or directory
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Emacs 29 options
  (unless (version< emacs-version "29")
    (setopt dired-mouse-drag-files t
            dired-make-directory-clickable t
            mouse-drag-and-drop-region-cross-program t
            dired-free-space nil))

  ;; "-lAXhv --group-directories-first"
  (:option dired-listing-switches "-agho --group-directories-first"
           dired-clean-up-buffers-too t
           dired-omit-files "^\\.[^.].*"
           dired-recursive-copies 'always
           dired-recursive-deletes 'always
           dired-auto-revert-buffer t
           ;; Automatically revert Dired buffers after ‘dired-do’ operations
           dired-do-revert-buffer t
           ;; Whether Dired should create destination dirs when copying/removing files
           dired-create-destination-dirs 'ask
           dired-omit-verbose nil
           dired-kill-when-opening-new-dired-buffer t
           dired-auto-revert-buffer #'dired-directory-changed-p
           dired-dwim-target t
           dired-hide-details-hide-symlink-targets nil
           dired-clean-confirm-killing-deleted-buffers nil  ;; don't ask about killing buffer visiting file
           dired-deletion-confirmer 'y-or-n-p
           delete-by-moving-to-trash t)

  (:with-map dired-jump-map
    (:bind "j" dired-jump))

  (:with-map dired-mode-map
    (:bind
     "C-<return>" alacritty
     "C-c C-e" wdired-change-to-wdired-mode
     "C-c C-c" dired-hide-details-mode
     "M-<return>" vb/dired-open-file
     "C-c o" vb/dired-open-file
     "o" vb/dired-open-file
     "O" vb/dired-open-marked-files))

  (setup (:if-feature general)
    (vb/leader-key "d" '(dired :which-key "File Manager")))
  (setup (:if-feature evil)
    (evil-define-key 'normal dired-mode-map
      (kbd "gr") 'revert-buffer
      (kbd "gg") 'evil-goto-first-line
      (kbd "G") 'evil-goto-line)))

(setup (:if-feature evil)
  (:with-map dired-mode-map
    (:bind
     [remap evil-backward-char] #'dired-up-directory
     [remap evil-forward-char] #'dired-find-file)))

(setup (:pkg dirvish)
  (:require dirvish)

  ;; Preview directory using exa command
  (dirvish-define-preview exa (file)
    "Use `exa' to generate directory preview."
    :require ("exa") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("exa" "-al" "--color=always" "--icons"
                 "--group-directories-first" ,file))))
  (add-to-list 'dirvish-preview-dispatchers 'exa)

  (:option dirvish-attributes '(all-the-icons
                                file-time
                                file-size
                                collapse
                                subtree-state
                                vc-state
                                git-msg)
           dirvish-cache-dir (expand-file-name "dirvish/" .var)
           dirvish-reuse-session nil
           ;; specific height for the in single window and full-frame sessions
           dirvish-header-line-height '(25 . 35)
           dirvish-mode-line-height 25 ;; shorthand for '(25 . 25)

           ;; Appearance
           ;; Segments
           ;; 1. the order of segments *matters* here
           ;; 2. it's ok to place raw string inside
           dirvish-use-header-line 'global ;; Make header line span all panes
           dirvish-header-line-format
           '(:left (path) :right (free-space))
           dirvish-mode-line-format
           '(:left (sort file-time " " file-size symlink) :right (omit yank index))
           ;;dirvish-path-separators (list " " "  " " ⋗ ")
           dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))

  (:with-map dired-mode-map
    (:bind
     "C-c f" #'dirvish-fd
     "a" #'dirvish-quick-access
     "." #'dired-create-empty-file
     "f" #'dirvish-file-info-menu
     "y" #'dirvish-yank-menu
     "s" #'dirvish-quicksort ;; remapped `dired-sort-toggle-or-edit'
     "v" #'dirvish-vc-menu   ;; remapped `dired-view-file'
     "q" #'dirvish-quit
     "L" #'dirvish-history-go-forward
     "H" #'dired-omit-mode
     "m" #'dired-mark
     "M" #'dirvish-mark-menu
     "o" #'vb/dired-open-file
     "TAB" #'dirvish-subtree-toggle

     "M-f" #'dirvish-history-go-forward
     "M-b" #'dirvish-history-go-backward
     "M-l" #'dirvish-ls-switches-menu
     "M-t" #'dirvish-layout-toggle
     "M-s" #'dirvish-setup-menu
     "M-e" #'dirvish-emerge-menu
     "M-j" #'dirvish-fd-jump))


  (dirvish-override-dired-mode))


(provide 'init-dired)
;;; init-dired.el ends here
