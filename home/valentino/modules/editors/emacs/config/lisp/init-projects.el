;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(setup (:pkg envrc)
  (:needs "direnv")
  (:with-hook after-init-hook
    (:hook envrc-global-mode)))

(setup (:pkg magit)
  (:autoload magit-status)

  ;; Magit related functions
  (defun vb/exec-regex-kill-buffers-on-magit()
    "Kill magit buffers without asking for confirmation."
    (interactive)
    (vb/regex-kill-buffers "magit"))

  (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
           ;; Suppress the message we get about "Turning on magit-auto-revert-mode" when loading Magit.
           magit-no-message '("Turning on magit-auto-revert-mode...")

           ;; Don't try to save unsaved buffers when using Magit. We know
           ;; perfectly well that we need to save our buffers if we want Magit
           ;; to see them.
           magit-save-repository-buffers nil
           git-commit-summary-max-length 100)
  (:bind-into magit-status-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "Q" #'(lambda () (interactive) (vb/regex-kill-buffers "magit"))
    [remap evil-execute-last-recorded-macro] #'(lambda () (interactive) (vb/regex-kill-buffers "magit"))
    [remap evil-record-macro] #'(lambda () (interactive) (vb/regex-kill-buffers "magit"))
    [remap evil-jump-forward] #'magit-section-toggle
    [remap evil-delete-char] #'magit-discard
    [remap evil-paste-before] #'magit-push
    [remap evil-find-char-backward] #'magit-pull
    [remap evil-find-char] #'magit-fetch
    [remap evil-change-whole-line] #'magit-submodule
    [remap evil-shell-command] #'magit-git-command
    [remap evil-ret] #'magit-diff-visit-file
    [remap evil-substitute] #'magit-stage))

(setup (:if-feature evil)
  (evil-define-key 'normal magit-mode-map
    "gr" #'magit-refresh
    "gR" #'magit-refresh-all))

(setup (:pkg blamer))

(setup ediff
  (:option ediff-window-setup-function 'ediff-setup-windows-plain ;; Don't spawn new window for ediff
           ediff-split-window-function 'split-window-horizontally))

(provide 'init-projects)
;;; init-projects.el ends here
