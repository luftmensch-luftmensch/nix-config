;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(setup (:pkg envrc)
  (:needs "direnv")
  (:with-hook after-init-hook
    (:hook envrc-global-mode)))

(setup ediff
  (:option ediff-window-setup-function 'ediff-setup-windows-plain ;; Don't spawn new window for ediff
           ediff-split-window-function 'split-window-horizontally
           ediff-keep-variants nil
           ediff-show-clashes-only t))

(provide 'init-projects)
;;; init-projects.el ends here
