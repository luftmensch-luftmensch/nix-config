;;; init-evil.el --- Evil mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; (setq-default evil-want-keybinding nil)
(setup (:pkg evil)
  (:require evil)
  ;; (:with-after evil
  ;;   (:pkg evil-collection)
  ;;   (evil-collection-init))
  (:option evil-want-integration t    ;; This is optional since it's already set to t by default.
           ;; evil-want-keybinding nil
           evil-emacs-state-modes nil
           evil-want-fine-undo t)  ;; By default while in insert all changes are one big blob. Be more granular
  (evil-mode 1)
  (setup (:if-feature general)
    (general-def
      :states '(normal)
      ;; Remapping normal state keybindings
      ;; vim vinigar style
      "-" #'(lambda () (interactive) (dired "."))
      "#" 'comment-line
      "C-S-J" #'evil-window-move-very-bottom
      "C-S-K" #'evil-window-move-very-top
      "C-S-H" #'evil-window-move-far-right
      "C-S-L" #'evil-window-move-far-left
      ;; Taken from https://github.com/minad/consult/issues/318
      "n" #'(lambda () (interactive) (search-forward (car consult--line-history)))
      "N" #'(lambda () (interactive) (search-backward (car consult--line-history)))
      ;; "<left>" 'vb/dont-arrow-me-bro
      ;; "<right>" 'vb/dont-arrow-me-bro
      ;; "<up>" 'vb/dont-arrow-me-bro
      ;; "<down>" 'vb/dont-arrow-me-bro

      "(" 'evil-previous-open-paren
      ")" 'evil-next-close-paren)))
(provide 'init-evil)
;;; init-evil.el ends here
