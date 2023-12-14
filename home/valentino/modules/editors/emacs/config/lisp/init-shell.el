;;; init-shell.el --- Emacs <3 Shell -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain `eshell', `vterm', and similar terminal emulators available for Emacs.
;;; Code:
(setup vterm
  (unless (vb/using-nix-p)
    (:pkg vterm))

  (:autoload vterm vterm-other-window)

  (:option vterm-buffer-name " "
           vterm-max-scrollback 5000
           vterm-kill-buffer-on-exit t)
  (:bind-into vterm-mode-map
    [remap evil-delete-backward-word] #'vterm--self-insert
    [remap evil-emacs-state] #'vterm--self-insert
    [remap evil-paste-last-insertion] #'vterm--self-insert
    [remap evil-complete-next] #'vterm--self-insert
    [remap evil-complete-previous] #'vterm--self-insert
    [remap evil-copy-from-below] #'vterm--self-insert
    [remap evil-insert-digraph] #'vterm--self-insert
    [remap evil-shift-left-line] #'vterm--self-insert)

  (defun alacritty()
    "Open the terminal on the current directory."
    (interactive)
    (call-process-shell-command "alacritty > /dev/null 2>&1 & disown"))
  (add-to-list 'display-buffer-alist
               '("\xe795 " ;; Original regex: "\*vterm\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0)))

  (setup (:if-feature general)
    ;; Additional keymaps using <SPC> prefix
    (vb/leader-key "s" '(vterm :which-key "Vterm")))

  (setup (:if-feature evil)
    (evil-set-initial-state 'vterm-mode 'insert)
    (evil-define-key 'normal vterm-mode-map
        "u" 'vterm-undo
        "P" 'vterm-yank)))

(setup (:pkg multi-vterm)
  (:with-after vterm
    (:option multi-vterm-dedicated-window-height-percent 20)))

(provide 'init-shell)
;;; init-shell.el ends here
