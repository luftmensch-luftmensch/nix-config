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

  ;; https://github.com/akermu/emacs-libvterm/issues/711
  (defun vb/vterm--get-color (index &rest args)
    "Retrieve the color by INDEX from `vterm-color-palette'.

A special INDEX of -1 refers to the default colors.  ARGS can
optionally include `:underline’ or `:inverse-video’ to indicate
cells with these attributes.  If ARGS contains `:foreground',
return the foreground color of the specified face instead of the
background color.

This function addresses an issue where the foreground color in
vterm may match the background color, rendering text invisible."
    (let ((foreground    (member :foreground args))
          (underline     (member :underline args))
          (inverse-video (member :inverse-video args)))
      (let* ((fn (if foreground #'face-foreground #'face-background))
             (base-face
              (cond ((and (>= index 0)
                          (< index 16))
                     (elt vterm-color-palette index))
                    ((and (= index -1) foreground
                          underline)
                     'vterm-color-underline)
                    ((and (= index -1)
                          (not foreground)
                          inverse-video)
                     'vterm-color-inverse-video)
                    ((and (= index -2))
                     'vterm-color-inverse-video))))
        (if base-face
            (funcall fn base-face nil t)
          (if (eq fn 'face-background)
              (face-foreground 'default)
            (face-background 'default))))))

  (advice-add 'vterm--get-color :override #'vb/vterm--get-color)


  (defun alacritty()
    "Open the terminal on the current directory."
    (interactive)
    (let ((terminal (pcase (system-name)
                      ("kronos" "foot -a=default_term 2>/dev/null & disown")
                      ("atlas" "alacritty 2>/dev/null & disown"))))
      (call-process-shell-command terminal)))
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
