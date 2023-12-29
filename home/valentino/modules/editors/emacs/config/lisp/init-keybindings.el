;;; init-keybindings.el --- Evil mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(setup (:pkg general)
  (general-evil-setup)
  (defun code/indent (beg end times)
    "Indent selection for n TIMES with BEG and END directive."
    (interactive "r\nP")
    (indent-code-rigidly beg end (* times 2)) (setq deactivate-mark nil))
  (general-vmap
    :keymaps 'override
    "<" #'(lambda (b e n) (interactive "r\nP") (code/indent b e (if n (- n) -1)))
    ">" #'(lambda (b e n) (interactive "r\nP") (code/indent b e (if n n 1))))
  (general-create-definer vb/leader-key
    :states '(normal dired insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Override <C-c> emacs keymap
  (general-create-definer vb/ctrl-c-keys
    :prefix "C-c")

  ;; Override <C-x> emacs keymap
  (general-create-definer vb/ctrl-x-keys
    :prefix "C-x")

  (vb/leader-key
    ;; Elfeed
    "e"  '(:ignore t :which-key "Elfeed")
    "el" '(elfeed :which-key "Elfeed - view")
    "eu" '(elfeed-update :which-key "Elfeed - update")

    ;; EMMS
    "a"  '(:ignore t :which-key "Music")
    "a a" '(emms-play-directory :which-key "Play playlist")
    "a s" '(emms-play-directory :which-key "Play/Pause")
    "a x" '(emms-stop :which-key "Stop")
    "a n" '(emms-next :which-key "Next track")
    "a p" '(emms-previous :which-key "Previous track")

    "s" '(vterm :which-key "Apri terminale")

    ;; Reload
    "r" '(:ignore t :which-key "Reload")
    "r r" '(config-reload :which-key "Reload")

    ;; Agenda
    "b"  '(:ignore t :which-key "Agenda")
    "b a" '(org-agenda :which-key "Agenda view")
    "b n" '(org-capture :which-key "Capture note")

    ;; Magit
    "g"  '(:ignore t :which-key "Git")
    "gg" #'((lambda () (interactive) (magit "~/config")) :which-key "Personal Config")
    "gG" #'((lambda () (interactive) (magit "~/nix-config")) :which-key "Nixos Config")
    "g." #'((lambda () (interactive) (magit "~/nix-config")) :which-key "Nixos Config")
    "g." '(magit :which-key "Open Magit in current directory")

    ;; Mail with NotMuch
    "o" '(:ignore t :which-key "Mail")
    "om" '(notmuch :which-key "Mail dashboard")
    "oc" '(notmuch-mua-new-mail :which-key "Compose mail"))

  (vb/ctrl-c-keys
    "a"   '(org-agenda-list :which-key "Open agenda")
    "! l"  '(consult-flymake :which-key "Show errors")
    "! r"  '(consult-ripgrep :which-key "Ripgrep")
    "! a"  '(embark-act :which-key "Embark Act"))

  (vb/ctrl-x-keys
    "b" '(consult-buffer :wich-key "Show buffers")
    "C-b" '(consult-buffer :wich-key "Show buffers")
    )

  ;; All-mode keymaps [Ovverride normal behaviour]
  (general-def
    :keymaps 'override

    ;; Global keybindings
    "<f12>" 'revert-buffer ;; Try out -> (revert-buffer :ignore-auto :noconfirm)
    "<XF86Favorites>" 'revert-buffer
    "<XF86AudioRaiseVolume>" 'revert-buffer
    "<Favorites>" 'revert-buffer

    ;; Remapping global keybindings
    "C-M-k" #'(lambda () (interactive)
                (mapc 'kill-buffer (buffer-list))
                (cd "~/")
                (message "All buffers closed"))
    "C-x C-s" #'(lambda () (interactive) (save-some-buffers t))
    "C-c RET" #'(lambda () (interactive) (if (eq major-mode 'org-mode) (org-open-at-point) (browse-url-at-point)))

    ;;"<escape>" 'keyboard-escape-quit

    "M-[" 'previous-buffer
    "M-]" 'next-buffer
    "M-m" 'menu-bar-mode ; Opens the menu with M-m, very KDE-ish
    "C-x C-k" '(dirvish-side :which-key "Treemacs view")))
(provide 'init-keybindings)
