;;; init-telega.el --- Telegram on Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Here we go.  The idea of using Emacs for everything is (almost) real.
;; `telega' is a great client, maybe the best client around for Telegram.
;; Sometimes it has issues which depend on the version `tdlib' installed on your system, but what the hell: it's good!

;;; Code:
(setup telega
  (unless (vb/using-nix-p)
    (:pkg telega))

  (:autoload telega)
  (:option telega-directory (expand-file-name (concat user-emacs-directory "telega/"))
           ;; telega-directory (locate-user-emacs-file "telega/")
           telega-use-images t
           telega-emoji-font-family "Noto Color Emoji"
           telega-emoji-use-images nil 
           ;; telega-emoji-company-backend 'telega-company-emoji
           telega-completing-read-function completing-read-function
           telega-animation-play-inline 2
           telega-inserter-for-chat-button 'telega-ins--chat-full-2lines
           telega-chat-button-width 30
           switch-to-buffer-preserve-window-point t
           telega-chat--display-buffer-action '((display-buffer-reuse-window display-buffer-use-some-window))
           telega-root-fill-column (+ 50 telega-chat-button-width)
           telega-chat-fill-column (+ 50 telega-chat-button-width)
           telega-chat-input-markups '("markdown2" "org"))

  (put (get 'telega-chat 'button-category-symbol)
       :inserter 'telega-ins--chat-full-2lines)

  (:bind-into telega-root-mode-map
    "<tab>" telega-chat-button-toggle-view
    "TAB" telega-chat-button-toggle-view
    [remap evil-record-macro] #'bury-buffer
    [remap evil-execute-last-recorded-macro] #'telega-kill)

  (:bind-into telega-chat-mode-map
    [remap telega-msg-resend] 'telega-msg-forward-marked-or-at-point
    ;; [remap evil-record-macro] #'bury-buffer
    [remap evil-record-macro] #'kill-this-buffer
    [remap evil-ret] #'telega-chatbuf-newline-or-input-send)

  (:bind-into telega-image-mode-map
    [remap evil-record-macro] #'bury-buffer
    [remap evil-execute-last-recorded-macro] #'kill-this-buffer
    "C-+" #'image-increase-size
    "C--" #'image-decrease-size)

  (:when-loaded
    (:also-load telega-mnz)
    (:global "C-c t" telega-prefix-map))

  (:with-mode telega-chat-mode
    ;; From Andrew Tropin <3
    (:local-set completion-at-point-functions (mapcar
                                               #'cape-company-to-capf
                                               (append (list 'telega-company-emoji
                                                             'telega-company-username
                                                             'telega-company-hashtag)
                                                       (when (telega-chat-bot-p telega-chatbuf--chat)
                                                         '(telega-company-botcmd)))))
    (:hook telega-mnz-mode)
    (:hook visual-fill-column-mode))

  (:with-hook telega-load-hook
    (:hook telega-notifications-mode)))

(setup (:if-feature general)
  (vb/leader-key
    "t" '(:ignore t :which-key "Telega")
    "t t" '(telega :which-key "Open Telega")
    "t c" '(telega-chat-with :which-key "Chat with")
    "t s" '(telega-view-search :which-key "Search for...")
    "t q" '(telega-kill :wich-key "Close Telega")))


(setup (:if-feature evil)
  (evil-define-key 'normal telega-root-mode-map
    "J" telega-root-fastnav-map))
(provide 'init-telega)
;;; init-telega.el ends here
