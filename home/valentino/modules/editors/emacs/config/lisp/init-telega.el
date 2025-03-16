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

  (:with-map telega-root-mode-map
    (:bind
     "<tab>" telega-chat-button-toggle-view
     "TAB" telega-chat-button-toggle-view
     [remap evil-record-macro] #'bury-buffer
     [remap evil-execute-last-recorded-macro] #'telega-kill))

  (:with-map telega-chat-mode-map
    (:bind
     [remap telega-msg-resend] 'telega-msg-forward-dwim
     [remap evil-record-macro] #'kill-current-buffer
     [remap evil-ret] #'telega-chatbuf-newline-or-input-send))

  (:with-map telega-image-mode-map
    (:bind
     [remap evil-record-macro] #'bury-buffer
     [remap evil-execute-last-recorded-macro] #'kill-current-buffer
     "C-+" #'image-increase-size
     "C--" #'image-decrease-size))

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
    (:hook telega-notifications-mode))
  ;; I don't really care of birthday top popup in telega
  (:when-loaded (telega-contact-birthdays-mode -1)))

(setup (:if-feature general)
  (vb/leader-key
    "t" '(:ignore t :which-key "Telega")
    "t t" '(telega :which-key "Open Telega")
    "t c" '(telega-chat-with :which-key "Chat with")
    "t s" '(telega-view-search :which-key "Search for...")
    "t f" '(telega-chabuf-input-formatting-set :which-key "Format region")
    "t q" '(telega-kill :wich-key "Close Telega")))


(setup (:if-feature evil)
  (evil-define-key 'normal telega-root-mode-map "J" telega-root-fastnav-map))
(provide 'init-telega)
;;; init-telega.el ends here
