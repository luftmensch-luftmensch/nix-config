;;; init-games.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs can play games too

;;; Code:
(setup tetris
  (:option gamegrid-glyph-height-mm 10
           gamegrid-user-score-file-directory (expand-file-name "games/" .var))

  (:bind-into tetris-mode-map
    "è" tetris-pause-game
    "R" tetris-start-game

    [remap evil-paste-after] #'tetris-pause-game
    [remap evil-forward-char] #'tetris-move-right
    [remap evil-backward-char] #'tetris-move-left
    [remap evil-previous-line] #'tetris-rotate-next
    [remap evil-lookup] #'tetris-rotate-prev
    [remap evil-next-line] #'tetris-move-down
    [remap evil-join] #'tetris-move-bottom
    [remap evil-record-macro] #'quit-window))

(provide 'init-games)
;;; init-mail.el ends here
