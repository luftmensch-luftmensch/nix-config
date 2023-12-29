;;; init-help.el --- Sometimes we need help from someone/something :) -*- lexical-binding: t -*-

;;; Commentary:

;; The minibuffer is our best friend, let's use it more with extensions.

;;; Code:

(setup (:pkg which-key)
       (:hide-mode)
       (:option
        which-key-idle-delay 0.2
        which-key-prefix-prefix "◉ ")
       (which-key-mode 1))

(provide 'init-help)
;;; init-help.el ends here
