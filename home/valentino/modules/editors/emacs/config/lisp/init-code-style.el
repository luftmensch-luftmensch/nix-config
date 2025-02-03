;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand, and format all my code.
;; Plus, general tab settings, tree-sitter support, fancy stuff.

;;; Code:

(setup (:pkg format-all)
  (:hide-mode)
  (:hook-into prog-mode)
  (:global "<f1>" format-all-buffer))

(setup eldoc
  (:hide-mode)
  (global-eldoc-mode 1))

(setup (:pkg rainbow-mode)
  (:hook-into web-mode json-mode))

;; Tabs, indentation, and the TAB key
(setup indent
  (:option tab-always-indent 'complete
           tab-first-completion 'word-or-paren-or-punct
           tab-width 2
           indent-tabs-mode nil))

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(setup treesit
  (:only-if (treesit-available-p))
  (:option treesit-font-lock-level 4))

(setup treesit-auto
  (:only-if (treesit-available-p))
  (:pkg treesit-auto)

  (:load-after treesit)
  (:autoload global-treesit-auto-mode)

  (:option treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'init-code-style)
;;; init-code-style.el ends here
