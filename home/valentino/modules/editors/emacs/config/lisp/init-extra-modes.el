;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup (:pkg cmake-mode)
  (:file-match (rx (or "CmakeLists.txt" ".cmake") eos)))

(setup (:pkg nix-mode)
  (:file-match (rx ".nix" eos)))

(setup (:pkg go-mode)
  (:file-match (rx ".go" eos)))

(setup (:pkg markdown-mode)
  (:file-match (rx (or ".md" ".markdown" ".mdown") eos)))

(setup (:pkg yaml-mode)
  (:file-match (rx (or ".yml" ".yaml") eos)))

(setup (:pkg json-mode)
  (:file-match (rx ".json" eos)))

(setup (:pkg dockerfile-mode)
  (:file-match (rx ".dockerfile" eos)))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
