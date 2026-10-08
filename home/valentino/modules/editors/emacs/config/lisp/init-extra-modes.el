;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup (:pkg cmake-mode)
  (:match-file "CMakeLists.txt" "*.cmake"))

(setup (:pkg nix-ts-mode)
  (:match-file "*.nix"))

(setup (:pkg go-mode)
  (:match-file "*.go"))

(setup (:pkg markdown-mode)
  (:match-file "*.md" "*.markdown" "*.mdown"))

(setup (:pkg yaml-mode)
  (:match-file "*.yml" "*.yaml"))

(setup (:pkg json-mode)
  (:match-file "*.json"))

(setup (:pkg dockerfile-mode)
  (:match-file "Dockerfile" "Dockerfile.*" "*.dockerfile"))

;; Organize and send HTTP requests from Emacs with verb
;; (setup (:pkg verb))

;; Organize and send HTTP requests from Emacs with plz-see (wrapper around plz)
;; (setup (:pkg plz-see))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
