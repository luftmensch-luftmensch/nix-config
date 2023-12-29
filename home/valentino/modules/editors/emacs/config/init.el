;;; init.el ---  Emacs configuration -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: luftmensch-luftmensch

;; This file IS NOT intended to be edited!

;;; Commentary:
;;                    ____,
;;                   /.---|
;;                   `    |     ___
;;                       (=\.  /-. \
;;                        |\/\_|"|  |
;;                        |_\ |;-|  ;
;;                        | / \| |_/ \
;;                        | )/\/      \
;;                        | ( '|  \   |
;;                        |    \_ /   \
;;                        |    /  \_.--\
;;                        \    |    (|\`
;;                         |   |     \
;;                         |   |      '.
;;                         |  /         \
;;                         \  \.__.__.-._)
;;
;;
;; Well, hello there! How are you doing wanderer? Looking for some
;; lisp goodness?  You might find it here, you might not.  If you do
;; find what you are looking for here, feel free take them with you,
;; give them a new life, a new filesystem, a new home.  All I ask of
;; you is to treat them with love and care.  They have always been
;; with me, playing along with my musing, catching little typos and
;; finding little bugs.  They stuck strong to my side even when the
;; Rust borrow checker came for me.  I'm not gonna lie, there has been
;; many a times where I have doubted my skills, but they have always
;; believed in me.
;;
;; If these parenthesis could talk, they would have a lot of stories
;; to tell.  Some good, some bad, some really ugly.  But at the end of
;; the day, I'm sure they are all happy to be where they are.
;;
;; If they give you any trouble, my GitHub issues is always open
;; unlike the doors of heaven.  They probably won't, these are the
;; good ones, but God sometimes have different plans, and everyone
;; gets hit with hard times.
;;
;; For any additional documentation and for editing this file, see leaf.org
;;
;; Good luck!

;; -------------------------------------------------------------------------------- ;;
;;   This file was auto-tangled from an orgmode file. (C)  luftmensch-luftmensch    ;;
;; -------------------------------------------------------------------------------- ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; Do not pollute `init.el` with user customizations, use instead the proper `custom.el`
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file 'noerror 'nomessage))
;; A sane default for bookmarks (synched between devices)
;; (setq bookmark-default-file "~/config/emacs/bookmarks")
(setq bookmark-default-file (locate-user-emacs-file "bookmarks")
      ;; The current config is a symlink handled by nix
      vc-follow-symlinks t)

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever, and should be remapped
(global-unset-key (kbd "C-z"))

;; Functions to determine if we are using a Nix installation of Emacs, or not, then we set our configuration path.
(defun vb/using-nix-p ()
  "Verifies if the running Emacs executable is under the `/nix/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of nix
    (string-prefix-p "/nix/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))
(defvar vb/config-path
  (let ((real-path (expand-file-name
                    "config/emacs/"
                    (getenv "HOME"))))
    (if (and (vb/using-nix-p)
             (file-exists-p real-path))
        (expand-file-name real-path)
      (expand-file-name user-emacs-directory))))

;; Require package management file
(require 'init-setup)

(require 'init-performance)

(require 'init-help)

(require 'init-fonts)

(require 'init-themes)

(require 'init-appearance)

(require 'init-modeline)

(require 'init-keybindings)
(require 'init-evil)

;; (require 'init-dash)

(require 'init-editing)

(require 'init-windows)

(require 'init-buffers)

(require 'init-dired)

(require 'init-complete)

(require 'init-embark)

(require 'init-consult)

(require 'init-complete-in-buffer)

(require 'init-org)

(require 'init-org-languages)

(require 'init-org-export)

(require 'init-projects)

(require 'init-code-style)

(require 'init-spell-and-check)

(require 'init-lsp)

;; (require 'init-snippets)

(require 'init-extra-modes)

(require 'init-mail)

;;(require 'init-tex)

(require 'init-reading)

(require 'init-shell)

(require 'init-telega)
(require 'init-games)

;; (require 'init-media)

;; Nice mode to control your system (and user) services without leaving Emacs.
(setup (:pkg daemons))


;;; init.el ends here
