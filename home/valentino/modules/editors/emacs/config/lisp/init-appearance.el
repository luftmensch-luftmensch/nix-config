;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain appearance settings stuff.

;;; Code:

(setup appearance
  (setq frame-title-format '(:eval (concat "emacs@" system-name " - "(format "%s  [%s]" (buffer-name) major-mode)))
        icon-title-format frame-title-format)

  ;; https://www.emacswiki.org/emacs/TransparentEmacs
  (set-frame-parameter nil 'alpha-background 90)

  (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; Stuff
  ;; Memo: Calendar back and forth M-{ M-}
  (setq calendar-week-start-day 1
        calendar-date-style 'european
        calendar-day-name-array ["Dom" "Lun" "Mar" "Mer" "Gio" "Ven" "Sab"]
        calendar-day-abbrev-array ["Dom" "Lun" "Mar" "Mer" "Gio" "Ven" "Sab"]
        calendar-day-header-array ["Dom" "Lun" "Mar" "Mer" "Gio" "Ven" "Sab"]
        calendar-month-name-array ["Gen" "Feb" "Mar" "Apr" "Mag"
  			                           "Giu" "Lug" "Ago" "Set" "Ott" "Nov" "Dic"])
  (setq display-time-default-load-average nil)
  (setq highlight-nonselected-windows nil)
  (setq echo-keystrokes 0.1)

  ;; Other graphical stuff
  (setq visible-bell nil)
  (setq x-gtk-use-system-tooltips t)
  (setq x-stretch-cursor nil)

  ;; Dialogs
  (setq use-dialog-box nil      ; Mouse events dialog
        use-file-dialog nil)    ; Disable dialog for files

  ;; Cursor
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type t)
  (blink-cursor-mode 0)

  ;; Bidirectional settings
  ;; (setq-default bidi-display-reordering 'left-to-right)
  ;; (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Lines related
  (setq-default truncate-lines nil)
  (setq-default visual-line-mode t)

  (setq-default indicate-buffer-boundaries nil))

;; You must run `all-the-icons-install-fonts` the first time.
(setup (:pkg all-the-icons)
  (:require all-the-icons))
(provide 'init-appearance)
