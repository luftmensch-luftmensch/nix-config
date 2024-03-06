;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain appearance settings stuff.

;;; Code:

(setup appearance
  ;; https://www.emacswiki.org/emacs/TransparentEmacs
  (set-frame-parameter nil 'alpha-background 90)
  (add-to-list 'default-frame-alist '(alpha-background . 90))

  (:option frame-title-format '(:eval (concat "emacs@" system-name " - "(format "%s  [%s]" (buffer-name) major-mode)))
           icon-title-format frame-title-format

           ;; Calendar & Date setup
           calendar-date-style 'european
           calendar-day-name-array ["Dom" "Lun" "Mar" "Mer" "Gio" "Ven" "Sab"]
           calendar-day-abbrev-array calendar-day-name-array
           calendar-day-header-array calendar-day-name-array
           calendar-month-name-array ["Gen" "Feb" "Mar" "Apr" "Mag" "Giu" "Lug" "Ago" "Set" "Ott" "Nov" "Dic"]

           display-time-default-load-average nil
           highlight-nonselected-windows nil

           ;; Other graphical stuff
           visible-bell nil
           x-gtk-use-system-tooltips t
           x-stretch-cursor nil
           use-dialog-box nil
           use-file-dialog nil
           echo-keystrokes 0.1
           ;; Cursor
           cursor-in-non-selected-windows nil
           cursor-type t
           blink-cursor-mode 0
           ;; Lines
           truncate-lines nil
           visual-line-mode t
           indicate-buffer-boundaries nil

           ;; Bidirectional settings
           ;; bidi-display-reordering 'left-to-right
           ;; bidi-paragraph-direction 'left-to-right
           ))

;; You must run `all-the-icons-install-fonts` the first time.
(setup (:pkg all-the-icons) (:require all-the-icons))

(provide 'init-appearance)
;;; init-appearance.el ends here
