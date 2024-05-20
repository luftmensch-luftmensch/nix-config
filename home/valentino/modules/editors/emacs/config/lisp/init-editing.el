;;; init-editing.el --- Basic editing configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file is pretty simple, it only contains editing related utilities and preferences.
;; It's still experimental and very poor, so I only consider it a starting point.

;;; Code:

;;
;;; General

;; Force UTF-8
(setup encoding
  (setq coding-system-for-read 'utf-8-unix)
  (setq coding-system-for-write 'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix utf-8-unix))
  (setq locale-coding-system 'utf-8-unix)
  (setq selection-coding-system 'utf-8)
  (setq x-select-request-type nil)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8-unix))


;;; Keep history and keep the order
(setup backup
  (:option auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" .var)))
           backup-directory-alist `(("." . ,(expand-file-name "backups/" .var)))
           backup-by-copying t
           version-control t
           delete-old-versions t
           kept-new-versions 5
           kept-old-versions 0
           create-lockfiles nil
           auto-save-include-big-deletions t))

;;; Lines related

(setup display-line-numbers
  ;; Defaults
  (setq-default display-line-numbers-widen t)
  (setq-default display-line-numbers-width 3)

  ;; Preferences
  (:option display-line-numbers-type 'relative
           display-line-numbers-width-start nil
           display-line-numbers-grow-only t)

  ;; Hooks
  (:with-hook (prog-mode-hook text-mode-hook conf-mode-hook)
    (:hook (lambda () (display-line-numbers-mode 1))))
  (:with-hook (dired-mode-hook org-agenda-mode-hook pdf-view-mode-hook)
    (:hook (lambda () (display-line-numbers-mode 0)))))

;;; Scrolling
(setup scrolling
  ;; Enable smooth scroll on Emacs 29
  (unless (version< emacs-version "29")
    (pixel-scroll-precision-mode 1))

  ;; Vertical scroll
  (setq scroll-step 1
        scroll-margin 10
        ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
        ;; for tall lines.
        auto-window-vscroll nil)

  ;; Horizontal scroll
  (setq hscroll-margin 16
        hscroll-step 1
        auto-hscroll-mode t)

  ;; General tweaks

  ;; More performant rapid scrolling over unfontified regions. May cause brief
  ;; spells of inaccurate syntax highlighting right after scrolling, which should
  ;; quickly self-correct.
  (setq fast-but-imprecise-scrolling t)

  ;; Emacs spends too much effort recentering the screen if you scroll the
  ;; cursor more than N lines past window edges (where N is the settings of
  ;; `scroll-conservatively'). This is especially slow in larger files
  ;; during large-scale scrolling commands. If kept over 100, the window is
  ;; never automatically re-centered.
  (setq scroll-conservatively 101
        scroll-preserve-screen-position t
        scroll-preserve-screen-position t))

(setup mouse
  ;; Movement related
  (setq focus-follows-mouse nil
        make-pointer-invisible t
        mouse-autoselect-window nil)

  ;; Scroll
  (setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2)

  ;; Behavior
  (setq mouse-wheel-follow-mouse t)
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-1-click-follows-link t)
  (setq mouse-yank-at-point t)

  (:global "<mouse-2>" clipboard-yank))

(setup elec-pair
  (electric-pair-mode 1))

(setup paren
  (:option show-paren-style 'parenthesis
           show-paren-when-point-in-periphery t
           show-paren-when-point-inside-paren nil)
  (show-paren-mode 1))

(setup selection
  (:option save-interprogram-paste-before-kill t
           kill-do-not-save-duplicates t
           kill-whole-line t
           select-enable-clipboard t
           select-enable-primary nil))

(setup (:pkg goto-last-change)
  (:global "C-z" goto-last-change))

(setup (:require autorevert)
  (:hide-mode auto-revert)
  (:option auto-revert-verbose t
           global-auto-revert-non-file-buffers t)
  (:with-hook after-init-hook
    (:hook global-auto-revert-mode)))

(setup (:pkg diff-hl)
  (:hook-into prog-mode)

  (:with-mode dired-mode
    (:hook diff-hl-dired-mode))

  (:with-after magit
    (:with-hook magit-pre-refresh-hook
      (:hook diff-hl-magit-pre-refresh))
    (:with-hook magit-post-refresh-hook
      (:hook diff-hl-magit-post-refresh))))

(setup long-lines
  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?↩)))

(provide 'init-editing)
;;; init-editing.el ends here
