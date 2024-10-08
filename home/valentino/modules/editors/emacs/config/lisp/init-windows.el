;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(defvar vb/window-configuration nil
  "Current window configuration.  Used by `my/monocle-mode.")

(define-minor-mode vb/monocle-mode
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " [M]"
  :global nil
  (let ((win vb/window-configuration))
    (if (one-window-p)
        (when win
          (set-window-configuration win))
      (setq vb/window-configuration (current-window-configuration))
      (when (window-parameter nil 'window-slot)
        (let ((buf (current-buffer)))
          (other-window 1)
          (switch-to-buffer buf)))
      (delete-other-windows))))
(setup windmove
  ;; Windmove with shift+arrows
  (windmove-default-keybindings)
  (add-hook 'org-shiftup-final-hook    #'windmove-up)
  (add-hook 'org-shiftdown-final-hook  #'windmove-down)
  (add-hook 'org-shiftleft-final-hook  #'windmove-left)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

(setup window
  (setq window-resize-pixelwise nil
        help-window-select t)

  ;; Splitting around
  (setq split-width-threshold 125 ;; 125
        split-height-threshold 0)

  ;; Dividers
  (setq window-divider-default-right-width 6)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 0)

  (:global "C-x <up>"   enlarge-window
           "C-x <down>" shrink-window
           "C-x {"      shrink-window-horizontally
           "C-x }"      enlarge-window-horizontally)
  ;; Additional keymaps using <SPC> prefix
  (setup (:if-feature general)
    (vb/leader-key

      ;; Monocle layout
      "f" '(vb/monocle-mode :which-key "Toogle full screen on current buffer")

      "x" '(delete-other-windows :which-key "Unfocus other window")
      "X" '(delete-window :which-key "Unfocus current window")
      "v" '((lambda () (interactive) (split-window-right) (balance-windows) (other-window 1))
            :which-key "Vertical split")
      "h" '((lambda () (interactive) (split-window-below) (balance-windows) (other-window 1))
            :which-key "Vertical split")
      "<" '(shrink-window-horizontally :which-key "Shrink window width")
      ">" '(enlarge-window-horizontally :which-key "Expand window width")
      "+" '(enlarge-window :which-key "Expand window height")
      "-" '(shrink-window :which-key "Shrink window height"))))

;; Use the official repository from prot (more updated) instead of the emacs-straight one
(setup (:pkg (beframe :type git :host github :repo "protesilaos/beframe"))
  (:option beframe-functions-in-frames '(project-prompt-project-dir)
           beframe-global-buffers '("*scratch*"
                                    "*Messages"
                                    "*Backtrace*"
                                    "*Async-native-compile-log*"
                                    "*straight-byte-compilation*"
                                    "*straight-process*"))
  (:global "C-c b" 'beframe-prefix-map)

  (:with-after consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe--consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe--consult-source))


  (defun vb/beframe-infer-frame-name (frame name)
    "Infer a suitable name for FRAME with given NAME.
See `beframe-rename-frame'."
    (when-let (((frame-list))
               (buffer (car (frame-parameter frame 'buffer-list)))
               (file-name (when (bufferp buffer) (buffer-file-name buffer)))
               (buf-name (buffer-name buffer))
               (dir (with-current-buffer buffer (or (vc-root-dir) default-directory)))
               (projectp (and (bound-and-true-p project--list)
                              (listp project--list)
                              (member (list dir) project--list))))
      (cond
       ((and name (stringp name))
        name)
       ((and projectp buf-name)
        (format "%s" (file-name-nondirectory (directory-file-name dir))))
       ((and (not (minibufferp)) file-name)
        (format "%s %s" buf-name dir))
       ((not (minibufferp))
        buf-name)
       (t
        dir))))

  ;; https://github.com/protesilaos/beframe/issues/3#issuecomment-2320773437
  (advice-add 'beframe-infer-frame-name :override #'vb/beframe-infer-frame-name)

  (beframe-mode 1))

(provide 'init-windows)
;;; init-windows.el ends here
