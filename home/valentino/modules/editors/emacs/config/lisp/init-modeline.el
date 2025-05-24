;;; init-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup vb/modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup vb/modeline-faces nil
  "Faces for my custom modeline."
  :group 'vb/modeline)

(defcustom vb/modeline-string-truncate-length 20
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface vb/modeline-icon '((t (:family "Iosevka Nerd Font Propo")))
  "Face for Unicode Icons using Nerd Font.")

(defface vb/modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-red-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-green-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-yellow-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-blue-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-magenta-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'vb/modeline-faces)

(defface vb/modeline-indicator-cyan-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'vb/modeline-faces)

;;;; Common helper functions

(defun vb/modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (< (window-total-width) split-width-threshold)
       (> (length str) vb/modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun vb/modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
Truncation is done up to `vb/modeline-string-truncate-length'."
  (if (vb/modeline--string-truncate-p str)
      (concat (substring str 0 vb/modeline-string-truncate-length) "...")
    str))

;;;; Keyboard macro indicator

(defvar-local vb/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'vb/modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local vb/modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'vb/modeline-indicator-cyan-bg)))
  "Mode line construct to report the multilingual environment.")

;;;; Input method

(defvar-local vb/modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s" current-input-method-title)
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status

;; What else is there beside remote files?  If
;; nothing, this must be renamed accordingly.
(defvar-local vb/modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'vb/modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Evil state

(defvar evil-state)
(defvar evil-visual-selection)

(defun vb/modeline-evil-state-tag ()
  "Return mode line tag depending on the Evil state."
  (pcase evil-state
    ('normal (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-blue))))
    ('insert (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-yellow))))
    ('visual (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-magenta))))
    ('motion (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-yellow))))
    ('emacs  (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-magenta))))
    ('operator (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-red))))
    ('replace (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-red))))
    ('vb/basic (propertize "   " 'face '(:inherit (vb/modeline-icon vb/modeline-indicator-red))))))

(defvar-local vb/modeline-evil
    '(:eval
      (when (and (mode-line-window-selected-p) (bound-and-true-p evil-mode))
        (vb/modeline-evil-state-tag)))
  "Mode line construct to display the Evil state.")

;;;; Buffer name and modified status

(defun vb/modeline-buffer-identification-face ()
  "Return appropriate face or face list for `vb/modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun vb/modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `vb/modeline-string-truncate'."
  (when-let ((name (buffer-name)))
    (vb/modeline-string-truncate name)))

(defun vb/modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (vb/modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun vb/modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `vb/modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local vb/modeline-buffer-identification
    '(:eval
      (propertize (vb/modeline-buffer-name)
                  'face (vb/modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (vb/modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun vb/modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun vb/modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun vb/modeline-major-mode-help-echo ()
  "Return `help-echo' value for `vb/modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local vb/modeline-major-mode
    (list
     (propertize "%[" 'face 'vb/modeline-indicator-red)
     '(:eval
       (concat
        (vb/modeline-major-mode-indicator)
        " "
        (propertize
         (vb/modeline-string-truncate
          (vb/modeline-major-mode-name))
         'mouse-face 'mode-line-highlight
         'help-echo (vb/modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'vb/modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local vb/modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun vb/modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

;; NOTE 2023-07-27: This is a good idea, but it hardcodes Git, whereas
;; I want a generic VC method.  Granted, I only use Git but I still
;; want it to work as a VC extension.

;; (defun vb/modeline-diffstat (file)
;;   "Return shortened Git diff numstat for FILE."
;;   (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
;;               (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
;;               (added (nth 0 stats))
;;               (deleted (nth 1 stats)))
;;     (cond
;;      ((and (equal added "0") (equal deleted "0"))
;;       "")
;;      ((and (not (equal added "0")) (equal deleted "0"))
;;       (propertize (format "+%s" added) 'face 'shadow))
;;      ((and (equal added "0") (not (equal deleted "0")))
;;       (propertize (format "-%s" deleted) 'face 'shadow))
;;      (t
;;       (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar vb/modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun vb/modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun vb/modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (vb/modeline--vc-help-echo file)
               'local-map vb/modeline-vc-map)
   ;; " "
   ;; (vb/modeline-diffstat file)
   ))

(defun vb/modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (vb/modeline-string-truncate
   (vb/modeline--vc-text file branch face)))

(defvar vb/modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun vb/modeline--vc-get-face (key)
  "Get face from KEY in `vb/modeline--vc-faces'."
  (alist-get key vb/modeline--vc-faces 'up-to-date))

(defun vb/modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (vb/modeline--vc-get-face (vc-state file backend)))

(defvar-local vb/modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (vb/modeline--vc-branch-name file backend))
                  (face (vb/modeline--vc-face file backend)))
        (vb/modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun vb/modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar vb/modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro vb/modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "vb/modeline-flymake-%s" type)) ()
     (when-let ((count (vb/modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    ;; FIXME 2023-07-03: Clicking on the text with
                    ;; this buffer and a single warning present, the
                    ;; diagnostics take up the entire frame.  Why?
                    'local-map vb/modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(vb/modeline-flymake-type error "✖")
(vb/modeline-flymake-type warning "!")
(vb/modeline-flymake-type note "·" success)

(defvar-local vb/modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `vb/modeline-flymake-type'
         '(:eval (vb/modeline-flymake-error))
         '(:eval (vb/modeline-flymake-warning))
         '(:eval (vb/modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local vb/modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;; Miscellaneous

(defvar-local vb/modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;;; Risky local variables

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '(vb/modeline-kbd-macro
                     vb/modeline-narrow
                     vb/modeline-input-method
                     vb/modeline-buffer-status
                     vb/modeline-evil
                     vb/modeline-buffer-identification
                     vb/modeline-major-mode
                     vb/modeline-process
                     vb/modeline-vc-branch
                     vb/modeline-flymake
                     vb/modeline-eglot
                     vb/modeline-align-right
                     vb/modeline-misc-info))
  (put construct 'risky-local-variable t))

;;;; Subtle mode line style

;; (defun vb/modeline-set-faces (_theme)
;;   "Make THEME mode lines subtle."
;;   (let ((subtle (face-foreground 'shadow)))
;;     (custom-set-faces
;;      `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
;;      `(mode-line-active ((t :inherit mode-line :box ,subtle)))
;;      `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle))))))

(defun vb/modeline-set-faces (_theme)
  "Make THEME mode lines subtle."
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     ;; `(mode-line-active ((t :inherit mode-line :box ,subtle)))
     ;; `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle)))
     `(mode-line-active ((t :inherit mode-line :box "#989898")))
     `(mode-line-inactive ((t :background unspecified :foreground "#989898" :box unspecified :overline "#989898"))))))

(defun vb/modeline-unset-faces ()
  "Make window dividers for THEME invisible."
  (custom-set-faces
   `(mode-line (( )))
   `(mode-line-active (( )))
   `(mode-line-inactive (( )))))

(defun vb/modeline--enable-mode ()
  "Enable `vb/modeline-subtle-mode'."
  (vb/modeline-set-faces nil)
  (add-hook 'enable-theme-functions #'vb/modeline-set-faces))

(defun vb/modeline--disable-mode ()
  "Disable `vb/modeline-subtle-mode'."
  (vb/modeline-unset-faces)
  (remove-hook 'enable-theme-functions #'vb/modeline-set-faces))

;;;###autoload
(define-minor-mode vb/modeline-subtle-mode
  "Increase the padding/spacing of frames and windows."
  :global t
  (if vb/modeline-subtle-mode
      (vb/modeline--enable-mode)
    (vb/modeline--disable-mode)))

(defconst RIGHT_PADDING 1)

;; Wait for emacs30 that adds `mode-line-format-right-align'. More info -> https://www.reddit.com/r/emacs/comments/14u6tsl/news_rightaligned_modeline_and_more/
(setup modeline
  ;; Adapted from https://gist.github.com/fhdhsni/990cba7794b4b6918afea94af0b30d66
  (setq vb/modeline-align-left '("%e"
                                 vb/modeline-kbd-macro
                                 vb/modeline-narrow
                                 vb/modeline-buffer-status
                                 vb/modeline-input-method
                                 vb/modeline-evil
                                 vb/modeline-buffer-identification
                                 "  "
                                 vb/modeline-major-mode
                                 vb/modeline-process
                                 "  "
                                 vb/modeline-vc-branch)

        vb/modeline-align-right '("%2 "
                                  vb/modeline-eglot
                                  " "
                                  vb/modeline-flymake
                                  "%2 "
                                  vb/modeline-misc-info
                                  " %p [%l:%c] "))

  (defun vb/mode-line-fill-right (face reserve)
    "Return empty space using FACE and leaving RESERVE space on the right."
    (unless reserve
      (setq reserve 20))
    (when (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
                'face face))

  (defun reserve-middle/right ()
    "Helper function to reserve space for the right section of the mode-line."
    (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

  ;; Currently I don't have anything to display in the middle section
  ;; (defun vb/mode-line-fill-center (face reserve)
  ;;   "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  ;;   (unless reserve
  ;;     (setq reserve 20))
  ;;   (when (and window-system (eq 'right (get-scroll-bar-mode)))
  ;;     (setq reserve (- reserve 3)))
  ;;   (propertize " "
  ;;               'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
  ;;                                              (.5 . left-margin))))
  ;;               'face face))
  ;; (defun reserve-left/middle ()
  ;;   "Helper function to reserve space for the middle section of the mode-line."
  ;;   (/ (length (format-mode-line mode-line-align-middle)) 2))


  (setq-default mode-line-format
                (list
                 vb/modeline-align-left
                 '(:eval (vb/mode-line-fill-right 'mode-line (+ RIGHT_PADDING (length (format-mode-line vb/modeline-align-right)))))
                 vb/modeline-align-right))
  (vb/modeline-subtle-mode 1))


(provide 'init-modeline)

;;; init-modeline.el ends here
