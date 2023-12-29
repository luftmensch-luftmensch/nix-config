;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:
(defgroup vb/faces()
  "Extensions for faces."
  :group 'faces)

(defcustom vb/font-height 140
  "Variable that specifies the font height."
  :type 'integer
  :group 'vb/faces)

(setup (:pkg fontaine)
  (:option x-underline-at-descent-line nil
           use-default-font-for-symbols t)

  (unless (version< emacs-version "28")
	  (setq-default text-scale-remap-header-line t))

  (:option vb/font-height (pcase (system-name)
                            ("atlas" 110)
                            ("kronos" 140)))


  ;; TODO: fix this, the state is not re-stored correctly.
  ;; (:option fontaine-latest-state-file (locate-user-emacs-file "var/fontaine-state.eld"))

  (:option fontaine-presets
		       `(
             ;; Sarasa
             (sarasa
              :default-family "Sarasa Mono Slab SC"
              :default-height ,vb/font-height)
             ;; Victor Mono
             ;; (victor
		         ;;  :default-family "VictorMono Nerd Font"
		         ;;  :default-height ,vb/font-height)
             ))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'sarasa))

  (:with-hook kill-emacs-hook
		(:hook fontaine-store-latest-preset))

  (:with-hook (modus-themes-after-load-theme-hook ef-themes-post-load-hook)
		(:hook fontaine-apply-current-preset)))

(provide 'init-fonts)
;;; init-fonts.el ends here
