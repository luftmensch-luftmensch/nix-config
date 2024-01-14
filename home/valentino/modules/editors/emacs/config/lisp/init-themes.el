;;; init-themes.el --- Themes -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of `modus-themes' and `ef-themes', high accessibility themes by Protesilaos.

;;; Code:

(setup (:pkg modus-themes)
  ;; Preferences
  (:option modus-themes-org-blocks nil ;; 'gray-background
           modus-themes-mixed-fonts nil
           modus-themes-variable-pitch-ui nil
           modus-themes-italic-constructs t
           modus-themes-bold-constructs t)

  ;; Custom overrides
  (:option modus-themes-common-palette-overrides
           '((cursor magenta-cooler)
             ;; Modeline
             ;; (bg-mode-line-active bg-blue-subtle)
             (fg-mode-line-active fg-main)
             ;; borderless mode line
             (border-mode-line-active unspecified) ;; blue-intense
             (border-mode-line-inactive unspecified)

             ;; Region
             (bg-region bg-lavender)
             (fg-region unspecified)
             ;; Mouse Hovers
             (bg-hover bg-green-subtle)
             ;; Fringe invisible
             (fringe unspecified)
             ;; Inline code in prose (markup)
             (prose-block fg-dim)
             (prose-code green-cooler)
             (prose-done green)
             (prose-macro magenta-cooler)
             (prose-metadata fg-dim)
             (prose-metadata-value fg-alt)
             (prose-table fg-alt)
             (prose-tag magenta-faint)
             (prose-todo red)
             (prose-verbatim magenta-warmer)
             ;; Syntax
             (comment yellow-cooler)
             (string green-warmer)
             ;; Checkers
             (underline-err red-faint)
             (underline-warning yellow-faint)
             (underline-note cyan-faint)
             ;; Links - No underlines
             (underline-link unspecified)
             (underline-link-visited unspecified)
             (underline-link-symbolic unspecified)
             ;; Box buttons
             (bg-button-active bg-main)
             (fg-button-active fg-main)
             (bg-button-inactive bg-inactive)
             (fg-button-inactive "gray50")
             ;; Prompts
             (fg-prompt cyan)
             (bg-prompt bg-cyan-nuanced)
             ;; Completion
             (fg-completion-match-0 fg-main)
             (fg-completion-match-1 fg-main)
             (fg-completion-match-2 fg-main)
             (fg-completion-match-3 fg-main)
             (bg-completion-match-0 bg-blue-subtle)
             (bg-completion-match-1 bg-yellow-subtle)
             (bg-completion-match-2 bg-cyan-subtle)
             (bg-completion-match-3 bg-red-subtle)
             ;; Mail citations
             (mail-cite-0 blue)
             (mail-cite-1 yellow)
             (mail-cite-2 green)
             (mail-cite-3 magenta)
             (mail-part magenta-cooler)
             (mail-recipient cyan)
             (mail-subject red-warmer)
             (mail-other cyan-cooler)
             ;; Line numbers
             (fg-line-number-inactive "gray50")
             (fg-line-number-active fg-main)
             (bg-line-number-inactive unspecified)
             (bg-line-number-active unspecified)
             ;; Apply more colorful foreground to some headings (headings 0-8).
             ;; Level 0 is for Org #+title and related.
             (fg-heading-1 blue-warmer)
             (fg-heading-2 yellow-cooler)
             (fg-heading-3 cyan-cooler)
             ;; Make the Org agenda use alternative and varied colors.
             ;; default value (for timestamps and more)
             (date-common cyan)
             (date-deadline red-warmer)
             (date-event magenta-warmer)
             ;; for M-x calendar
             (date-holiday blue)
             (date-now yellow-warmer)
             (date-scheduled magenta-cooler)
             (date-weekday cyan-cooler)
             (date-weekend blue-faint)))

  (modus-themes-select 'modus-operandi))

(setup (:pkg ef-themes)

  (:option
   ef-themes-to-toggle '(ef-dark ef-frost)

   ef-themes-mixed-fonts nil ;; enable inheritance from ‘fixed-pitch’ in some faces
   ;; Heading styles
   ;; ef-themes-headings '((0 . (variable-pitch light 1.9))
   ;;                      (1 . (variable-pitch light 1.8))
   ;;                      (2 . (variable-pitch regular 1.7))
   ;;                      (3 . (variable-pitch regular 1.6))
   ;;                      (4 . (variable-pitch regular 1.5))
   ;;                      ;; absence of weight means `bold'
   ;;                      (5 . (variable-pitch 1.4))
   ;;                      (6 . (variable-pitch 1.3))
   ;;                      (7 . (variable-pitch 1.2))
   ;;                      (t . (variable-pitch 1.1)))

   ;; Use proportional fonts (‘variable-pitch’) in UI elements.
   ;; This includes the mode line, header line, tab bar, and tab line.
   ;; ef-themes-variable-pitch-ui t
   ;; Control the appearance of the ‘region’ face.
   ef-themes-region '(intense no-extend neutral)))

;; I set circadian in the configuration of my themes
(setup (:pkg circadian)
  (:load-after modus-themes)
  (:option circadian-themes '(("8:00" . modus-vivendi)
                              ("20:00" . modus-vivendi)))
  (circadian-setup))

(provide 'init-themes)
;;; init-themes.el ends here
