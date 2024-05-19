;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode is certainly the killer feature of Emacs.
;; You can do anything, for example capturing of templates, export, markdown like editing.

;;; Code:

(defun vb/org-mode-setup ()
  "Set important modes for me while editing org documents.

- Setting variable-pitch allows different face definition;
- I prefer visual-line here, instead of truncating lines."
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(setup (:pkg org :type built-in)
  (setq load-path
        (cl-remove-if (lambda (path) (string-match-p "lisp/org\\'" path)) load-path))
  ;; General
  (:option org-adapt-indentation nil
           org-fold-catch-invisible-edits 'smart
           org-cycle-separator-lines 0    ;; Give a more compact and consistent view

           org-directory "~/Dropbox/org/"
           org-auto-align-tags nil

           org-use-property-inheritance t ;; It's convenient to have properties inherited
           org-return-follows-link t      ;; make RET follow links
           org-tags-column 0              ;; place tags directly next to headline text
           org-archive-mark-done nil
           org-startup-folded 'content
           org-insert-heading-respect-content t
           org-read-date-prefer-future 'time
           org-startup-folded t
           org-startup-indented t

           ;; Prettify
           org-ellipsis " ▾"             ;; "…" " ⤵"  " " " " "⤶" " "
           org-hide-leading-stars nil
           org-pretty-entities t
           org-pretty-entities-include-sub-superscripts t
           org-hide-emphasis-markers t
           org-fontify-quote-and-verse-blocks t
           org-list-allow-alphabetical t
           org-highlight-latex-and-related '(native latex)
           org-image-actual-width 500

           ;; Date
           org-display-custom-times t
           org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>")

           ;; Footnotes
           org-footnote-section nil ;; place footnotes locally rather than in its own section
           org-footnote-auto-adjust t ;; renumber footnotes

           ;; Insertion/Yanking
           org-M-RET-may-split-line '((default . t)) ;; don't split line when creating a new headline, list item, or table field
           org-yank-adjusted-subtrees t ;; adjust subtrees to depth when yanked
           org-yank-folded-subtrees t                ;; fold subtrees on yank

           org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
           org-list-indent-offset 1 ;; increase sub-item indentation

           ;; Movement
           org-return-follows-link t ;; make RET follow links
           org-special-ctrl-a/e t    ;; better movement in headers

           ;; Searching
           org-imenu-depth 8   ;; scan to depth 8 w/imenu
           imenu-auto-rescan t ;; make sure imenu refreshes

           ;; Source block settings
           org-src-fontify-natively t         ;; use lang-specific fontification
           org-src-window-setup 'other-window ;; edit source in other window
           org-src-tab-acts-natively t        ;; use lang bindings
           org-confirm-babel-evaluate nil     ;; no confirm evaluation

           ;; Agenda
           ;; Set all org files inside org-directory as agenda-files
           org-agenda-files (directory-files-recursively org-directory "\\.org$")
           org-log-done 'time                  ;; Having the time a item is done sounds convenient
           org-agenda-restore-windows-after-quit t
           org-agenda-skip-unavailable-files t ;; just skip non-reachable files in org-agenda-files
           ;; Taken from https://emacs.stackexchange.com/questions/12517/how-do-i-make-the-timespan-shown-by-org-agenda-start-yesterday
           org-agenda-span 7
           org-agenda-start-day "+0d"
           org-agenda-start-on-weekday nil
           org-agenda-compact-blocks t ;; Make agenda shorter
           org-agenda-skip-deadline-if-done t
           org-agenda-skip-scheduled-if-done t
           org-agenda-skip-scheduled-if-deadline-is-shown t
           org-agenda-skip-timestamp-if-deadline-is-shown t
           org-agenda-skip-archived-trees nil
           org-agenda-include-deadlines t
           org-agenda-dim-blocked-tasks t
           org-agenda-inhibit-startup t ;; ~50x speedup
           org-agenda-current-time-string "← now ─────────────────────────────────────────────────" ;; "ᐊ---------- now"
           org-agenda-time-grid '((daily) () "" "")
           org-agenda-window-frame-fractions '(0.3 . 0.4) ; define min and max height for org-agenda buffers
           org-agenda-hide-tags-regexp ".*" ;; Hide tags
           org-agenda-prefix-format '((agenda . " %?-2i %t ")
                                      (todo . " %i %-12:c")
                                      (tags . " %i ")
                                      (search . " %i %-12:c"))

           org-agenda-category-icon-alist `(("Anime" ,(list (all-the-icons-faicon "home" :height 0.8)) nil nil :ascent center))

           org-tag-alist '(("@Anime"    . ?A)
                           ("@Emacs"    . ?E)
                           ("@Nixos"    . ?N)
                           ("@Movies"   . ?M)
                           ("@Personal" . ?P)
                           ("@Work"     . ?W)
                           ("@TVSeries" . ?T)
                           ("@Uni"      . ?U))

           org-agenda-custom-commands '(("A" "Anime"
                                         tags-todo "@Anime"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header "⚡ Anime")))

                                        ("E" "Emacs"
                                         tags-todo "@Emacs"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header " Emacs")))

                                        ("N" "Nixos"
                                         tags-todo "@Nixos"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header " Nixos")))

                                        ("M" "Movies"
                                         tags-todo "@Movies"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header " Movies")))

                                        ("P" "Personale"
                                         tags-todo "@Personal"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header " Personale")))

                                        ("W" "Work"
                                         tags-todo "@Work"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header " Work")))

                                        ("T" "TVSeries"
                                         tags-todo "@TVSeries"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header "  Serie TV")))

                                        ("U" "Uni"
                                         tags-todo "@Uni"
                                         ((org-agenda-files '("~/Dropbox/org/Agenda.org"))
                                          (org-agenda-overriding-header "  Uni"))))
           org-capture-templates
           '(("A" "Anime" entry (file+headline "~/Dropbox/org/Agenda.org" "Anime")
              "* TODO %? :@Anime:\n%U" :empty-lines 0)
             ("D" "Deadline" entry (file+headline "~/Dropbox/org/Agenda.org" "DEADLINE")
              "* TODO %? %^G \n  DEADLINE: %^T" :empty-lines 0)
             ("E" "Emacs" entry (file+headline "~/Dropbox/org/Agenda.org" "Emacs")
              "* TODO %? :@Emacs:\n%U" :empty-lines 0)
             ("N" "NIXOS" entry (file+headline "~/Dropbox/org/Agenda.org" "Nixos")
              "* TODO %? :@Nixos:\n%U" :empty-lines 0)
             ("M" "MOVIES" entry (file+headline "~/Dropbox/org/Agenda.org" "Movies")
              "* TODO %? :@Movies:\n%U" :empty-lines 0)
             ("P" "Personal" entry (file+headline "~/Dropbox/org/Agenda.org" "Personale")
              "* TODO %? :@Personal:\n%U" :empty-lines 0)

             ("T" "Serie TV" entry (file+headline "~/Dropbox/org/Agenda.org" "Serie TV")
              "* TODO %? :@TVSeries:\n%U" :empty-lines 0)
             ("U" "Uni" entry (file+headline "~/Dropbox/org/Agenda.org" "Uni")
              "* TODO %? :@Uni:\n%U" :empty-lines 0)
             ("W" "Work" entry (file+headline "~/Dropbox/org/Agenda.org" "Work")
              "* TODO %? :@Work:\n%U" :empty-lines 0)
             ("S" "Scheduled TODO" entry (file+headline "~/Dropbox/org/Agenda.org" "SCHEDULED")
              "* TODO %? %^G \nSCHEDULED: %^T\n  %U" :empty-lines 0))

           ;; TODOS
           org-use-fast-todo-selection 'expert ;; don't use popup window for todos
           org-enforce-todo-dependencies t ;; don't set to DONE if children aren’t DONE
           org-enforce-todo-checkbox-dependencies t

           ;; TODO customization
           org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                               (sequence "TODO [/] (T)" "READY(r)" "|" "COMPLETED(c)" "CANC(k@)"))

           ;; Source blocks
           org-hide-block-startup nil
           org-src-preserve-indentation nil
           org-edit-src-content-indentation 0)

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t)))

  (:local-set completion-at-point-functions '(cape-dabbrev cape-file))

  ;; (:hook vb/org-mode-setup)
  (:bind-into org-mode-map
    [tab] 'org-cycle
    [S-tab] 'org-shifttab
    "M-j" 'org-next-visible-heading
    "M-k" 'org-previous-visible-heading
    "M-h" 'org-promote-subtree
    "M-l" 'org-demote-subtree

    "C-j" 'org-move-subtree-down
    "C-k" 'org-move-subtree-up))

;; TODO: Map 'org-agenda-view-mode-dispatch
(setup (:if-feature evil)
  (:bind-into org-agenda-mode-map
    [remap evil-shift-right] 'org-agenda-later
    [remap evil-shift-left] 'org-agenda-earlier
    [remap evil-record-macro] 'org-agenda-quit
    [remap evil-find-char-to] 'org-agenda-todo
    [remap evil-find-char-to-backward] 'org-agenda-show-tags
    [remap evil-replace] 'org-agenda-redo
    [remap evil-ret] 'org-agenda-switch-to))

(setup (:pkg org-appear)
  (:autoload org-appear-mode)
  (:hook-into org-mode)
  (:option org-appear-autoemphasis t
           org-appear-autolinks t
           org-appear-autosubmarkers t))

;; Fontifying `org-mode' buffers to be as pretty as possible is of paramount importance, and Minad's lovely `org-modern' goes a long way in this regard.
(setup (:pkg org-modern)
  (:load-after org)
  (:hook-into org-mode)
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka Nerd Font")
  (:option org-modern-label-border 1
           org-modern-list '((43 . "▶") (45 . "–") (42 . "•"))
           org-modern-hide-stars nil      ;; Compatibility with org-indent
           org-modern-block-fringe nil    ;; Bad
           org-modern-variable-pitch nil
           org-modern-timestamp t
           org-modern-table t
           org-modern-table-vertical 1
           org-modern-table-horizontal 0.2))

(setup (:pkg (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent"))
  (:hook-into org-indent-mode))

(provide 'init-org)
;;; init-org.el ends here
