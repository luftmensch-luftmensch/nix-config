;;; init-reading.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Documents & rdss feeds

;;; Code:

(setup (:pkg pdf-tools)
  (:option display-buffer-alist '(("^\\*outline"
                                   display-buffer-in-side-window
                                   (side . left)
                                   (window-width . 0.20)
                                   (inhibit-switch-frame . t)))
           pdf-tools-installer-os "nixos")


  (:bind-into pdf-view-mode-map
    "C-+" #'pdf-view-enlarge
    "C--" #'pdf-view-shrink
    "C-j" #'pdf-view-next-page
    "C-k" #'pdf-view-previous-page
    [remap evil-open-below] #'pdf-outline
    [remap evil-window-top] #'pdf-view-fit-height-to-window
    [remap evil-record-macro] #'quit-window
    [remap evil-execute-last-recorded-macro] #'kill-this-buffer
    [remap evil-forward-section-begin] #'pdf-view-next-page-command
    [remap evil-backward-section-begin] #'pdf-view-previous-page-command)

  (:with-mode pdf-view-mode
    (:file-match "\\.[pP][dD][fF]\\'"))

  (pdf-tools-install :no-query))
(defun open-pdf ()
  "Open the corresponding pdf based on the current org file opened."
  (interactive)
  (if (or (eq (length buffer-file-name) 0) (eq (file-exists-p (concat (file-name-base (buffer-file-name)) ".pdf")) nil))
      (message "Please select a valid file")
    (call-process-shell-command (concat "zathura " (file-name-base (buffer-file-name)) ".pdf > /dev/null 2>&1 & disown"))))

(setup (:pkg saveplace-pdf-view)
  (:load-after pdf-tools))

(setup (:pkg elfeed)
  (:global "C-c e l" elfeed)
  (:option elfeed-feeds (quote
                         (;; GNU/Linux related
                          ("https://www.reddit.com/r/linux.rss" linux)
                          ("https://www.reddit.com/r/linuxmemes.rss" linux linux-memes)
                          ("https://www.linuxserver.io/blog.rss" linux linux-server)

			                    ;; Nixos related
			                    ("https://www.reddit.com/r/nixos.rss"         nixos)
			                    ("https://christine.website/blog.rss"         nixos Xe)

			                    ;; Arch related
			                    ("https://www.reddit.com/r/archlinux.rss" arch)

			                    ;; Programming languages related
			                    ("https://nullprogram.com/feed/"                programming nullprogram)
			                    ("https://www.reddit.com/r/golang.rss"          programming golang)
			                    ("https://bitfieldconsulting.com/golang?format=rss" programming golang)

			                    ("https://www.reddit.com/r/C_Programming.rss"   programming C)
			                    ("https://www.reddit.com/r/ProgrammerHumor.rss" programming ProgrammerHumor)

			                    ;; Emacs related
			                    ("https://www.reddit.com/r/emacs.rss"    emacs)
			                    ("https://www.reddit.com/r/orgmode.rss"  emacs orgmode)
			                    ("https://planet.emacslife.com/atom.xml" emacs emacslife)

			                    ;; Latex related
			                    ("https://www.reddit.com/r/LaTeX.rss" latex)

			                    ;; Mobile related
                          ("https://www.reddit.com/r/androiddev.rss" android android-dev)
			                    ("https://www.reddit.com/r/fdroid.rss"     android fdroid)
			                    ("https://www.reddit.com/r/FlutterDev.rss" android flutter)

			                    ;; Miscellaneous
			                    ("https://www.rousette.org.uk/archives/index.xml" geekoides)
			                    ("https://www.bytelab.codes/rss/" bytelab)))

           elfeed-search-date-format '("%d-%m-%Y" 10 :left)
           elfeed-db-directory (expand-file-name "elfeed/" .var) ;; "~/.config/emacs/elfeed"
	         elfeed-search-filter "@5-days-ago +unread")
  (:bind-into elfeed-search-mode-map
    [remap evil-ret] 'elfeed-search-show-entry
    [remap evil-goto-char] 'elfeed-search-browse-url
    ;; filter
    [remap evil-change-whole-line] #'elfeed-search-set-filter
    [remap evil-substitute] #'elfeed-search-live-filter
    [remap evil-change] #'elfeed-search-clear-filter
    [remap evil-record-macro] #'elfeed-search-quit-window)

  (:bind-into elfeed-show-mode-map
    [remap elfeed-search-browse-url]  #'elfeed-show-visit
    [remap evil-goto-char] 'elfeed-show-visit
    [remap evil-record-macro] #'elfeed-kill-buffer
    "C-j" 'elfeed-show-next
    "C-k" 'elfeed-show-prev))

(setup (:if-feature evil)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "+") 'elfeed-search-tag-all
    (kbd "-")'elfeed-search-untag-all
    (kbd "u") 'elfeed-search-untag-all-unread
    (kbd "U") 'elfeed-search-tag-all-unread

    (kbd "gr") 'elfeed-update))


(provide 'init-reading)
;;; init-reading.el ends here
