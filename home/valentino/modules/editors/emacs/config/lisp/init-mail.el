;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; `Notmuch' is a fast, tag-based email indexer to use with your favorite interface (e.g. Emacs :D).
;; I previously used `mu4e', I didn't really like it though.

;; This code is heavily based on Prot's code.
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-notmuch.el
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-email.el

;;; Code:
(let*
    ((not-much-path
      (file-name-directory (directory-file-name (file-name-directory (executable-find "notmuch")))))
     (not-much-load-path (concat not-much-path "share/emacs/site-lisp/")))
  (add-to-list 'load-path not-much-load-path))

(defgroup vb/notmuch()
  "Extensions for notmuch."
  :group 'notmuch)

(defcustom vb/notmuch-delete-tag "deleted"
  "Tag that applies to mail marked for deletion."
  :type 'string
  :group 'vb/notmuch)

(defcustom vb/notmuch-mark-delete-tags
  `(,(format "+%s" vb/notmuch-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion."
  :type '(repeat string)
  :group 'vb/notmuch)

(defcustom vb/notmuch-mark-archive-tags '( "-deleted" "-inbox" "-unread")
  "List of tags to mark for archive."
  :type '(repeat string)
  :group 'vb/notmuch)

(defcustom vb/notmuch-mark-flag-tags '("+flagged" "-unread")
  "List of tags to mark as important (flagged is a special tag)."
  :type '(repeat string)
  :group 'vb/notmuch)

(defcustom vb/notmuch-mark-spam-tags '("+spam" "-inbox" "-unread")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'vb/notmuch)

;;;; Autoload of commands
(autoload 'notmuch-interactive-region "notmuch")
(autoload 'notmuch-tag-change-list "notmuch")
(autoload 'notmuch-search-next-thread "notmuch")
(autoload 'notmuch-search-tag "notmuch")

(defmacro vb/notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.
Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.
With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags.
This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (when ,tags
       (notmuch-search-tag
        (notmuch-tag-change-list ,tags untag) beg end))
     (when (eq beg end)
       (notmuch-search-next-thread))))

(vb/notmuch-search-tag-thread vb/notmuch-search-delete-thread vb/notmuch-mark-delete-tags)
(vb/notmuch-search-tag-thread vb/notmuch-search-flag-thread vb/notmuch-mark-flag-tags)
(vb/notmuch-search-tag-thread vb/notmuch-search-spam-thread vb/notmuch-mark-spam-tags)

(defmacro vb/notmuch-show-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.
With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
              (notmuch-tag-change-list ,tags untag)))))

(vb/notmuch-show-tag-message vb/notmuch-show-delete-message vb/notmuch-mark-delete-tags)
(vb/notmuch-show-tag-message vb/notmuch-show-flag-message vb/notmuch-mark-flag-tags)
(vb/notmuch-show-tag-message vb/notmuch-show-spam-message vb/notmuch-mark-spam-tags)

(autoload 'notmuch-refresh-this-buffer "notmuch")
(autoload 'notmuch-refresh-all-buffers "notmuch")

(defun vb/notmuch-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'."
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))

(defun vb/lieer-sendmail ()
  "Set the required variables to send a mail through `lieer'.
To improve."
  (let (from (message-fetch-field "from"))
    (when (string= from "mario.liguori.056@gmail.com")
      (setq-local sendmail-program "gmi"
                  message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/.config/mails/gmail")))))


;; Current client for mails
(setup notmuch
  (:autoload notmuch notmuch-mua-new-mail)
  ;; UI
  (:option notmuch-show-logo t
           notmuch-column-control 0.5
           notmuch-hello-auto-refresh t
           notmuch-hello-recent-searches-max 15
           notmuch-hello-thousands-separator "."
           notmuch-show-all-tags-list t
           notmuch-hello-insert-footer t
           notmuch-hello-sections
           '(notmuch-hello-insert-header
             notmuch-hello-insert-saved-searches
             notmuch-hello-insert-search
             notmuch-hello-insert-recent-searches
             notmuch-hello-insert-alltags))
  ;; Search
  (:option notmuch-search-oldest-first nil
           notmuch-show-empty-saved-searches t
           notmuch-search-result-format
           '(("date" . "%12s ")
             ("count" . "%-7s ")
             ("authors" . "%-20s ")
             ("subject" . "%80s ")
             ("tags" . "[%s]"))
           notmuch-tree-result-format
           '(("date" . "%12s  ")
             ("authors" . "%-20s")
             ((("tree" . "%s")
               ("subject" . "%s"))
              . " %-80s ")
             ("tags" . "[%s]"))
           notmuch-search-line-faces
           '(("unread" . notmuch-search-unread-face)
             ("flagged" . notmuch-search-flagged-face)))

  ;; Saved searches
  (:option notmuch-saved-searches
           ;; Personal
           `((:name "📥 inbox (personal)"
                    :query "tag:inbox and tag:personal"
                    :sort-order newest-first
                    :key ,(kbd "p i"))
             (:name "📔 unread (personal)"
                    :query "tag:unread and tag:inbox and tag:personal"
                    :sort-order newest-first
                    :key ,(kbd "p u"))))

  ;; Tags
  (:option notmuch-archive-tags vb/notmuch-mark-archive-tags
           notmuch-message-replied-tags '("+replied")
           notmuch-message-forwarded-tags '("+forwarded")
           notmuch-show-mark-read-tags '("-unread")
           notmuch-draft-tags '("+draft")
           notmuch-draft-folder "drafts"
           notmuch-draft-save-plaintext 'ask)

  ;; Tag formats (with emojis)
  (:option notmuch-tag-formats
           '(("unread" (propertize tag 'face 'notmuch-tag-unread))
             ("flagged" (propertize tag 'face 'notmuch-tag-flagged) ;; Icon is enough
              (concat "🚩")))

           notmuch-tag-deleted-formats
           '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
              (concat "🚫" tag))
             (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
              (concat "🚫" tag)))

           notmuch-tag-added-formats
           '((".*" (notmuch-apply-face tag 'notmuch-tag-added)
              (concat "✏️" tag))))

  ;; Reading
  (:option notmuch-show-relative-dates t
           notmuch-show-all-multipart/alternative-parts nil
           notmuch-show-indent-messages-width 1
           notmuch-show-indent-multipart t
           notmuch-show-part-button-default-action 'notmuch-show-view-part
           notmuch-show-text/html-blocked-images "." ; block everything
           notmuch-wash-wrap-lines-length 120
           notmuch-unthreaded-show-out nil
           notmuch-message-headers '("To" "Cc" "Subject" "Date")
           notmuch-message-headers-visible t)

  (:option notmuch-wash-citation-lines-prefix 3
           notmuch-wash-citation-lines-suffix 3)

  ;; Composition
  (:option notmuch-mua-compose-in 'current-window
           notmuch-mua-hidden-headers nil
           notmuch-address-command 'internal
           notmuch-always-prompt-for-sender t
           notmuch-mua-cite-function 'message-cite-original
           notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never
           notmuch-mua-user-agent-function nil
           notmuch-maildir-use-notmuch-insert t
           notmuch-crypto-process-mime t
           notmuch-crypto-get-keys-asynchronously t
           notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
           (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                   "pi[èe]ce\s+jointe?\\)\\b"))

  ;; Tagging keys
  (:option notmuch-tagging-keys
           `((,(kbd "d") vb/notmuch-mark-delete-tags "⛔ Mark for deletion")
             (,(kbd "a") vb/notmuch-mark-archive-tags "📫 Mark to archive")
             (,(kbd "f") vb/notmuch-mark-flag-tags "🚩 Flag as important")
             (,(kbd "s") vb/notmuch-mark-spam-tags "⚠️ Mark as spam")
             (,(kbd "r") ("-unread") "✅ Mark as read")
             (,(kbd "u") ("+unread") "📔 Mark as unread")))

  ;; Identities
  (:option notmuch-identies '("valentinobocchetti59@gmail.com" "vale.bocchetti@studenti.unina.it")
           notmuch-fcc-dirs '(("valentinobocchetti59@gmail.com" . "gmail/sent +personal +sent")
                              ;; ("vale.bocchetti@studenti.unina.it" . "unina/sent +university +sent")
                              ))

  ;; Other cosmetic formatting
  (add-to-list 'notmuch-tag-formats '("encrypted" (concat tag "🔒")))
  (add-to-list 'notmuch-tag-formats '("attachment" (concat tag "📎")))

  (:with-hook notmuch-mua-send-hook
    (:hook notmuch-mua-attachment-check))

  (:global "C-c m" notmuch
           "C-x m" notmuch-mua-new-mail)
  (:with-map notmuch-search-mode-map
    (:bind
     ;; K
     [remap evil-lookup] #'notmuch-tag-jump
     ;; RET
     [remap evil-ret] #'notmuch-search-show-thread
     ;; /
     [remap evil-search-forward] #'notmuch-search-filter))

  (:with-map notmuch-show-mode-map
    (:bind
     [remap evil-change-whole-line] #'notmuch-show-save-attachments
     ;; H
     [remap evil-window-top] #'notmuch-show-toggle-visibility-headers
     ;; *
     [remap evil-search-word-forward] #'notmuch-show-tag-all
     ;; RET
     [remap evil-ret] #'notmuch-show-toggle-message

     "C-+" #'notmuch-show-add-tag
     "C--" #'notmuch-show-remove-tag
     "C-j" 'notmuch-show-next-message
     "C-k" 'notmuch-show-previous-message
     "M-j" 'notmuch-show-next-thread-show
     "M-k" 'notmuch-show-previous-thread-show)))

(setup (:if-feature evil)
  (evil-define-key 'normal notmuch-common-keymap
    "C" 'notmuch-mua-new-mail
    "cc" 'notmuch-mua-new-mail
    "q" 'notmuch-bury-or-kill-this-buffer
    "s" 'notmuch-search
    "J" 'notmuch-jump-search
    "gr" 'vb/notmuch-refresh-buffer))

(setup sendmail
  (:option send-mail-function 'sendmail-send-it
           mail-specify-envelope-from t
           message-sendmail-envelope-from 'header
           mail-envelope-from 'header)
  (:with-hook message-send-hook (:hook vb/lieer-sendmail)))

(setup message
  (:option message-cite-style 'message-cite-style-gmail
           message-citation-line-function 'message-insert-formatted-citation-line))

(provide 'init-mail)
;;; init-mail.el ends here
