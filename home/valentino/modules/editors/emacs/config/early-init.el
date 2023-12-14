;;; early-init.el --- Emacs early init -*- lexical-binding: t -*-
;;

;; Author: luftmensch-luftmensch

;; This file is an early-init file for Emacs (introduced with Emacs 27).
;; It will be executed before init.el when emacs is loaded.

;; -------------------------------------------------------------------------------- ;;
;;   This file was auto-tangled from an orgmode file. (C)  luftmensch-luftmensch    ;;
;; -------------------------------------------------------------------------------- ;;

;; This file IS NOT intended to be edited!

;;; Commentary:

;; Early init file has been introduced in Emacs 27, it is a file loaded
;; before GUI is initialized, so unwanted elements are here.
;; Example: scroll-bars, fringes, menu-bar, tool-bar.

;;; Code:

;; Restore things after init
(defvar +emacs--startup-restore-alist nil
  "Variables and values to restore after init.")

;; Speed up init
(add-hook 'emacs-startup-hook
          (defun emacs-startup@restore-values ()
            "Restore values set during init.
This applies values in `+emacs--startup-restore-alist'."
            (dolist (a +emacs--startup-restore-alist)
              (set (car a) (cdr a)))))

(defun +set-during-startup (variable value &optional restore)
  "Set VARIABLE to VALUE during startup, but restore to RESTORE.
If RESTORE is nil or not passed, save the original value and
restore that."
  (unless after-init-time
    (setf (alist-get variable +emacs--startup-restore-alist)
          (or restore (symbol-value variable)))
    (set-default variable value)))

;;; Define a directory and an expanding function
(defmacro +define-dir (name directory &optional docstring inhibit-mkdir)
  "Define a variable and function NAME expanding to DIRECTORY.
DOCSTRING is applied to the variable.  Ensure DIRECTORY exists in
the filesystem, unless INHIBIT-MKDIR is non-nil."
  (declare (indent 2)
           (doc-string 3))
  (unless inhibit-mkdir
    (make-directory (eval directory) :parents))
  `(progn
     (defvar ,name ,directory
       ,(concat docstring (when docstring "\n")
                "Defined by `/define-dir'."))
     (defun ,name (file &optional mkdir)
       ,(concat "Expand FILE relative to variable `" (symbol-name name) "'.\n"
                "If MKDIR is non-nil, the directory is created.\n"
                "Defined by `/define-dir'.")
       (let ((file-name (expand-file-name (convert-standard-filename file)
                                          ,name)))
         (when mkdir
           (make-directory (file-name-directory file-name) :parents))
         file-name))))

(+define-dir .etc (locate-user-emacs-file ".etc")
  "Directory for all of Emacs's various files.
See `no-littering' for examples.")

(+define-dir .var (locate-user-emacs-file "var")
  "Directory for all of Emacs's various files.
See `no-littering' for examples.")

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'.

;; Garbage collection
(+set-during-startup 'gc-cons-threshold most-positive-fixnum)

(add-hook 'minibuffer-setup-hook (defun garbage-collect@minibuffer-enter ()
                                   (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (defun garbage-collect@minibuffer-exit ()
                                  (setq gc-cons-threshold 800000)))

;; Debug during init
(unless (eq debug-on-error 'startup)
  (+set-during-startup 'debug-on-error 'init))

;; (setq gc-cons-threshold  most-positive-fixnum)

;; Add load-path for submodules
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(push (locate-user-emacs-file "lisp") load-path)

;; Set a better directory to store the native comp cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
;; (when (and (fboundp 'native-comp-available-p)
;;            (native-comp-available-p))
;;   (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; From DOOM
;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation nil)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil))

;; Another trick from DOOM
(unless (or (daemonp)
            noninteractive
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun doom-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

;; From DOOM
;;
;; NOTE: In DOOM these are defined in another file, not in early init, that's horrible because
;; starting a client where this settings are defined later causes a little flash at startup (before redisplay)
;; where menu-bar is present.
;;
;; Not calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because they do extra and unnecessary work that can be more
;; concisely and efficiently expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      column-number-mode t
      fringe-mode 10)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	        (lambda ()
	          (message "Emacs loaded %d packages in %s with %d garbage collections."
		                 (hash-table-count straight--profile-cache)
		                 (format "%.2f seconds"
			                       (float-time
			                        (time-subtract after-init-time before-init-time)))
		                 gcs-done)))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Configure and bootstrap `straight.el'
(setq straight-repository-branch "develop"
      straight-base-dir .etc
      straight-check-for-modifications '(check-on-save find-when-checking)
      ;; used for :fork so don't need to specify settings
      straight-host-usernames '((github . "lufthmensch-luftmensch")
                                (gitlab . "lufthmensch-luftmensch"))
      straight-profiles `((nil . ,(expand-file-name "straight/versions/lock.el" user-emacs-directory))))

(defvar bootstrap-version)
(let ((bootstrap-file
       ;; (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Additional post-setup of `straight.el'
;; Early-loaded packages -- those that, for some reason or another,
;; need to be ensured to be loaded first.
(require 'straight-x)

;;; Appendix

;; Get rid of a dumb alias.  straight-ಠ_ಠ-mode really slows down all
;; minibuffer completion functions.  Since it's a (rarely-used, even)
;; alias anyway, I just define it back to nil.  By the way, the alias
(defalias 'straight-ಠ_ಠ-mode nil)

;;; early-init.el ends here
