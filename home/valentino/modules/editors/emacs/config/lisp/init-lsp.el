;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode.

;;; Code:

;;
;;; NOTE: These are taken from https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/config.el
(defvar vb/lsp--default-read-process-output-max nil)
(defvar vb/lsp--default-gcmh-high-cons-threshold nil)
(defvar vb/lsp--optimization-init-p nil)

(define-minor-mode vb/lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  :group 'lsp
  (if (not vb/lsp-optimization-mode)
	    (setq-default read-process-output-max vb-lsp--default-read-process-output-max
		                gcmh-high-cons-threshold vb/lsp--default-gcmh-high-cons-threshold
		                vb-lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless vb/lsp--optimization-init-p
	    (setq vb/lsp--default-read-process-output-max (default-value 'read-process-output-max)
	          vb/lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
	    (setq-default read-process-output-max (* 1024 1024))
	    ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
	    ;;        library, so we up the GC threshold to stave off GC-induced
	    ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
	    ;;        so we modify its variables rather than `gc-cons-threshold'
	    ;;        directly.
	    (setq-default gcmh-high-cons-threshold (* 2 vb/lsp--default-gcmh-high-cons-threshold))
	    (gcmh-set-high-threshold)
	    (setq vb/lsp--optimization-init-p t))))

(defcustom vb-lsp-client 'eglot
  "Preferred lsp-client."
  :type 'symbol
  :group 'lsp)

;;; EGLOT

;; Eglot is built-in in Emacs 29+, so this condition doesn't consent the installation
;; if it is already present.
(setup eglot
	(unless (package-installed-p 'eglot)
	  (:pkg eglot))
	(:option eglot-autoshutdown t)

	;; List of modes and servers
	(:when-loaded
	  (add-to-list 'eglot-server-programs '((c++-mode c-mode)
						                              "clangd"
						                              "-j=8"
						                              "--log=error" "--malloc-trim"
						                              "--background-index"
						                              "--clang-tidy"
						                              "--cross-file-rename"
						                              "--completion-style=detailed"
						                              "--pch-storage=memory"
						                              "--header-insertion=never"
						                              "--header-insertion-decorators=0"))
	  (add-to-list 'eglot-server-programs `(nix-mode . ,(eglot-alternatives '(("nil")
										                                                        ("rnix-lsp")))))
	  (add-to-list 'eglot-server-programs '((go-mode) "gopls")))

	(:with-after (cape yasnippet)
		(:local-set completion-at-point-functions (list (cape-super-capf
								                                     #'eglot-completion-at-point
								                                     #'cape-yasnippet
								                                     #'cape-file))))
  ;; WIP: Test cache busting feature of cape
  (:with-after (cape)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (:bind-into eglot-mode-map
    [remap evil-lookup] #'eldoc-doc-buffer)
  ;; (:bind-into eglot-mode-map
  ;;   "gd" 'xref-find-definitions
  ;;   "gD" 'xref-find-definitions-other-window
  ;;   "g5" 'xref-find-definitions-other-frame

  ;;   "gp" 'flymake-goto-prev-error)

	;; Hooks
	(:with-mode (c-mode c++-mode go-mode nix-mode python-mode)
		(:hook eglot-ensure)))

(setup (:if-feature gcmh)
	(:with-hook (eglot-managed-mode-hook)
		(:hook vb/lsp-optimization-mode)))

;; TODO: https://github.com/svaante/dape
;; (setup (:pkg dape))

(setup (:if-feature general)
  ;; Additional keymaps using <SPC> prefix
  (vb/leader-key
    "gd" '(xref-find-definitions :which-key "Goto definition")
    "gD" '(xref-find-definitions-other-window :which-key "Goto definition")
    "g5" '(xref-find-definitions-other-frame :which-key "Goto definition")
    "gr" '(xref-find-references :which-key "Find reference")
    "gn" '(flymake-goto-next-error :which-key "Goto next error")
    "gp" '(flymake-goto-prev-error :which-key "Goto prev error")
    "gR" '(eglot-rename :which-key "Rename variable/function")))
(provide 'init-lsp)
;;; init-lsp.el ends here
