;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(setup flymake
  ;; Dumb `flymake' made me crash for this -> Broken on emacs 30
  ;; (add-to-list 'elisp-flymake-byte-compile-load-path load-path)

  (:option flymake-fringe-indicator-position 'left-fringe
           flymake-suppress-zero-counters t
           flymake-start-on-flymake-mode t
           flymake-no-changes-timeout 0.3
           flymake-start-on-save-buffer t
           flymake-proc-compilation-prevents-syntax-check t
           flymake-wrap-around nil)

  (:option flymake-mode-line-format
           '("" flymake-mode-line-exception flymake-mode-line-counters))

  (:option flymake-mode-line-counter-format
           '(" " flymake-mode-line-error-counter
             flymake-mode-line-warning-counter
             flymake-mode-line-note-counter ""))

  (:with-map ctl-x-x-map
    (:bind "m" #'flymake-mode))

  (:bind "C-c ! s" #'flymake-start
         "C-c ! b" #'flymake-show-buffer-diagnostics
         "C-c ! f" #'flymake-show-project-diagnostics
         "C-c ! n" #'flymake-goto-next-error
         "C-c ! p" #'flymake-goto-prev-error)
  (:with-map flymake-diagnostics-buffer-mode-map
    (:bind [remap evil-record-macro] #'quit-window))

  (:hook-into prog-mode text-mode))

;; From Purcell
(setup (:pkg flymake-flycheck)
  (:with-after flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))

    (:with-mode flymake-mode
      (:local-set flymake-diagnostic-functions
                  (append flymake-diagnostic-functions
                          (flymake-flycheck-all-chained-diagnostic-functions))))))

;; Enchanted Spell Checker
(setup (:require jinx)
  (unless (vb/using-nix-p)
    (:pkg jinx))
  (:with-mode text-mode
    (:hook #'jinx-mode)))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
