; eglot-lsp and dape-dap stuff

(use-package "eldoc-box"
  :ensure t
  :diminish eldoc-box-hover-at-point-mode
  :hook (eldoc-mode . (lambda ()
                        (if (display-graphic-p)
                            (eldoc-box-hover-at-point-mode)))))

;; (use-package sideline :ensure t
;;   :init (setq sideline-backends-right '(sideline-eglot sideline-flymake)))

;; (use-package sideline-flymake
;;   :ensure t
;;   :hook (flymake-mode . sideline-mode)
;;   :init (setq sideline-flymake-display-mode 'point))

;; (use-package sideline-eglot
;;   :ensure t)

(use-package "eglot"
  :ensure t
  :bind ("<f2>" . eglot-rename)
  :hook ( eglot-mode . sideline-mode))


(add-hook 'eglot-managed-mode-hook #'imenu-add-menubar-index)
(setq imenu-auto-rescan t)

(when (executable-find "emacs-lsp-booster")
  (use-package "eglot-booster"
    :ensure t
    :vc (:url "https://github.com/jdtsmith/eglot-booster")
    :after  eglot
    :config (eglot-booster-mode)))


;; dape debugger
(unless (version<= emacs-version "29.1")
  (use-package "dape"
    :ensure t
    :hook (kill-emacs . dape-breakpoint-save)
          (after-init . dape-breakpoint-load)
    :bind ("C-x C-a <down>" . dape-next)
          ("C-x C-a <right>" . dape-step-in)
          ("C-x C-a <left>" . dape-step-out)
    :config (setq dape-inlay-hints t)
            (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)))

