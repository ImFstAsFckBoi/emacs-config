;; lsp stuff

(use-package "lsp-mode" :ensure t)
(use-package "lsp-ui"
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :ensure t :commands
  lsp-ivy-workspace-symbol)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-lens-enable t)
(setq lsp-modeline-diagnostics-enable t)
; (setq lsp-keymap-prefix "M-l")
