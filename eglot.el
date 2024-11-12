;; eglot-lsp stuff

(use-package "eldoc-box"
  :ensure t
  :hook (eldoc-mode . (lambda ()
                        (if (display-graphic-p)
                            (eldoc-box-hover-at-point-mode))))
  :diminish eldoc-box-hover-at-point-mode)

(use-package "eglot"
  :ensure t
  :bind ("<f2>" . eglot-rename))
