;; Consult and Vertico, etc setup

(use-package "consult"
  :ensure t
  :bind ("C-f" . consult-line)
        ("C-s" . save-buffer) ; beacause we rebind search
        ("C-x b" . consult-buffer)
        ("C-x g" . consult-ripgrep)
        ("C-:" . :)
  :config (defalias 'errors 'consult-flymake "Search flymake errors")
          (defalias ': 'consult-goto-line "Goto Line number")
          (setq xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref))


(use-package "vertico"
  :ensure t
  :config (vertico-mode)
          (vertico-multiform-mode))

(use-package savehist
  :config (savehist-mode))

(use-package orderless
  :ensure t
  :custom
          (completion-styles '(basic
                               partial-completion
                               orderless
                               flex)))

(use-package "marginalia"
  :ensure t
  :config (marginalia-mode))


(use-package "consult-eglot"
  :ensure 
  :config (defalias 'symbols 'consult-eglot-symbols "Search workspace symbols"))
