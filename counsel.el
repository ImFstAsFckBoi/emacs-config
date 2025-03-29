(use-package "counsel"
  :ensure t
  :diminish ivy-mode
  :bind ("C-f" . swiper)
        ("C-s" . save-buffer) ; beacause we rebind search
        ("C-x b" . counsel-buffer-or-recentf)
        ("C-p" . counsel-fzf)
        ("M-x" . counsel-M-x)
        ("M-y" . counsel-yank-pop)
        ("C-O" . counsel-imenu)
        ("C-x C-f" . counsel-find-file)
  :config (ivy-mode))
