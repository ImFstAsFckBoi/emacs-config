;;; Emacs config

; set early just to be safe
(setq custom-file "~/.config/emacs/.custom.el")

;;; package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.config/emacs/elisp/")
(when (not package-archive-contents)
    (package-refresh-contents))


;;; === basic setup =====================================================
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)

(use-package "diminish" :ensure t)

(use-package "kkp"
  :ensure t
  :config (global-kkp-mode +1))

(use-package "xclip"
  :ensure t
  :config (xclip-mode))

(setq auto-save-file-name-transforms  `((".*" "~/.config/emacs/saves/" t)))
(setq backup-directory-alist '(("." . "~/.config/emacs/backups/")))
(fset 'yes-or-no-p 'y-or-n-p)



;;; === theme ===========================================================
(set-face-attribute 'default nil :height 200)

(use-package "gruber-darker-theme"
  :ensure t
  :config (load-theme 'gruber-darker t))

;; (use-package "monokai-theme"
;;   :ensure t
;;   :config (load-theme 'monokai  t))

;; (use-package "catppuccin-theme"
;;   :ensure t
;;   :config (load-theme 'catppuccin t))

;; (use-package "adwaita-dark-theme"
;;   :ensure t
;;   :config (load-theme 'adwaita-dark t))

;; (set-frame-font "ComicShannsMono Nerd Font Mono" nil t)
;; (add-to-list 'default-frame-alist `(font . "Iosevka"))

(use-package "which-key"
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package "counsel"
  :ensure t
  :diminish ivy-mode
  :bind ("C-f" . swiper)
        ("C-s" . save-buffer)
        ("M-x" . counsel-M-x)
        ("C-p" . counsel-fzf)
        ("M-y" . counsel-yank-pop)
        ("C-x b" . counsel-buffer-or-recentf)
        ("C-O" . counsel-imenu)
        ("C-x C-f" . counsel-find-file)
  :config (ivy-mode))
 
;;; === dired ===========================================================
(use-package "treemacs"
  :ensure t
  :bind ("C-x C-d" . treemacs))

;;; === hiiii! ==========================================================
(load-file "~/.config/emacs/hiiii.el")

;;; === random shit =====================================================
(defun msgwall (msg n)
  "Open a buffer with a wall of repeating phrase"
  (interactive "sMessage: \nnLines: ")
  (switch-to-buffer "msgwall")
  (erase-buffer)
  (dotimes (_ n) (insert (concat msg " "))))

(defun mktemp ()
  "Open a /tmp file in a new buffer"
  (interactive)
  (find-file (string-trim (shell-command-to-string "mktemp"))))


;;; === load other files ================================================
(load-file "~/.config/emacs/editor.el")
(load-file "~/.config/emacs/languages.el")
(load-file "~/.config/emacs/eglot.el")
(load-file "~/.config/emacs/tempo.el")
(load custom-file)



