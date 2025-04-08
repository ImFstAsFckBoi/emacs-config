;; Emacs config

;; Early init
(setq custom-file "~/.config/emacs/.custom.el")
(add-to-list 'load-path "~/.config/emacs/elisp/")
(setq backup-directory-alist '(("." . "~/.config/emacs/backups/")))
(setq auto-save-file-name-transforms  `((".*" "~/.config/emacs/saves/" t)))
(setq max-lisp-eval-depth 3200) ; cope and seethe

;;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(when (version<= emacs-version "28.1")
  (defun length= (LIST N)
    (eq (length LIST) N)))

;;; Basic setup 
(recentf-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode 1)
(scroll-bar-mode -1)
(xterm-mouse-mode 1)
(setq-default scroll-step 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default inhibit-startup-message t)

(use-package "diminish" :ensure t)

(unless (display-graphic-p)
  (use-package "kkp"
    :ensure t
    :config (global-kkp-mode +1)))

(use-package "xclip"
  :ensure t
  :config (xclip-mode))


;;; Theme, font & looks
(set-face-attribute 'default nil :height 160)
(set-face-attribute 'mode-line-buffer-id nil :foreground "#ffbcd8")

(defun insert-at (LIST N VAL)
  (let ((trail (nthcdr N LIST)))
    (setf (nthcdr N LIST) (cons VAL trail))))

(insert-at mode-line-format 1 "  🌸 ")
(insert-at mode-line-format 5 "✨ ")

(unless (version<= emacs-version "28.1")
  (use-package "spacious-padding"
	:ensure t
	:init (spacious-padding-mode)
    (setq spacious-padding-widths '( :mode-line-width 2 ))))

;; (use-package "gruber-darker-theme"
;;   :ensure t
;;   :config (load-theme 'gruber-darker t))

(use-package "doom-themes"
  :ensure t
  :config (load-theme 'doom-one t))

(add-to-list 'default-frame-alist `(font . "monospace"))
;; (add-to-list 'default-frame-alist `(font . "ComicShannsMono Nerd Font Mono"))
;; (add-to-list 'default-frame-alist `(font . "Iosevka Nerd Font"))


;;; General keybinds
(global-set-key (kbd "C-x f") 'recentf)
(global-set-key (kbd "C-x z") 'suspend-frame)
(global-set-key (kbd "C-x m") 'switch-to-minibuffer)

(use-package "which-key"
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))


;;; Global things

(use-package "restart-emacs"
  :ensure t)

(use-package "zoxide"
  :ensure t
  :config (defalias 'zi 'zoxide-find-file)
          (defalias 'zi-cd 'zoxide-cd))

(use-package "treemacs"
  :ensure t
  :bind ("C-x C-d" . treemacs))

(use-package "treemacs-magit"
  :after 'treemacs
  :ensure t)

(use-package "golden-ratio" 
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode 1))

(use-package "solaire-mode"
  :ensure t
  :config (solaire-global-mode +1))


;;; Setup webjump

(use-package webjump
  :bind ("C-x w j" . webjump)
  :config (defun webjump-ida-course-page (_)
            (format "https://www.ida.liu.se/~%s/" (upcase (read-string "Course code: "))))
          
          (add-to-list 'webjump-sites '("IDA course page" . webjump-ida-course-page))
          (add-to-list 'webjump-sites '("Hoogλe" . [simple-query 
                                                  "https://hoogle.haskell.org/"
                                                  "https://hoogle.haskell.org/?hoogle="
                                                  ""]))
          (add-to-list 'webjump-sites '("Python docs" .
                                        [simple-query 
                                         "https://docs.python.org/"
                                         "https://docs.python.org/3/search.html?q=" 
                                         ""])))


;;; ✨ hiiii! :3 
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

(defalias 'make 'compile)

(defun insert-list-reverse (list)
  (if (not list) nil
      (delete-char 1)
      (insert-char (string-to-char (car list)))
      (left-char 2)
      (insert-list-reverse (cdr list))))

(defun reverse-chars-region (beg end)
  "Reverse the characters in a region"
  (interactive "r")
  (goto-char (- end 1))
  (insert-list-reverse (string-split (buffer-substring-no-properties beg end) "" t)))

(defun is-on-top ()
  (when (eq (count-windows) 2)
    (< (nth 1 (window-edges (selected-window))) (nth 1 (window-edges (next-window))))))


(defun rotate()
  "Rotate two-window-setups between Left-Right split and Top-Bottom split"
  (interactive)
  (when (eq (count-windows) 2)
    (let ((buffer (window-buffer (next-window)))
          (mknew (if (is-on-top) '(split-window-right) '(split-window-below))))
      (delete-other-windows)
      (eval mknew)
      (set-window-buffer (next-window) buffer))))



;;; === load other files ================================================
(load-file (expand-file-name "lsp-dap.el" user-emacs-directory))
(unless (version<= emacs-version "28.1")
  (load-file (expand-file-name "consult.el" user-emacs-directory)))

(load-file (expand-file-name "editor.el" user-emacs-directory))
(load-file (expand-file-name "languages.el" user-emacs-directory))
(load-file (expand-file-name "tempo.el" user-emacs-directory))

(load custom-file)
