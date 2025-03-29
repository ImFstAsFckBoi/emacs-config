;;; language-modes

(use-package treesit-auto
  :ensure t
  :config (treesit-auto-add-to-auto-mode-alist 'all)
          (global-treesit-auto-mode))

;; (use-package "flex")       ; (f)lex scanner generator language mode
;; (use-package "bison-mode") ; bison / yacc parser generator language mode

(defun setup-hide-ifdef ()
  (hide-ifdef-mode t)
  (hide-ifdef-toggle-shadowing)
  (hide-ifdefs))

(add-hook 'c-ts-mode-hook 'setup-hide-ifdef)
(add-hook 'c++-ts-mode 'setup-hide-ifdef)


(use-package markdown-mode
  :ensure t
  :config (setq markdown-command "cmark"))
