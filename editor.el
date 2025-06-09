;;; editor.el --- buffer editor config

(electric-pair-mode electric-quote-mode)
(global-display-line-numbers-mode)
(electric-indent-mode -1)
(delete-selection-mode)
(setq-default tab-width 4)
(setq-default truncate-lines -1)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;;; Corfu + yas
(unless (version<= emacs-version "28.1")
  (use-package "corfu"
    :ensure t
    :custom (corfu-cycle t)
            (corfu-auto t)
            (corfu-auto-prefix 2)
            (corfu-auto-delay 0.0)
            (corfu-quit-at-boundary 'separator)
            (corfu-echo-documentation 0.1)
    :config (global-corfu-mode)
            (corfu-history-mode))

  (unless (display-graphic-p)
    (use-package "corfu-terminal"
      :after corfu
      :ensure t
      :config  (corfu-terminal-mode +1)))

  (use-package "nerd-icons-corfu"
    :after corfu
    :ensure t
    :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(use-package "yasnippet"
  :ensure t
  :config (yas-global-mode 1))


;;; Embark
(unless (version<= emacs-version "28.1")
  (use-package embark
    :ensure t
    :bind (("C-." . embark-act)
           ("C-," . embark-act-noquit)
           ("C-;" . embark-dwim)
           ("C-h B" . embark-bindings))

    :config  (defun embark-act-noquit ()
               "Run action but don't quit the minibuffer afterwards."
               (interactive)
               (let ((embark-quit-after-action nil))
                 (embark-act)))

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  (use-package embark-consult
    :after (embark consult)
    :ensure t
    :demand t
    :hook (embark-collect-mode . consult-preview-at-point-mode)))


;;; Jinx spellcheck
(unless (version<= emacs-version "28.1")
  (use-package jinx
    :ensure t
    :demand t
    :diminish jinx-mode
    :after embark
    :bind(:map embark-symbol-map ("$" . embark-jinx)
               :map embark-general-map ("$" . embark-jinx)
               :map embark-region-map ("$" . embark-jinx))
    :config (global-jinx-mode))

  (use-package embark-jinx
    :after (embark jinx)
    :ensure t
    :vc (:url "https://github.com/ImFstAsFckBoi/embark-jinx")))

(use-package eros
  :ensure t
  :config (eros-mode 1))

;; TODO: figure out
;; (use-package blamer :ensure t)

;; Git integrations
(use-package "diff-hl"
  :ensure t
  :config (global-diff-hl-mode))

;;; General keybinds
(unless (version<= emacs-version "29.0")
  (use-package "beat+"
    :ensure t
    :vc (:url "https://github.com/ImFstAsFckBoi/beatp")
    :bind ("C-w" . beatp-dwim-kill)
          ("M-w" . beatp-dwim-save)
          ("C-d" . beatp-select-around-word-or-next-match)
          ("C-<right>" . beatp-right-to-boundary)
          ("C-<left>" . beatp-left-to-boundary)
          ("C-<delete>" . beatp-delete-right-to-boundary)
          ("C-<backspace>" . beatp-delete-left-to-boundary)
          ("<up>" . beatp-dwim-previous-line)))

;; Set alt up/down to move lines a la vscode
(use-package "move-text"
  :ensure t
  :bind ("M-<down>" . move-text-down)
        ("M-<up>" . move-text-up))


(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-r") 'repeat)
(global-set-key (kbd "C-x a") 'mark-whole-buffer)
(global-set-key (kbd "C-x C-.") 'emoji-search)
(global-set-key (kbd "C-x C-,") 'emoji-insert)
(global-set-key (kbd "C-p") 'search-backward)
(global-set-key (kbd "C-n") 'search-forward)

(use-package "multiple-cursors"
  :ensure t
  :bind  ("M-S-<up>" . 'mc/mark-previous-lines)
         ("M-S-<down>" . 'mc/mark-next-lines))

(global-unset-key (kbd "<mouse-2>"))
(global-unset-key (kbd "<mouse-3>"))
(global-unset-key (kbd "C-q"))


;;; === render whitespace ================================================
(global-whitespace-mode 1)
(diminish 'global-whitespace-mode)
(diminish 'whitespace-mode)

(use-package "indent-guide"
  :ensure t
  :config (indent-guide-global-mode))

(setq-default whitespace-style
              '(face spaces tabs newline space-mark tab-mark newline-mark))

;; Set display styles for space (center dot) tab (|->)
(setq whitespace-display-mappings
      '((space-mark   ?\     [?\u00B7] [?.])
        (tab-mark     ?\t    [?\u21E5 ?\t] [?\u00BB ?\t] [?\\ ?\t])))

; set foreground (characters) to light gray
(set-face-attribute 'whitespace-space nil :foreground "#52494e" :background nil)
(set-face-attribute 'whitespace-tab nil :foreground "#52494e" :background nil)


;; ;; Whitespace color corrections.
;; (use-package "color"
;;   :config
;;   (let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
;;          (ws-color (color-lighten-name "#000000" ws-lighten)))
;;     (custom-set-faces
;;      `(whitespace-newline                ((t (:foreground ,ws-color))))
;;      `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
;;      `(whitespace-space                  ((t (:foreground ,ws-color))))
;;      `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
;;      `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
;;      `(whitespace-tab                    ((t (:foreground ,ws-color))))
;;      `(whitespace-trailing               ((t (:foreground ,ws-color)))))))

