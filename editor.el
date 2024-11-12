;;; editor.el

(global-display-line-numbers-mode)
(electric-pair-mode electric-quote-mode)
(electric-indent-mode -1)


(use-package "company"
  :diminish company-mode
  :ensure t
  :config (global-company-mode)
          (setq company-idle-delay 0)
          (setq company-minimum-prefix-length 1))

(use-package "yasnippet"
  :ensure t
  :config (yas-global-mode 1))

;; alt up/down to move lines al a vscode
(use-package "move-text"
  :ensure t
  :bind ("M-<down>" . move-text-down)
        ("M-<up>" . move-text-up))

;; general keybinds
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x z") 'suspend-frame)
(global-set-key (kbd "C-/") 'comment-line)


;; tab / indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(defun indent-region-custom(numSpaces)
    (progn
        (setq regionStart (line-beginning-position))
        (setq regionEnd (line-end-position))

        (when (use-region-p)
            (setq regionStart (region-beginning))
            (setq regionEnd (region-end)))
        (save-excursion
            (goto-char regionStart)
            (setq start (line-beginning-position))
            (goto-char regionEnd)
            (setq end (line-end-position))
            (indent-rigidly start end numSpaces)
            (setq deactivate-mark nil))))


(defun untab-region (N)
    (interactive "p")
    (indent-region-custom (- tab-width)))


(defun tab-region (N)
    (interactive "p")
    (if (active-minibuffer-window)
        (minibuffer-complete)
    (if (string= (buffer-name) "*shell*")
        (comint-dynamic-complete)
    (if (use-region-p)
        (indent-region-custom tab-width)
        (tab-to-tab-stop)))))

;; (global-set-key (kbd "<backtab>") 'untab-region)
;; (global-set-key "\t" 'tab-region)


;; Ctrl backspace like vscode
(defun ryanmarcus/backward-kill-word ()
  "Remove all whitespace if the character behind
   the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n\t]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n\t]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

(global-set-key (kbd "C-<backspace>") 'ryanmarcus/backward-kill-word)


;; replace region when pasting
(defun killer-yank ()
  "If region is active, delte it when yanking"
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end)))
  (yank))



(global-set-key (kbd "C-y") 'killer-yank)

(defun vscp/C-d ()
  "Mimic vscode’s C-d keybind"
  (interactive)
  (if (region-active-p)
    (progn (mc/mark-next-like-this (region-beginning)))
    (progn (left-word (mark-word)))))


(use-package "multiple-cursors"
  :ensure t
  :bind ("C-d" . 'vscp/C-d)
    ("C-S-<up>" . 'mc/mark-previous-lines)
    ("C-S-<down>" . 'mc/mark-next-lines))


;; whitespace as dots
(global-whitespace-mode 1)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)
(setq whitespace-display-mappings
  '((space-mark   ?\     [?\u00B7] [?.])
    (tab-mark     ?\t    [?\u21E5 ?\t] [?\u00BB ?\t] [?\\ ?\t])))
;; (set-face-attribute 'whitespace-space nil :background nil :foreground "#52494e")
(set-face-attribute 'whitespace-tab nil :background nil :foreground "#52494e")
(setq-default whitespace-style
              '(face spaces tabs newline space-mark tab-mark newline-mark))

;; unset bullshit
(global-unset-key (kbd "<mouse-2>"))
