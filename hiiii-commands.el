;; front page commands

(defun open-recent-files ()
  (call-interactively 'consult-recent-file))

(defun open-projects ()
  (call-interactively 'project-switch-project))

(defun open-config ()
  (dired "~/.config/emacs/"))

(defun quit-emacs ()
  (kill-emacs))

(defun save-hiiii ()
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max) hiiii-page-file)
      (message "Saved hiiii!"))))


(defun open-repos ()
  (find-file "~/repos/"))


;; interactive commands
(defun home ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun hiiii()
  (interactive)
  (home))
