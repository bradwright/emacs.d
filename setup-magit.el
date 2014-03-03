;; Magit - Git interface
;; https://github.com/magit/magit

(autoload 'magit-grep "magit" "Grep for files" t)

;; key
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c f") 'magit-grep)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-G") 'magit-status))

;; we no longer need vc-git
(delete 'Git vc-handled-backends)

(defun magit-maybe-commit (&optional show-options)
  "Runs magit-commit unless prefix is passed"
  (interactive "P")
  (if show-options
      (magit-key-mode-popup-committing)
    (magit-commit)))

;; magit settings
(setq
 ;; use ido to look for branches
 magit-completing-read-function 'magit-ido-completing-read
 ;; don't put "origin-" in front of new branch names by default
 magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
 ;; open magit status in same window as current buffer
 magit-status-buffer-switch-function 'switch-to-buffer
 ;; highlight word/letter changes in hunk diffs
 magit-diff-refine-hunk t
 ;; ask me if I want to include a revision when rewriting
 magit-rewrite-inclusive 'ask
 ;; ask me to save buffers
 magit-save-some-buffers t
 ;; pop the process buffer if we're taking a while to complete
 magit-process-popup-time 10
 ;; ask me if I want a tracking upstream
 magit-set-upstream-on-push t
 )

(require-package 'magit)

;;; fullframe - programatically fullscreen things
;;; https://github.com/tomterl/fullframe
(require-package 'fullframe)
(fullframe magit-status magit-mode-quit-window nil)

(provide 'setup-magit)
