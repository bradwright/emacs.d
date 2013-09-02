;;; eproject - lightweight hooks around 'projects'
;;; https://github.com/jrockway/eproject
(after-load 'eproject
  ;; hide minor-mode
  (diminish 'eproject-mode)

  (defun bw/eproject-find-files ()
    "If we're in a Git project, use git ls-files to look up the
files, because it won't try to open any .gitignored files."
    (interactive)
    (if (member (eproject-type) '(generic-git))
        (magit-find-file-completing-read)
      (eproject-find-file)))

  (defun bw/eproject-ido-switch-buffers ()
    "Like ido-switch-buffer, but for the current eproject."
    (interactive)
    (if (not (eproject-root))
        (error "No active project was found")
      (switch-to-buffer
       (ido-completing-read
        (concat (eproject-name) " buffers: ")
        (mapcar #'buffer-name (--eproject-buffers))))))

  (defun --eproject-buffers ()
    (when (eproject-root)
      (cdr (assoc (eproject-root) (eproject--project-buffers)))))

  ;; use ido where available
  (setq eproject-completing-read-function 'eproject--ido-completing-read)

  ;; use my own functions to clobber eproject's methods
  (after-load 'eproject-extras
    (define-key (current-global-map) [remap eproject-switch-to-buffer] 'bw/eproject-ido-switch-buffers)
    (define-key (current-global-map) [remap eproject-find-file] 'bw/eproject-find-files))

  (when (eq system-type 'darwin)
    (global-set-key (kbd "s-b") 'bw/eproject-ido-switch-buffers)))

(require-package 'eproject)
(require 'eproject)

(provide 'setup-eproject)
