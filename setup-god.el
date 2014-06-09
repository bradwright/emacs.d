;;; god-mode - Modifier-free keybindings
;;; https://github.com/chrisdone/god-mode

(require-package 'god-mode)

(defun bw/god-mode-change-cursor ()
  "Changes cursor type if god-mode is enabled"
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      original-cursor-type)))

(after-load 'god-mode
  ;; (global-set-key (kbd "<escape>") 'god-local-mode)
  ;; ;; press 'i' to enter normal Emacs mode
  ;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  ;; don't enable god-mode in terminals
  (add-to-list 'god-exempt-major-modes 'term-mode)
  (setq original-cursor-type cursor-type)
  (add-hook 'god-mode-enabled-hook 'bw/god-mode-change-cursor)
  (add-hook 'god-mode-disabled-hook 'bw/god-mode-change-cursor))

(require 'god-mode)

(provide 'setup-god)
