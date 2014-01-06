;;; god-mode - Modifier-free keybindings
;;; https://github.com/chrisdone/god-mode

(require-package 'god-mode)

(after-load 'god-mode
  (global-set-key (kbd "<escape>") 'god-local-mode)
  ;; press 'i' to enter normal Emacs mode
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  ;; enable god-mode globally
  (god-mode-all))

(require 'god-mode)

(provide 'setup-god)
