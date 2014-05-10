;;; enh-ruby-mode - better Ruby semantic parsing
;;; https://github.com/zenspider/enhanced-ruby-mode
(require-package 'enh-ruby-mode)
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(dolist (spec '(("\\.rb$" . enh-ruby-mode)
                ("[vV]agrantfile$" . enh-ruby-mode)
                ("[gG]emfile$" . enh-ruby-mode)
                ("[pP]uppetfile$" . enh-ruby-mode)
                ("\\.rake$" . enh-ruby-mode)
                ("\\.rabl$" . enh-ruby-mode)
                ("[cC]apfile$" . enh-ruby-mode)
                ("\\.gemspec$" . enh-ruby-mode)
                ("\\.builder$" . enh-ruby-mode)))
  (add-to-list 'auto-mode-alist spec))

(setq enh-ruby-use-encoding-map nil
      ;; don't deep indent arrays and hashes
      enh-ruby-deep-indent-paren nil)

(defun bw/enh-ruby-mode-faces ()
  "Lazily set faces"
  (set-face-attribute 'erm-syn-errline nil :box nil)
  (set-face-attribute 'erm-syn-warnline nil :box nil)
  (set-face-attribute 'enh-ruby-op-face nil :foreground nil :inherit 'default)
  (set-face-attribute 'enh-ruby-string-delimiter-face nil :foreground "#dc322f" :background nil)
  (set-face-attribute 'enh-ruby-regexp-delimiter-face nil :foreground "#dc322f" :background nil)
  (set-face-attribute 'enh-ruby-heredoc-delimiter-face nil :foreground "#dc322f" :background nil))

(add-hook 'enh-ruby-mode-hook 'bw/turn-on-subword-mode)
(add-hook 'enh-ruby-mode-hook 'bw/enh-ruby-mode-faces)

(provide 'setup-ruby)
