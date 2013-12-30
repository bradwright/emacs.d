;;; js2-mode - Major mode for editing Javascript
;;; https://github.com/mooz/js2-mode
(require-package 'js2-mode)
(after-load 'js2-mode
  (rename-modeline "js2-mode" js2-mode "JS2")

  (add-hook 'js2-mode-hook 'bw/turn-on-subword-mode)

  (setq
   ;; highlight everything
   js2-highlight-level 3
   ;; 4 space indent
   js2-basic-offset 4
   ;; idiomatic closing bracket position
   js2-consistent-level-indent-inner-bracket-p t
   ;; allow for multi-line var indenting
   js2-pretty-multiline-decl-indentation-p t
   ;; Don't highlight missing variables in js2-mode: we have jslint for
   ;; that
   js2-highlight-external-variables nil
   ;; jslint shows missing semi-colons
   js2-strict-missing-semi-warning nil)

  ;; Flycheck configuration
  (after-load 'flycheck
    (add-to-list 'exec-path (concat dotfiles-dir "node_modules/.bin/"))

    (flycheck-define-checker
     javascript-jslint-reporter
     "JSLint based checker"
     :command ("jslint" "--terse" source)
     :error-patterns
     ((warning line-start (1+ nonl) ":" line ":" column ":" blank (message) line-end))
     :modes js2-mode))

  (defun bw/turn-on-flycheck-mode-js2 ()
    "Turn on and define JS2 mode checker"
    (flycheck-select-checker 'javascript-jslint-reporter)
    (flycheck-mode 1))

  (add-hook 'js2-mode-hook 'bw/turn-on-flycheck-mode-js2))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'setup-javascript)
