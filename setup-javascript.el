;;; flymake-cursor - show flymake errors in the minibuffer
;;; http://www.emacswiki.org/emacs/flymake-cursor.el
(require-package 'flymake-cursor)
(require 'flymake-cursor)

;;; js2-mode - Major mode for editing Javascript
;;; https://github.com/mooz/js2-mode
(require-package 'js2-mode)
(after-load 'js2-mode
  (rename-modeline "js2-mode" js2-mode "JS2")

  (add-hook 'js2-mode-hook 'bw/turn-on-subword-mode)

  ;; Flymake uses node.js jslint
  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (expand-file-name (file-relative-name
                                          temp-file
                                          (file-name-directory buffer-file-name))))
           (node-bin (concat dotfiles-dir "node_modules/.bin"))
           (exec-path (add-to-list 'exec-path node-bin)))
      (list "jslint" (list "--terse" local-file))))

  (when (load "flymake" t)
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.js\\'" flymake-jslint-init))
    ;; jslint lines look like:
    ;; jslint:25:45:Missing trailing ; character
    (add-to-list 'flymake-err-line-patterns
                 '("^\\(.*\\)\\(([[:digit:]]+)\\):\\(.*\\)$" 1 2 nil 3)))

  (add-hook 'js2-mode-hook 'bw/turn-on-flymake-mode)

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
   js2-strict-missing-semi-warning nil))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'setup-javascript)
