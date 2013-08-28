;;; Evil - Emacs vi emulation
(require-package 'evil)

(after-load 'evil
            (setq
             ;; this stops evil from overwriting the cursor color
             evil-default-cursor t
             ;; h/l wrap around to next lines
             evil-cross-lines t
             evil-default-state 'normal)

            ;; use ido to open files
            (define-key evil-ex-map "e " 'ido-find-file)
            (define-key evil-ex-map "b " 'ido-switch-buffer)

            (define-key evil-normal-state-map [escape] 'keyboard-quit)
            (define-key evil-visual-state-map [escape] 'keyboard-quit)
            (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
            (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

            ;; modes to map to different default states
            (dolist (mode-map '((comint-mode . emacs)
                                (term-mode . emacs)
                                (eshell-mode . emacs)
                                (help-mode . emacs)
                                (fundamental-mode . emacs)))
              (evil-set-initial-state `,(car mode-map) `,(cdr mode-map))))

(autoload 'evil-mode "evil" "Emacs Vi emuLation" t)
(global-set-key (kbd "C-z") 'evil-mode)

(require-package 'evil-leader)

(after-load 'evil-leader

            (global-evil-leader-mode)
            ;; keyboard shortcuts
            (evil-leader/set-key
              "a" 'ag-project
              "A" 'ag
              "b" 'ido-switch-buffer
              "c" 'mc/mark-next-like-this
              "C" 'mc/mark-all-like-this
              "e" 'er/expand-region
              "E" 'mc/edit-lines
              "f" 'ido-find-file
              "g" 'magit-status
              "i" 'idomenu
              "j" 'ace-jump-mode
              "k" 'kill-buffer
              "K" 'kill-this-buffer
              "o" 'occur
              "p" 'magit-find-file-completing-read
              "r" 'recentf-ido-find-file
              "s" 'ag-project
              "t" 'bw-open-term
              "T" 'eshell
              "w" 'save-buffer
              "x" 'smex
              )
            )

(provide 'setup-evil)
