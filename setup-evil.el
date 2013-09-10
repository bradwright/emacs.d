;;; evil-leader - ViM leader key for evil-mode
;;; https://github.com/cofi/evil-leader
(require-package 'evil-leader)

(global-evil-leader-mode 1)
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
  "G" 'magit-blame-mode
  "i" 'idomenu
  "j" 'ace-jump-mode
  "J" 'ace-jump-word-mode
  "k" 'kill-buffer
  "K" 'kill-this-buffer
  "l" 'linum-mode
  "o" 'occur
  "O" 'browse-url
  "p" 'magit-find-file-completing-read
  "P" 'popwin:popup-last-buffer
  "r" 'bw/recentf-ido-find-file
  "s" 'ag-project
  "t" 'bw/open-term
  "T" 'eshell
  "w" 'save-buffer
  "x" 'smex
  "y" 'bury-buffer)

;;; Evil - Emacs vi emulation
;;; http://gitorious.org/evil/pages/Home
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

  ;; use <space> to activate ace-jump-mode
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

  ;; modes to map to different default states
  (dolist (mode-map '((comint-mode . emacs)
                      (term-mode . emacs)
                      (eshell-mode . emacs)
                      (help-mode . emacs)
                      (git-commit-mode . insert)
                      (fundamental-mode . emacs)
                      (ag-mode . emacs)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map))))

(autoload 'evil-mode "evil" "Emacs Vi emuLation" t)
(global-set-key (kbd "C-z") 'evil-mode)

;;; linum-relative - makes Evil behave like:
;;; https://github.com/myusuf3/numbers.vim
;;; https://github.com/coldnew/linum-relative
(require-package 'linum-relative)
(after-load 'linum-relative
  (defun bw/linum-non-relative (line-number)
    "Linum formatter that copies the format"
    (propertize (format linum-relative-format line-number)
                'face 'linum))

  (defun bw/linum-relative-formatting ()
    "Turn on relative formatting"
    (setq-local linum-format 'linum-relative))

  (defun bw/linum-normal-formatting ()
    "Turn on non-relative formatting"
    (setq-local linum-format 'bw/linum-non-relative))

  ;; I never use linum-mode except for this, so it's okay to
  ;; clobber it
  (setq linum-format 'bw/linum-non-relative
        ;; show >> on line where cursor is
        linum-relative-current-symbol ">>")

  ;; in Normal mode, use relative numbering
  (add-hook 'evil-normal-state-entry-hook 'bw/linum-relative-formatting)
  ;; in Insert mode, use normal line numbering
  (add-hook 'evil-insert-state-entry-hook 'bw/linum-normal-formatting)
  ;; turn off linum mode automatically when entering Emacs mode
  (add-hook 'evil-emacs-state-entry-hook 'bw/disable-linum-mode)
  ;; turn off linum mode when entering Emacs
  (add-hook 'evil-emacs-state-entry-hook 'bw/linum-normal-formatting)

  ;; copy linum face so it doesn't look weird
  (set-face-attribute 'linum-relative-current-face nil :foreground (face-attribute 'font-lock-keyword-face :foreground) :background nil :inherit 'linum :bold t))

(require 'linum-relative)

;; abbrev comes with evil
(diminish 'abbrev-mode)

(provide 'setup-evil)
