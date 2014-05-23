;;; Packaging via ELPA
(after-load 'package
  ;; Store installed packages alongside this code
  (setq package-user-dir
        ;; this is version-specific
        (bw/join-dirs (bw/join-dirs dotfiles-dir ".elpa") emacs-version))

  (setq package-archives
        '(("gnu"          . "http://elpa.gnu.org/packages/")
          ("melpa"        . "http://melpa.milkbox.net/packages/")
          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
          ("elpy"         . "http://jorgenschaefer.github.io/packages/")))

  (when (boundp 'package-pinned-packages)
    ;; this is Emacs 24.4+
    (setq-default package-pinned-packages
                  '()))

  ;; initialise package.el
  (package-initialize)

  ;;; this is *not* Emacs 24.4+
  (unless (boundp 'package-pinned-packages)

    ;; Auto-install the Melpa package, since it's used to filter
    ;; packages.
    (unless (package-installed-p 'melpa)
      (progn
        (switch-to-buffer
         (url-retrieve-synchronously
          "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
        (package-install-from-buffer (package-buffer-info) 'single)))

    ;; Blacklist some non-melpa packages
    (after-load 'melpa
      (setq-default package-archive-exclude-alist
                    '(("melpa"
                       melpa          ;; Otherwise this will always update due
                                      ;; to Melpa versioning
                       diminish       ;; not updated in forever
                       flymake-cursor ;; Melpa version is on wiki
                       idomenu        ;; not updated in ages
                       json-mode      ;; not on Melpa
                       ))))

    ;; Clean up after ELPA installs:
    ;; https://github.com/purcell/emacs.d/blob/master/init-elpa.el
    (defadvice package-generate-autoloads
        (after close-autoloads (name pkg-dir) activate)
      "Stop package.el from leaving open autoload files lying around."
      (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
        (with-current-buffer (find-file-existing path)
          (kill-buffer nil))))))

(require 'package nil t)

;; OSX packages and PATH mangling
(when (eq system-type 'darwin)
  ;;; exec-path-from-shell - Copy shell environment variables to Emacs
  ;;; https://github.com/purcell/exec-path-from-shell
  (require-package 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH"  "MANPATH" "SHELL"))
  (exec-path-from-shell-initialize)

  (when (display-graphic-p)
    ;;; solarized-theme - Emacs version of Solarized
    ;;; https://github.com/bbatsov/solarized-emacs
    (require-package 'solarized-theme)
    ;; default modeline is a bit low contrast for me, this reverses it
    ;; out.
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-dark t)))


(require 'setup-magit)


;;; gitignore-mode - major mode for editing .gitignore files
;;; https://github.com/magit/git-modes
(require-package 'gitignore-mode)

;;; gitconfig-mode - major mode for editing .git/config files
;;; https://github.com/magit/git-modes
(require-package 'gitconfig-mode)

;;; ido-ubiquitous - because ido-everywhere isn't enough
;;; https://github.com/DarwinAwardWinner/ido-ubiquitous
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;;; ido-vertical - display ido candidates vertically
;;; https://github.com/rson/ido-vertical-mode.el
(require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
;; only show 5 candidates because it's vertical
(setq ido-max-prospects 5)

;;; smex - IDO completion and access frequent commands for M-x
;;; https://github.com/nonsequitur/smex
;;; XXX: This is somewhat clobbered by flx matching further down
(require-package 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
;; stop showing me "click this menubar" advice in the minibuffer
(setq-default smex-key-advice-ignore-menu-bar t)

;;; ag.el - Emacs frontend to ag search
;; https://github.com/Wilfred/ag.el
(require-package 'ag)
;; auto-loaded
(global-set-key (kbd "C-c f") 'ag-project)
(global-set-key (kbd "C-c a") 'ag)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-F") 'ag-project)
  (global-set-key (kbd "s-A") 'ag))
;; reuse ag buffers - without this my buffer list is full of named
;; buffers. I want it to behave like M-x rgrep.
(setq ag-reuse-buffers t)

;;; magit-find-file - Completing read frontend to git ls-files
;;; https://github.com/bradleywright/magit-find-file.el
(require-package 'magit-find-file)
;; this function is auto-loaded
(global-set-key (kbd "C-c p") 'magit-find-file-completing-read)
(global-set-key (kbd "M-p") 'magit-find-file-completing-read)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-p") 'magit-find-file-completing-read))


;;; flx-ido - advanced flex matching for ido
;;; https://github.com/lewang/flx
(require-package 'flx-ido)
;; use more RAM to cache more candidate lists
(setq gc-cons-threshold 20000000)
;; take over ido-mode
(flx-ido-mode 1)


;;; diminish.el - hide minor-mode lighters in modeline
;;; https://github.com/emacsmirror/diminish
(require-package 'diminish)

(after-load 'diminish
  (after-load 'subword
    (diminish 'subword-mode))
  (after-load 'eldoc
    (diminish 'eldoc-mode))
  (after-load 'autorevert
    (diminish 'auto-revert-mode)))

(require 'setup-javascript)

;;; paredit - tools for editing sexps
;;; http://melpa.milkbox.net/#/paredit
(require-package 'paredit)
(require 'setup-company)


;; autoloaded
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
(defun conditionally-enable-paredit-mode ()
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;;; idomenu - navigate code in current buffer using ido
;;; http://melpa.milkbox.net/#/idomenu
(require-package 'idomenu)
;; autoloaded
(global-set-key (kbd "C-c i") 'idomenu)
(setq imenu-auto-rescan t)

(require 'setup-ruby)
(require 'setup-evil)

;;; markdown-mode - for editing markdown
;;; http://melpa.milkbox.net/#/markdown-mode
(require-package 'markdown-mode)
(after-load 'markdown-mode
  ;; allow for navigation of Markdown headings with imenu
  (setq markdown-imenu-generic-expression
        '(("title"  "^\\(.*\\)[\n]=+$" 1)
          ("h2-"    "^\\(.*\\)[\n]-+$" 1)
          ("h1"   "^# \\(.*\\)$" 1)
          ("h2"   "^## \\(.*\\)$" 1)
          ("h3"   "^### \\(.*\\)$" 1)
          ("h4"   "^#### \\(.*\\)$" 1)
          ("h5"   "^##### \\(.*\\)$" 1)
          ("h6"   "^###### \\(.*\\)$" 1)
          ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq imenu-generic-expression markdown-imenu-generic-expression))))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;;; puppet-mode - syntax highlighting for Puppet
;;; https://github.com/lunaryorn/puppet-mode
(require-package 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;; undo-tree - visualise undo history as a tree
;;; http://www.dr-qubit.org/undo-tree/undo-tree.el
;;; This comes bundled with evil-mode
(after-load 'undo-tree
  (diminish 'undo-tree-mode))


;;; multiple-cursors.el - like the Sublime Text 2 thing
;;; https://github.com/magnars/multiple-cursors.el
(require-package 'multiple-cursors)
;; mc/* requires this `require` form as the autoload doesn't cleanly
;; work - the selected regions are always off in the first instance.
(after-load 'multiple-cursors
  (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-l") 'mc/mark-all-like-this))
(require 'multiple-cursors)


;;; expand-region - Increase selected region by semantic units
;;; https://github.com/magnars/expand-region.el
(require-package 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)


;;; browse-kill-ring - instead of a literal ring, let me pick
;;; https://github.com/browse-kill-ring/browse-kill-ring
(require-package 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)
(after-load
  ;; make it act like pop and overwrite previous yank
  (setq browse-kill-ring-replace-yank t))


;;; scss-mode - Major mode for editing SCSS files
;;; https://github.com/antonj/scss-mode
(require-package 'scss-mode)
(after-load 'scss-mode
  ;; these aren't safe-local-variables, so they can't normally be
  ;; overriden by other things. Fix that.
  (put 'scss-sass-command 'safe-local-variable 'stringp)
  (put 'css-indent-offset 'safe-local-variable 'integerp)
  (put 'scss-compile-at-save 'safe-local-variable 'booleanp))


;;; popwin-mode - Better window management
;;; https://github.com/m2ym/popwin-el
(require-package 'popwin)
(after-load 'popwin
  (add-to-list 'popwin:special-display-config `"*ag search*")
  (add-to-list 'popwin:special-display-config `("*magit-process*" :noselect t))
  (add-to-list 'popwin:special-display-config `"*Flycheck errors*")
  (add-to-list 'popwin:special-display-config `"*Occur*"))
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-c P") 'popwin:popup-last-buffer)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-P") 'popwin:popup-last-buffer))


;;; go-mode - Major mode for editing golang files
;;; http://melpa.milkbox.net/#/go-mode
(require-package 'go-mode)
(after-load 'go-mode
  ;; auto-format my Golang files correctly
  (add-hook 'before-save-hook 'gofmt-before-save)

  (global-set-key (kbd "C-c C-r") 'go-remove-unused-imports)

  (after-load 'flycheck
    (add-hook 'go-mode-hook 'flycheck-mode-on-safe)))


;;; go-eldoc - show documentation for Go functions
;;; https://github.com/syohex/emacs-go-eldoc
(require-package 'go-eldoc)
(after-load 'go-mode
  (add-hook 'go-mode-hook 'go-eldoc-setup))


;;; ace-jump-mode - Minor mode to jump around buffers
;;; https://github.com/winterTTr/ace-jump-mode
(require-package 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "C-;") 'ace-jump-char-mode)
(global-set-key (kbd "C-<return>") 'ace-jump-line-mode)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-j") 'ace-jump-char-mode)
  (global-set-key (kbd "s-J") 'ace-jump-line-mode))


;;; web-mode - Major mode for editing various templates and HTML
;;; https://github.com/fxbois/web-mode
(require-package 'web-mode)
(after-load 'web-mode
  (setq web-mode-engines-alist
        '(("\\.jinja\\'" . "django"))))

(dolist (alist '(("\\.html$'" . web-mode)
                 ("\\.html\\.erb$" . web-mode)
                 ("\\.mustache$" . web-mode)
                 ("\\.jinja$" . web-mode)
                 ("\\.php$" . web-mode)))
  (add-to-list 'auto-mode-alist alist))


;;; smartparens - smarter matching and pairing of things
;;; https://github.com/Fuco1/smartparens
(require-package 'smartparens)
(after-load 'smartparens
  (require 'smartparens-config)
  ;; replacement for show-paren-mode
  (show-smartparens-global-mode 1))
(require 'smartparens)


;;; cider - Clojure IDE and Repl
;;; https://github.com/clojure-emacs/cider
(require-package 'cider)
(after-load 'cider-mode
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'clojure-enable-cider))


;;; flycheck - Modern on the fly syntax checking and linting
;;; https://github.com/flycheck/flycheck
(require-package 'flycheck)
(after-load 'flycheck
  (setq
   ;; don't show anything in the left fringe
   flycheck-indication-mode nil))
(require 'flycheck)


(require 'setup-god)


;;; httprepl.el - Make HTTP requests via a REPL
;;; https://github.com/gregsexton/httprepl.el
(require-package 'httprepl)


;;; paradox - Package management with GitHub integration
;;; https://github.com/Bruce-Connor/paradox
(require-package 'paradox)
;; automatically star packages I install on GitHub
(setq paradox-automatically-star t)
(setq paradox-github-token (bw/get-keychain-password "paradox-github-token"))


;;; Projectile - Project management
;;; https://github.com/bbatsov/projectile
(require-package 'projectile)
(projectile-global-mode)

(defadvice projectile-current-project-files (around bw/use-magit-find-file activate)
  "If magit-find-file-completing-read is available use that to
call the files instead of Projectile's native caller - this is
much much faster"
  (autoload 'magit-find-file-files "magit-find-file")
  (if (magit-get-top-dir)
      (setq ad-return-value (magit-find-file-files (projectile-project-root)))
    ad-do-it))

(after-load 'projectile
  (define-key projectile-mode-map [remap projectile-grep] 'projectile-ag)
  (define-key projectile-mode-map [remap magit-find-file-completing-read] 'projectile-find-file)
  (define-key projectile-mode-map [remap ag-project] 'projectile-ag)
  (global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
  (global-set-key (kbd "C-x 4 s-b") 'projectile-switch-to-buffer-other-window))


;;; ace-window - ace jump for moving windows
;;; https://github.com/abo-abo/ace-window
(require-package 'ace-window)
(define-key (current-global-map) [remap other-window] 'ace-window)


(provide 'setup-elpa)
