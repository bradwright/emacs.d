;;; Packaging via ELPA
(after-load 'package
  ;; Store installed packages alongside this code
  (setq package-user-dir
        (bw/join-dirs dotfiles-dir ".elpa"))

  ;; Only use 3 specific directories
  (setq package-archives
        '(("gnu"       . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa"     . "http://melpa.milkbox.net/packages/")))

  ;; initialise package.el
  (package-initialize)

  ;; Clean up after ELPA installs:
  ;; https://github.com/purcell/emacs.d/blob/master/init-elpa.el
  (defadvice package-generate-autoloads
    (after close-autoloads (name pkg-dir) activate)
    "Stop package.el from leaving open autoload files lying around."
    (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
      (with-current-buffer (find-file-existing path)
        (kill-buffer nil))))

  ;; Auto-install the Melpa package, since it's used to filter
  ;; packages.
  (when (not (package-installed-p 'melpa))
    (progn
      (switch-to-buffer
       (url-retrieve-synchronously
        "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
      (package-install-from-buffer (package-buffer-info) 'single))))

;; Blacklist some non-melpa packages
(after-load 'melpa
  (setq package-archive-exclude-alist
         '(("melpa"
            melpa               ;; don't want to self-host this
            ))))

(require 'package nil t)

;; Magit - Git interface
;; https://github.com/magit/magit
;; key
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c f") 'magit-grep)

;; we no longer need vc-git
(delete 'Git vc-handled-backends)

;; make magit status go full-screen but remember previous window
;; settings
;; from: http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; Close popup when commiting - this stops the commit window
;; hanging around
;; From: http://git.io/rPBE0Q
(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

;; these two force a new line to be inserted into a commit window,
;; which stops the invalid style showing up.
;; From: http://git.io/rPBE0Q
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))

(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

;; restore previously hidden windows
(defadvice magit-quit-window (around magit-restore-screen activate)
  (let ((current-mode major-mode))
    ad-do-it
    ;; we only want to jump to register when the last seen buffer
    ;; was a magit-status buffer.
    (when (eq 'magit-status-mode current-mode)
      (jump-to-register :magit-fullscreen))))

(defun magit-maybe-commit (&optional show-options)
  "Runs magit-commit unless prefix is passed"
  (interactive "P")
  (if show-options
      (magit-key-mode-popup-committing)
    (magit-commit)))

(after-load 'magit
  (define-key magit-mode-map "c" 'magit-maybe-commit))

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
 magit-set-upstream-on-push 'askifnotset
 )

(require-package 'magit)

;;; ido-ubiquitous - because ido-everywhere isn't enough
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;;; ido-vertical - display ido candidates vertically
(require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
;; only show 5 candidates because it's vertical
(setq ido-max-prospects 5)

;;; smex - IDO completion for M-x
(require-package 'smex)
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)


;;; ag.el - Emacs frontend to ag search
(require-package 'ag)
;; auto-loaded
(global-set-key (kbd "C-c f") 'ag-project)
;; reuse ag buffers
(setq ag-reuse-buffers t)

;;; magit-find-file - Completing read frontend to git ls-files
(require-package 'magit-find-file)
;; this function is auto-loaded
(global-set-key (kbd "C-c t") 'magit-find-file-completing-read)
(global-set-key (kbd "M-p") 'magit-find-file-completing-read)

;; OSX packages
(when (eq system-type 'darwin)
  ;;; exec-path-from-shell - Copy shell environment variables to Emacs
  (require-package 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL"))
  (exec-path-from-shell-initialize)
  (when (display-graphic-p)
    ;;; solarized-theme - Emacs version of Solarized
    (require-package 'solarized-theme)
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-dark t)))

;;; flx-ido - advanced flex matching for ido
(require-package 'flx-ido)
;; use more RAM to cache more candidate lists
(setq gc-cons-threshold 20000000)
(flx-ido-mode 1)


;;; diminish.el - hide minor-mode lighters in modeline
(require-package 'diminish)
(after-load 'eldoc
  (diminish 'eldoc-mode))


;;; js2-mode - Major mode for editing Javascript
(require-package 'flymake-cursor)
(require 'flymake-cursor)

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

(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'setup-elpa)
