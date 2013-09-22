;;; My emacs.d init file.
;;; <brad@intranation.com>

;; Turn off chrome
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; inhibit startup screen
(setq inhibit-startup-screen t
      ;; Show *scratch* on start
      initial-buffer-choice t)

;; start a server, unless one is already running
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

;; base load path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customised Emacs configuration")

(add-to-list 'load-path dotfiles-dir)

(require 'utils)

;; tmp directory for storing non-config things
(make-directory (setq tmp-local-dir (bw/join-dirs dotfiles-dir ".tmp")) t)

;; My font
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 180 :family "Inconsolata"))

;; always highlight syntax
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Always show line number in the mode line
(line-number-mode 1)
;; ... and show the column number
(column-number-mode 1)

;; UTF-8 FTW
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; always add a trailing newline - POSIX
(setq require-final-newline t)

;; nuke trailing whitespace when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; I got sick of typing "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Default indentation
(setq-default
 ;; no tabs
 indent-tabs-mode nil
 ;; 4 spaces
 tab-width 4)

;; Don't clobber things in the system clipboard when killing
(setq save-interprogram-paste-before-kill t)

(when (display-graphic-p)
  ;; show help in the echo area instead of as a tooltip
  (tooltip-mode -1)

  ;; never pop a dialogue box
  (setq use-dialog-box nil)

  ;; From:
  ;; http://emacs-fu.blogspot.co.uk/2011/01/setting-frame-title.html
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

;; blink the cursor
(setq blink-cursor-interval 1.0)
(blink-cursor-mode 1)

;; show keystrokes immediately
(setq echo-keystrokes 0.1)

;; OSX specific overrides
(when (eq system-type 'darwin)
  ;; Mac hostnames have .local or similar appended
  (setq system-name (car (split-string system-name "\\.")))

  ;; OSX ls doesn't support --dired
  (setq dired-use-ls-dired nil)

  ;; OSX in a GUI
  (when (display-graphic-p)
    ;; This makes left-option do M-
    (setq ns-alternate-modifier 'meta)
    ;; ... and right-option just do option so I can still type
    ;; alternate characters.
    (setq ns-right-alternate-modifier nil)

    ;; use right command as a ctrl, since laptops never have right
    ;; ctrls
    (setq ns-right-command-modifier 'control)

    ;; command is super
    (setq ns-command-modifier 'super)

    ;; set fn to hyper
    (setq ns-function-modifier 'hyper))

  (unless (display-graphic-p)
    ;; Configuration to make Emacs run semi-normally in an OS X
    ;; terminal

    ;; XXX: strongly recommended to run in iTerm2, as it's more
    ;; configurable than Terminal.app.

    ;; Make sure cut/paste works properly. Gotten from:
    ;; http://mindlev.wordpress.com/2011/06/13/emacs-in-a-terminal-on-osx/#comment-20
    (defun bw/copy-from-osx ()
      "Copies the current clipboard content using the `pbcopy` command"
      (shell-command-to-string "pbpaste"))

    (defun bw/paste-to-osx (text &optional push)
      "Copies the top of the kill ring stack to the OSX clipboard"
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    ;; Override defaults to use the mac copy and paste
    (setq interprogram-cut-function 'bw/paste-to-osx)
    (setq interprogram-paste-function 'bw/copy-from-osx)))

;; smart beginning-of-line, from:
;; http://irreal.org/blog/?p=1946
(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

;; I want to use narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; I want to use scrolling
(put 'scroll-left 'disabled nil)

;; Show bell instead of making a sound
(setq visible-bell t)
;; Conditionally ring the bell
(setq ring-bell-function
      (lambda ()
        "Only rings the bell if it's not a valid quit case, e.g
keyboard-quit"
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

;; stop allowing point over minibuffer prompt
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties (add-to-list 'minibuffer-prompt-properties 'minibuffer-avoid-prompt))
(setq minibuffer-prompt-properties (add-to-list 'minibuffer-prompt-properties 'point-entered))

;;; Remap execute-extended-command
;; This is Yegge's advice
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "C-x m") 'execute-extended-command)
;; fat finger version
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;;; Remap yank/kill
;; Copy readline's kill word
(global-set-key (kbd "C-w") 'backward-kill-word)
;; Since we've unset C-w, map it to something else
(global-set-key (kbd "C-x C-k") 'kill-region)
;; ... and the clumsy version
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; indent automatically
(add-hook 'prog-mode-hook 'bw/turn-on-electric-indent-mode)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; ediff mode
(setq
 ;; make two side-by-side windows
 ediff-split-window-function 'split-window-horizontally
 ;; ignore whitespace diffs
 ediff-diff-options          "-w"
 ;; Do everything in one frame always
 ediff-window-setup-function 'ediff-setup-windows-plain)


;;; ido
(after-load 'ido
  (ido-mode t)
  (ido-everywhere t)
  ;; Ignore shitty Dropbox icon stuff:
  ;; http://stackoverflow.com/a/11341239/61435
  (add-to-list 'ido-ignore-files "Icon\n")

  ;; configure ido
  (setq
   ;; Speed up ido by using less candidates
   ido-max-prospects 10
   ;; Match arbitrary points in strings
   ido-enable-prefix nil
   ;; Match across entire string
   ido-enable-flex-matching t
   ;; Create a new buffer if there's no match candidate
   ido-create-new-buffer 'always
   ;; Don't try and guess if the string under point is a file
   ido-use-filename-at-point nil
   ;; case-insensitive matching
   ido-case-fold t
   ;; go back in time if required
   ido-use-virtual-buffers nil))

(require 'ido)


;;; bookmarks
(after-load 'bookmark
  ;; make the file location consistent
  (setq bookmark-default-file (expand-file-name ".emacs.bmk" tmp-local-dir)))


;; recently opened files
(after-load 'recentf
  (setq
   recentf-auto-cleanup 'never
   recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'" "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies")
   ;; save 100 most recent files
   recentf-max-saved-items 100
   ;; keep recent files in consistent place
   recentf-save-file (expand-file-name ".recentf" tmp-local-dir))

  ;; enable recentf
  (recentf-mode 1)

  ;; strip $HOME from the front of recentf candidate files
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name)

  ;; handle recent candidates with ido
  (after-load 'ido
    (defun bw/recentf-ido-find-file ()
      "Find a recent file using ido."
      (interactive)
      (let ((file (ido-completing-read "Recently: " recentf-list nil t)))
        (when file
          (find-file file))))

    (global-set-key (kbd "C-x C-r") 'bw/recentf-ido-find-file)))

(require 'recentf)

;; Hippie expand - auto-completion mechanism
;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
;; make hippie expand behave itself
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; emacs lisp configuration

;; automatically document the Emacs Lisp functions and variables under
;; point
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;; ansi-term configuration
;; force ansi-term to be utf-8 after it launches
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;; close the terminal buffer automatically on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

;;; backup and autosave configuration
(make-directory (setq tmp-backups-dir (bw/join-dirs tmp-local-dir "backups")) t)
(make-directory (setq tmp-autosaves-dir (bw/join-dirs tmp-local-dir "autosaves")) t)

(setq
 ;; don't clobber symlinks
 backup-by-copying t
 ;; copy into temporary directory
 backup-directory-alist `((".*" . ,tmp-backups-dir))
 ;; copy into autosaves directory
 auto-save-file-name-transforms `((".*" ,tmp-autosaves-dir t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; Use versioned backups
 version-control t)

;;; python-mode - Python major mode
(add-hook 'python-mode-hook 'bw/turn-on-subword-mode)

;;; tramp-mode - Edit files remotely
(after-load 'tramp
  ;; use SSH by default
  (setq tramp-default-method "ssh")
  ;; allow me to SSH to hosts and edit as sudo like:
  ;;   C-x C-f /sudo:example.com:/etc/something-owned-by-root
  ;; from: http://www.gnu.org/software/tramp/#Multi_002dhops
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
               '((regexp-quote (system-name)) nil nil)))


;;; ELPA customisation and installation
(require 'setup-elpa)


;;; Load system specific code

(setq local-dotfiles-dir (bw/join-dirs dotfiles-dir "local"))

(setq
 bw/user-config (concat local-dotfiles-dir user-login-name ".el")
 bw/system-config (concat local-dotfiles-dir system-name ".el"))

(when (file-exists-p bw/user-config)
  (load bw/user-config))
(when (file-exists-p bw/system-config)
  (load bw/system-config))


;; Load custom file last
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
