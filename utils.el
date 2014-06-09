;;; utils.el
;;; Various functions and macros

(require 'cl-lib)

;; Syntactic sugar for eval-after-load dance.
;; https://github.com/purcell/emacs.d/blob/aa789c9745b13612c4fea6e638d81d8ebbfecdf8/init-utils.el#L1-L5
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;;; On-demand installation of packages
;;; From:
;;; https://github.com/purcell/emacs.d/blob/aa789c9745b13612c4fea6e638d81d8ebbfecdf8/init-elpa.el#L63-L73
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun bw/kill-emacs ()
  "If this buffer is a client, just kill it, otherwise confirm
the quit."
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (if (= (length (frame-list)) 1)
        (cond ((y-or-n-p "Quit Emacs? ")
               (save-buffers-kill-terminal)))
      (save-buffers-kill-terminal))))

(global-set-key (kbd "C-x C-c") 'bw/kill-emacs)

(defmacro bw/diminish-after-load (to-load to-diminish)
  (eval-after-load ,to-load
    `(diminish ,to-diminish)))

(defun bw/packages-installed-p (to-install)
  "Checks whether all packages TO-INSTALL  are installed."
  (loop for p in to-install
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun bw/install-packages (to-install)
  "Auto-installs all packages TO-INSTALL"
  (unless (essential-packages-installed-p to-install)
    (package-refresh-contents)
    (dolist (p to-install)
      (unless (package-installed-p p)
        (package-install p)))
    (delete-other-windows)))

;; http://andrewcoxtech.blogspot.co.uk/2009/11/inserting-bom-into-file.html
(defun bw/insert-bom()
  "Inserts a valid UTF8 byte order mark"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ucs-insert (string-to-number "FEFF" 16))
    (message "BOM inserted")))

(defun bw/open-term (&optional arg)
  "Opens an ansi-term with value of $TERM - force new ansi-term
with prefix"
  (interactive "p")
  (if (or (not (get-buffer "*ansi-term*")) (= arg 4))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))
(global-set-key (kbd "C-c C-t t") 'bw/open-term)

(defun bw/require-list (items)
  "Require each thing in ITEMS"
  (interactive)
  (dolist (item items)
    (require `,item nil t)))

;; gotten from:
;; http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(defun bw/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)

(defun bw/get-keychain-password (account-name)
  "Get ACCOUNT-NAME keychain password from OS X Keychain"
  (interactive "sAccount name: ")
  (bw/chomp
   (shell-command-to-string
    (concat
     "security find-generic-password -wa "
     account-name))))

;; from:
;; https://github.com/technomancy/emacs-starter-kit/blob/31c2465712485a54aba6a3ef6d1bef9b564f8f37/starter-kit-defuns.el#L179
(defun sudo-edit (&optional arg)
  "Edit this file as sudo"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun bw/occur-non-ascii-chars ()
  "Finds characters that aren't in the displayable range for ASCII"
  (interactive)
  (occur "[^\000-\177]"))

(defun bw/turn-on-auto-fill ()
  "Enables auto-fill"
  (interactive)
  (auto-fill-mode 1))

(defun bw/turn-off-auto-fill ()
  "Disables auto-fill"
  (interactive)
  (auto-fill-mode -1))

(defun bw/enable-linum-mode ()
  "Enables linum-mode"
  (linum-on))

(defun bw/disable-linum-mode ()
  "Disables linum-mode"
  (linum-mode -1))

(defun bw/enable-hl-line-mode ()
  "Enables hl-line-mode"
  (interactive)
  (hl-line-mode 1))

(defun bw/turn-on-flymake-mode ()
  "Turns on flymake-mode locally"
  (interactive)
  (flymake-mode 1))

(defun bw/turn-on-electric-indent-mode ()
  "Turns on electric-indent-mode"
  (interactive)
  (electric-indent-mode 1))

(defun bw/turn-off-electric-indent-mode ()
  "Turns off electric-indent-mode"
  (interactive)
  (electric-indent-mode -1))

(defun bw/turn-on-electric-pair-mode ()
  "Turns on electric-pair-mode"
  (interactive)
  (electric-pair-mode 1))

(defun bw/turn-off-electric-pair-mode ()
  "Turns off electric-pair-mode"
  (interactive)
  (electric-pair-mode -1))

(defun bw/locate-library-dir (library)
  "Locates the directory containing a loaded library"
  (file-name-directory (locate-library library)))

(defun bw/add-to-load-path (dir)
  "Adds `dir` to load-path"
  (add-to-list 'load-path dir))

(defun bw/add-to-custom-theme-load-path (dir)
  "Adds `dir` to custom-theme-load-path"
  (add-to-list 'custom-theme-load-path dir))

(defun bw/join-dirs (prefix suffix)
  "Joins `prefix` and `suffix` into a directory"
  (file-name-as-directory (concat prefix suffix)))

(defun bw/turn-on-subword-mode ()
  "Turns on subword mode for a buffer"
  (subword-mode 1))

;; Gotten from:
;; http://whattheemacsd.com/appearance.el-01.html
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

(provide 'utils)
