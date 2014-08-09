;;; emacs.el - Load Emacs

;;; Commentary:

;; This file uses org-babel to load my literate configuration.

(require 'ob-tangle)

(org-babel-load-file (expand-file-name "emacs.org" (file-name-directory
                                                    (or (buffer-file-name) load-file-name))))
