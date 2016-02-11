;;; init.el - Load Emacs

;;; Commentary:

;; This file uses org-babel to load my literate configuration.

;; This is required by Emacs 25.1 for some reason.
;; (package-initialize)

(require 'ob-tangle)

(org-babel-load-file (expand-file-name "README.org" (file-name-directory
                                                    (or (buffer-file-name) load-file-name))))
