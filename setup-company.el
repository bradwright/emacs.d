;;; Company mode - intelligent completion
;;; https://github.com/company-mode/company-mode

(require-package 'company)

(after-load 'company
  (global-company-mode)
  (global-set-key (kbd "M-/") 'company-complete))


;;; company-go - company backend for Golang
;;; https://github.com/nsf/gocode/blob/master/emacs-company/company-go.el
(require-package 'company-go)


(provide 'setup-company)
