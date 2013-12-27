;;; Company mode - intelligent completion
;;; https://github.com/company-mode/company-mode

(require-package 'company)

(defun bw/company-complete-lambda (arg)
  "Ignores passed in arg like a lambda and runs company-complete"
  (company-complete))

(after-load 'company
  (global-company-mode t)
  (global-set-key (kbd "M-/") 'company-complete)
  (setq evil-complete-next-func 'bw/company-complete-lambda
        evil-complete-previous-func 'bw/company-complete-lambda))

(require 'company)


;;; company-go - company backend for Golang
;;; https://github.com/nsf/gocode/blob/master/emacs-company/company-go.el
(require-package 'company-go)

(require 'company-go)

(defun bw/setup-company-go ()
  "Hook for running on company-go"
  (set (make-local-variable 'company-backends) '(company-go)))

(after-load 'company-go
  (add-hook 'go-mode-hook 'bw/setup-company-go))



(provide 'setup-company)
