;;; Company mode - intelligent completion
;;; https://github.com/company-mode/company-mode

(require-package 'company)

(defun bw/company-complete-lambda (arg)
  "Ignores passed in arg like a lambda and runs company-complete"
  (company-complete))

(after-load 'company
  (global-company-mode t)
  ;; Make sure emacs does the right thing with completion command
  (global-set-key (kbd "M-/") 'company-complete)
  (setq company-minimum-prefix-length 0 ;; autocomplete right after '.'
        ;; shorter delay
        company-idle-delay .3
        ;; remove echo delay
        company-echo-delay 0
        ;; make sure evil uses the right completion functions
        evil-complete-next-func 'bw/company-complete-lambda
        evil-complete-previous-func 'bw/company-complete-lambda))

(require 'company)


;;; company-go - company backend for Golang
;;; https://github.com/nsf/gocode/blob/master/emacs-company/company-go.el
(require-package 'company-go)

(defun bw/setup-company-go ()
  "Hook for running on company-go"
  ;; we only want to use company-go - it's so accurate we won't need any other
  ;; completion engines
  (set (make-local-variable 'company-backends) '(company-go)))

(after-load 'company-go
  (add-hook 'go-mode-hook 'bw/setup-company-go))

(require 'company-go)


(provide 'setup-company)
