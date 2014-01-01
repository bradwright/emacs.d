;;; Company mode - intelligent completion
;;; https://github.com/company-mode/company-mode

(require-package 'company)

(defun bw/company-complete-lambda (arg)
  "Ignores passed in arg like a lambda and runs company-complete"
  (company-complete))

(defun bw/enable-company-mode ()
  "Enables company-mode"
  (company-mode 1)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(after-load 'company
  (add-hook 'prog-mode-hook 'bw/enable-company-mode)

  (setq company-minimum-prefix-length 0 ;; autocomplete right after '.'
        ;; shorter delay
        company-idle-delay .3
        ;; remove echo delay
        company-echo-delay 0
        ;; don't complete in certain modes
        company-global-modes '(not git-commit-mode)
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
