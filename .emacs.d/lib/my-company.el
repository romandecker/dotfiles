(require 'my-funcs)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-w") #'backward-kill-word)

  (define-key company-active-map [tab] 'my-funcs/expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'my-funcs/expand-snippet-or-complete-selection)
  (nconc company-backends '(company-yasnippet))
  (global-company-mode))

(provide 'my-company)
