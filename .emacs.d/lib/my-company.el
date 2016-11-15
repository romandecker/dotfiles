;;; package --- Custom settings for auto-completion
;;; Commentary:
;;; Code
(use-package company
  :ensure t
  :defer 1
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-w" . backward-kill-word)
              ([tab] . my/expand-snippet-or-complete-selection)
              ("TAB" . my/expand-snippet-or-complete-selection))
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (nconc company-backends '(company-yasnippet))
  (global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1))
  (message "company has loaded!"))


(provide 'my-company)
;;; my-company.el ends here
