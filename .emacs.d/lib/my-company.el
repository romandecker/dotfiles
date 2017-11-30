;;; package --- Custom settings for auto-completion
;;; Commentary:
;;; Code
(use-package company
  :ensure t
  :demand t
  :general
  (:keymaps 'company-active-map
    "M-n" nil
    "M-p" nil
    "C-n" 'company-select-next
    "C-p" 'company-select-previous
    "C-w" 'backward-kill-word
    "C-h" 'helm-company
    [tab] 'my/tab-dwim
    "TAB" 'my/tab-dwim)
  (:keymaps 'company-mode-map
   :states 'insert
   "C-n" 'company-complete)
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil)
  (nconc company-backends '(company-yasnippet))
  (global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1))
  (use-package helm-company
    :ensure t
    :config)
  )

(provide 'my-company)
;;; my-company.el ends here
