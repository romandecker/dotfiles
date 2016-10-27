(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (define-key evil-normal-state-map (kbd "] p") 'next-error)
  (define-key evil-normal-state-map (kbd "[ p") 'previous-error))

(provide 'my-flycheck)
