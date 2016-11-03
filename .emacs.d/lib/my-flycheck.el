(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (define-key evil-normal-state-map (kbd "] e") 'next-error)
  (define-key evil-normal-state-map (kbd "[ e") 'previous-error))

(provide 'my-flycheck)
