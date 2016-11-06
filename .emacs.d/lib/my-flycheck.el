(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (define-key evil-normal-state-map (kbd "] e") 'flycheck-next-error)
  (define-key evil-normal-state-map (kbd "[ e") 'flycheck-previous-error))

(provide 'my-flycheck)
