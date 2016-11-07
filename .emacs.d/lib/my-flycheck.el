;;; package --- My custom flycheck config
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (define-key evil-normal-state-map (kbd "] e") 'flycheck-next-error)
  (define-key evil-normal-state-map (kbd "[ e") 'flycheck-previous-error))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
