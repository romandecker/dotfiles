;;; package --- My custom flycheck config
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :general
  (:keymaps 'normal
   "] e" 'flycheck-next-error
   "[ e" 'flycheck-previous-error)
  :config
  (global-flycheck-mode))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
