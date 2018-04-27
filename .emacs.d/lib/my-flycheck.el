;;; package --- My custom flycheck config
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :demand t
  :general
  (:keymaps 'normal
   "] e" 'flycheck-next-error
   "[ e" 'flycheck-previous-error)
  :config
  (global-flycheck-mode)

  (use-package flycheck-popup-tip
    :ensure t
    :config
    (flycheck-popup-tip-mode)
    )
  )

(provide 'my-flycheck)
;;; my-flycheck.el ends here
