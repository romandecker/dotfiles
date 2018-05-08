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

  (defun my/flycheck-enable-checker ()
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'flycheck-disable-checker)))
  )

(provide 'my-flycheck)
;;; my-flycheck.el ends here
