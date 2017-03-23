;;; package --- My custom typescript config
;;; Commentary:
;;; Code:

(use-package tide
  :ensure t
  :config
  (defun my/tide-setup ()
    (interactive)
    (tide-setup))
  (add-hook 'typescript-mode-hook #'my/setup-tide-mode))


(provide 'my-typescript)
;;; my-typescript.el ends here
