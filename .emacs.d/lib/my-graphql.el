;;; package --- My custom graphql config
;;; Commentary:
;;; Code:

(use-package graphql-mode
  :ensure t
  :config

  (defun my/graphql-mode-hook ()
    (setq aggressive-fill-paragraph-mode nil))

  (add-hook-x graphql-mode-hook my/graphql-mode-hook))


(provide 'my-graphql)
;;; my-graphql.el ends here
