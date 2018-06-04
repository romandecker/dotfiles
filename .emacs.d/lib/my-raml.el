;;; package --- My custom config for editing RAML files
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :ensure t
  :config
  (setq yaml-indent-offset 2))

(provide 'my-raml)
;;; my-raml.el ends here
