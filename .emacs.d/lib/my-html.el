;;; package --- My custom config for editing HTML
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode)
  (setq emmet-move-cursor-between-quotes t))


(provide 'my-html)
;;; my-html.el ends here
