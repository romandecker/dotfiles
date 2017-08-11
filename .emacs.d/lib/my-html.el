;;; package --- My custom config for editing HTML
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :ensure t
  :general
  (:states 'visual
   :keymaps 'emmet-mode-keymap
   "C-y" 'emmet-wrap-with-markup)
  :config
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode)
  (setq emmet-move-cursor-between-quotes t))


(provide 'my-html)
;;; my-html.el ends here
