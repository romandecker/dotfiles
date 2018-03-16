;;; package --- My custom config for editing SGML-style documents (e.g. HTML, XML, etc...)
;;; Commentary:
;;; Code:

(use-package emmet-mode
  :ensure t
  :demand t
  :general
  (:states 'insert
   :keymaps 'emmet-mode-keymap
   "C-y ," 'emmet-expand-line
   "C-j"   'my/window-down)
  (:states 'normal
   :keymaps 'emmet-mode-keymap
   "C-j"   'my/window-down)
  :config
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode)
  
  (setq emmet-move-cursor-between-quotes t)
  (add-hook 'js2-jsx-mode-hook #'my/setup-emmet-for-jsx)
  (add-hook 'js-jsx-mode-hook #'my/setup-emmet-for-jsx)
  (add-hook 'rjsx-mode-hook #'my/setup-emmet-for-jsx))

(defun my/setup-emmet-for-jsx ()
  "Setup emmet for jsx."
  (interactive)
  (setq-local emmet-expand-jsx-className? t)
  (setq-local emmet-self-closing-tag-style " /")
  (emmet-mode 1)
  (message "Enabled emmet"))



(provide 'my-sgml)
;;; my-sgml.el ends here
