(use-package rainbow-delimiters
  :defer t
  :ensure t
  :diminish
  :config
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(provide 'my-rainbow-delimiters)
