;;; package --- Various visual tweaks
;;; Commentary:
;;; Code:
(use-package beacon
  :ensure t
  :defer 6
  :config
  (setq beacon-color "#cccccc")
  (beacon-mode 1))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook #'rainbow-mode)
  (add-hook 'emacs-lisp-mode #'rainbow-mode)
  (add-hook 'css-mode #'rainbow-mode)
  (add-hook 'html-mode #'rainbow-mode)
  (add-hook 'web-mode #'rainbow-mode))

(provide 'my-visuals)
;;; my-visuals.el ends here
