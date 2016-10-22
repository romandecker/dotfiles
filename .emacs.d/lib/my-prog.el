;;; package --- My custom config for general programming stuff
;;; Commentary:
;;; Code:
(use-package highlight-symbol
  :ensure t
  :after evil
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (define-key evil-normal-state-map (kbd "] o") 'highlight-symbol-next)
  (define-key evil-normal-state-map (kbd "[ o") 'highlight-symbol-prev))

(use-package indent-guide
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'indent-guide-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(provide 'my-prog)
;;; my-prog.el ends here
