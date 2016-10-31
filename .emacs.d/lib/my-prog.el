;;; package --- My custom config for general programming stuff
;;; Commentary:
;;; Code:
(use-package highlight-symbol
  :ensure t
  :after evil
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'hPermissionighlight-symbol-mode)
  (define-key evil-normal-state-map (kbd "] o") 'highlight-symbol-next)
  (define-key evil-normal-state-map (kbd "[ o") 'highlight-symbol-prev))

(use-package indent-guide
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'indent-guide-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'whitespace-cleanup-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  )

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode)

(defun my/hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook #'my/hide-trailing-whitespace)
(add-hook 'helm-mode-hook #'my/hide-trailing-whitespace)
(add-hook 'term-mode-hook #'my/hide-trailing-whitespace)

(add-hook 'prog-mode-hook #'linum-mode)

(provide 'my-prog)
;;; my-prog.el ends here
