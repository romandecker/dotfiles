;;; package --- My custom config for emacs lisp
;;; Commentary:
;;; Code:
(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode) (eldoc-mode))))

(provide 'my-elisp)
;;; my-elisp.el ends here
