;;; package --- My custom config for emacs lisp
;;; Commentary:
;;; Code:
(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode) (eldoc-mode))))

(defun my/eval-current-sexp ()
  (interactive)
  (save-excursion
    (paredit-forward-up)
    (eval-last-sexp nil)))

(provide 'my-elisp)
;;; my-elisp.el ends here
