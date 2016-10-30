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

(use-package evil-paredit
  :ensure t
  :config
  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "] ]") 'paredit-forward-slurp-sexp
    (kbd "] [") 'paredit-forward-barf-sexp
    (kbd "[ ]") 'paredit-backward-barf-sexp
    (kbd "[ [") 'paredit-backward-slurp-sexp))

(provide 'my-elisp)
;;; my-elisp.el ends here
