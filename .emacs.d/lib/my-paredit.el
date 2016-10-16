;;; package --- My custom paredit config
;;; Commentary:
;;; Code:
(use-package evil-paredit
  :ensure t
  :config
  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "] ]") 'paredit-forward-slurp-sexp
    (kbd "] [") 'paredit-forward-barf-sexp
    (kbd "[ ]") 'paredit-backward-barf-sexp
    (kbd "[ [") 'paredit-backward-slurp-sexp))

(provide 'my-paredit)
;;; my-paredit.el ends here
