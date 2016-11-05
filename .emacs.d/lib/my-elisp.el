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


(defun my/reload-help ()
  "Use revert-buffer (without confirmation) to reload the help buffer."
  (interactive)
  (message "Reloading...")
  (revert-buffer nil t))

;; some bindings for help-mode
(define-key help-mode-map (kbd "DEL") 'help-go-back)
(define-key help-mode-map [tab] 'forward-button)
(define-key help-mode-map (kbd "r") 'my/reload-help)

(provide 'my-elisp)
;;; my-elisp.el ends here
