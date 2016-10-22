;;; package --- My custom helm config
;;; Commentary:
;;; Code:
(use-package helm
  :ensure t
  :config
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action) ; complete with tab
  (setq helm-mode-fuzzy-match t
	helm-completion-in-region-fuzzy-match t)
  (helm-mode 1)
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on))
  (use-package ag
    :ensure t
    :config
    (use-package helm-ag
      :ensure t
      :config
      (advice-add 'helm-ag--edit
		  :after #'evil-mc-mode)))
  (use-package helm-descbinds
    :ensure t
    :config))

(provide 'my-helm)
;;; my-helm.el ends here
