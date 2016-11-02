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
        helm-completion-in-region-fuzzy-match t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 1)
  (helm-autoresize-mode t)
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
    :config)
  (use-package helm-swoop
    :ensure t
    :config
    (define-key helm-swoop-map (kbd "C-w") 'backward-kill-word)
    (define-key helm-swoop-map (kbd "C-c e") 'helm-swoop-edit)
    (define-key evil-normal-state-map (kbd "g *") 'helm-swoop)
    (define-key evil-normal-state-map (kbd "g /") 'helm-swoop-without-pre-input)
    (define-key helm-swoop-edit-map (kbd "C-c C-c") 'helm-swoop--edit-complete)
    (define-key helm-swoop-edit-map (kbd "C-c C-g") 'helm-swoop--edit-cancel)
    (advice-add 'helm-swoop--edit
                :after #'evil-mc-mode)))

(defvar my/helm-action-return-candidate
  (helm-make-actions "Select" (lambda (candidate) candidate)))

(provide 'my-helm)
;;; my-helm.el ends here
