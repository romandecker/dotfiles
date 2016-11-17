;;; package --- My custom helm config
;;; Commentary:
;;; Code:
(use-package helm
  :ensure t
  :config
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-d") 'helm-next-page)
  (define-key helm-map (kbd "C-u") 'helm-previous-page)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action) ; complete with tab
  (define-key evil-normal-state-map (kbd "g p") 'helm-show-kill-ring)
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 1)
  (helm-autoresize-mode t)
  (helm-mode 1)
  (use-package helm-projectile
    :ensure t
    :config
    (evil-leader/set-key
        "p P"   'helm-projectile-switch-project
        "p f"   'helm-projectile-find-file
        "p d"   'helm-projectile-find-dir
        "p /"   'helm-projectile-ag)
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

;; Replace helm-help-event-loop with own implementation that uses more
;; evil bindings
(eval-after-load "helm-lib"
  '(defun helm-help-event-loop ()
    (let ((prompt (propertize
                   "[SPC,C-v,down,next:NextPage  b,M-v,up,prior:PrevPage C-s/r:Isearch q:Quit]"
                   'face 'helm-helper))
          scroll-error-top-bottom)
      (helm-awhile (read-key prompt)
        (cl-case it
          ((?\C-d ? down next) (helm-help-scroll-up helm-scroll-amount))
          ((?\C-u ?b up prior) (helm-help-scroll-down helm-scroll-amount))
          (?\C-s (isearch-forward))
          (?\C-r (isearch-backward))
          (?0    (call-interactively #'move-beginning-of-line))
          (?\C-f (call-interactively #'forward-char))
          (?\C-b (call-interactively #'backward-char))
          (?\C-n (helm-help-next-line))
          (?\C-p (helm-help-previous-line))
          (?\M-a (call-interactively #'backward-sentence))
          (?\M-e (call-interactively #'forward-sentence))
          (?\M-f (call-interactively #'forward-word))
          (?\M-b (call-interactively #'backward-word))
          (?\C-  (helm-help-toggle-mark))
          (?\M-w (copy-region-as-kill
                  (region-beginning) (region-end))
                 (deactivate-mark))
          ;; new bindings added here:
          (?j    (helm-help-next-line))
          (?k    (helm-help-previous-line))
          (?\C-e (evil-scroll-line-down 1))
          (?\C-y (evil-scroll-line-up 1))
          (?g    (evil-goto-first-line))
          (?G    (evil-goto-line))
          (?/    (evil-search-forward))
          (?n    (evil-search-next))
          (?N    (evil-search-previous))
          ;; end of new bindings
          (?q    (cl-return))
          (t     (ignore)))))))

(provide 'my-helm)
;;; my-helm.el ends here
