;;; package --- My custom helm config
;;; Commentary:
;;; Code:


(defcustom my/helm-projectile-args nil "Custom arguments to be passed to ag when called via helm-projectile.")

(defun my/helm-projectile-ag ()
  "Use this in place of `helm-projectile-ag' to make sure it respects the custom var `my/helm-projectile-args'"
  (interactive)
  (helm-projectile-ag my/helm-projectile-args))


(use-package helm
  :ensure t
  :demand t
  :general
  (:keymaps 'helm-map
   "C-j" 'helm-next-line
   "C-k" 'helm-previous-line
   "C-d" 'helm-next-page
   "C-u" 'helm-previous-page
   "C-w" 'backward-kill-word
   "TAB" 'helm-execute-persistent-action) ; complete with tab
  (:states 'normal
   "g p" 'helm-show-kill-ring)
  :config
  (setq helm-mode-fuzzy-match t helm-completion-in-region-fuzzy-match t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 1)
  (helm-autoresize-mode t)
  (helm-mode 1)

  (use-package helm-projectile
    :ensure t
    :demand t
    :general
    (:prefix my/leader
     "p P"   'helm-projectile-switch-project
     "p f"   'helm-projectile-find-file
     "p d"   'helm-projectile-find-dir
     "p /"   'my/helm-projectile-ag)
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
    :general
    (:keymaps 'helm-swoop-map
     "C-w"   'backward-kill-word
     "C-c e" 'helm-swoop-edit)
    (:keymaps 'helm-swoop-edit-map
     "C-c C-c" 'helm-swoop--edit-complete
     "C-c C-g" 'helm-swoop--edit-cancel)
    (:states 'normal
     "g *" 'helm-swoop
     "g /" 'helm-swoop-without-pre-input)
    :config
    (advice-add 'helm-swoop--edit
                :after #'evil-mc-mode))

  (use-package helm-dash
    :ensure t
    :config)

  (defvar my/helm-action-return-candidate
    (helm-make-actions "Select" (lambda (candidate) candidate))))


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
