(use-package evil
  :ensure t
  :config
  (define-key evil-insert-state-map (kbd "SPC") 'my/smart-space)
  (define-key evil-insert-state-map (kbd "DEL") 'my/smart-delete)
  (define-key evil-insert-state-map [tab] 'my/tab-indent-or-complete)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)
  ; (define-key evil-insert-state-map (kbd "TAB") 'my/tab-indent-or-complete)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))
  (use-package evil-args
    :ensure t
    :config
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))
  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))
  (use-package evil-mc
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-n") 'my/smart-c-n)
    (evil-define-key 'normal evil-mc-key-map (kbd "C-n") 'my/smart-c-n)
    (global-evil-mc-mode 1))
  (use-package evil-exchange
    :ensure t
    :config
    (setq evil-exchange-key (kbd "gx"))
    (evil-exchange-install))
  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))
  (require 'my-bindings)
  (evil-mode 1)) ; evil-leader must be enabled before evil



(provide 'my-evil)
