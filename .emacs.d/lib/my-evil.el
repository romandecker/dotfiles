(require 'my-funcs)
(require 'my-window-funcs)

(use-package evil
  :ensure t
  :config
  (define-key evil-insert-state-map (kbd "SPC") 'my-funcs/smart-space)
  (define-key evil-insert-state-map (kbd "DEL") 'my-funcs/smart-delete)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'insert company-active-map
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous)
  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "; l") 'eval-last-sexp
    (kbd "; ;") 'eval-buffer
    (kbd "; f") 'eval-defun
    (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'my-funcs/dired-up-directory
    (kbd "RET") 'dired-find-alternate-file
    (kbd "l") 'dired-find-alternate-file
    (kbd "m") 'dired-mark
    (kbd "u") 'dired-unmark
    (kbd "U") 'dired-unmark-all-marks
    (kbd "C") 'dired-create-directory
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous
    (kbd "q") 'kill-this-buffer)
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
    (global-evil-mc-mode 1))
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      ":" 'helm-M-x
      "~" 'my-term-funcs/toggle-term
      "TAB" 'my-window-funcs/switch-to-last-buffer
      "b b" 'helm-buffers-list
      "b n" 'next-buffer
      "b p" 'previous-buffer
      "f f" 'helm-find-files
      "f r" 'helm-recentf
      "f s" 'save-buffer
      "f d" 'dired-jump
      "p p" 'helm-projectile-switch-project
      "p f" 'helm-projectile-find-file-dwim
      "p d" 'helm-projectile-find-dir
      "p a" 'helm-projectile-ag
      "t u" 'undo-tree-visualize
      "w q" 'evil-window-delete
      "w o" 'delete-other-windows
      "w |" 'split-window-right
      "w -" 'split-window-below
      "z"   'zoom-window-zoom
      ". e" 'my-funcs/open-dotfile
      ". r" 'my-funcs/reload-dotfile
      "? k" 'describe-key
      "? v" 'describe-variable
      "? f" 'describe-function
      "? m" 'describe-mode)
    (global-set-key (kbd "C-j") 'my-window-funcs/window-down)
    (global-set-key (kbd "C-k") 'my-window-funcs/window-up)
    (global-set-key (kbd "C-h") 'my-window-funcs/window-left)
    (global-set-key (kbd "C-l") 'my-window-funcs/window-right)
    (global-evil-leader-mode))
  (evil-mode 1)) ; evil-leader must be enabled before evil

(provide 'my-evil)
