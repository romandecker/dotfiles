(require 'my-funcs)
(require 'my-window-funcs)

(use-package evil
  :ensure t
  :config
  (define-key evil-insert-state-map (kbd "SPC") 'my-funcs/smart-space)
  (define-key evil-insert-state-map (kbd "DEL") 'my-funcs/smart-delete)
  (define-key evil-insert-state-map [tab] 'my-funcs/tab-indent-or-complete)
  ; (define-key evil-insert-state-map (kbd "TAB") 'my-funcs/tab-indent-or-complete)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)
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
    (kbd "y") 'dired-do-copy
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
  (use-package evil-exchange
    :ensure t
    :config
    (setq evil-exchange-key (kbd "gx"))
    (evil-exchange-install))
  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))
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
      "b d" 'kill-this-buffer
      "f f" 'helm-find-files
      "e l" 'eval-last-sexp
      "e b" 'eval-buffer
      "e f" 'eval-defun
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
      "w r" 'hydra-window-resize/body
      "z"   'zoom-window-zoom
      ". s" 'my-funcs/open-snippet-dir
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

(use-package which-key
  :ensure t
  :config
  (which-key-add-key-based-replacements
    "SPC TAB" "Last active buffer"
    "SPC :"   "Execute ex-command"
    "SPC ~"   "Toggle terminal"
    "SPC b"   "Buffers"
    "SPC e"   "Evaluate"
    "SPC f"   "Files"
    "SPC p"   "Projects"
    "SPC t"   "Toggles"
    "SPC w"   "Windows"
    "SPC ."   "Dotfiles"
    "SPC ?"   "Get help")
  (which-key-mode))


(provide 'my-evil)
