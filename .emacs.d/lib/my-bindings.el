;;; package --- My custom global bindings
;;; Commentary:
;;; Code:
(provide 'my-bindings)

(use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "SPC"   'helm-mini
      ":"     'helm-M-x
      "~"     'my/toggle-project-term
      "TAB"   'my/switch-to-last-buffer
      "b b"   'helm-buffers-list
      "b n"   'next-buffer
      "b p"   'previous-buffer
      "b d"   'kill-this-buffer
      "b m"   (lambda () (interactive) (switch-to-buffer (messages-buffer)))
      "j"     'previous-buffer
      "k"     'next-buffer
      "f f"   'helm-find-files
      "e l"   'eval-last-sexp
      "e b"   'eval-buffer
      "e f"   'eval-defun
      "f r"   'helm-recentf
      "f s"   'save-buffer
      "f d"   'dired-jump
      "g a"   'magit-stage-file
      "g b"   'magit-commit
      "g c"   'magit-commit
      "g d"   'magit-diff-buffer-file-popup
      "g l"   'magit-log-buffer-file-popup
      "g s"   'magit-status
      "g u"   'magit-unstage-file
      "p c"   'wg-create-workgroup
      "p p"   'wg-switch-to-workgroup
      "p f"   'helm-projectile-find-file
      "p d"   'helm-projectile-find-dir
      "p a"   'helm-projectile-ag
      "t u"   'undo-tree-visualize
      "t z"   'zoom-window-zoom
      "w q"   'evil-window-delete
      "w o"   'delete-other-windows
      "w |"   'split-window-right
      "w -"   'split-window-below
      "w r"   'hydra-window-resize/body
      "w z"   'zoom-window-zoom
      ". s"   'my/open-snippet-dir
      ". e"   'my/open-dotfile
      ". r"   'my/reload-dotfile
      ". l d" 'add-dir-local-variable
      ". l f" 'add-file-local-variable
      "? k"   'describe-key
      "? v"   'describe-variable
      "? f"   'describe-function
      "? m"   'describe-mode)
    (global-evil-leader-mode))

(global-set-key (kbd "C-j") 'my/window-down)
(global-set-key (kbd "C-k") 'my/window-up)
(global-set-key (kbd "C-h") 'my/window-left)
(global-set-key (kbd "C-l") 'my/window-right)

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
    "SPC g"   "Git"
    "SPC p"   "Projects"
    "SPC t"   "Toggles"
    "SPC w"   "Windows"
    "SPC ."   "Dotfiles"
    "SPC . l" "Dir/File-local variables"
    "SPC ?"   "Get help")
  (which-key-mode))
;;; my-bindings.el ends here
