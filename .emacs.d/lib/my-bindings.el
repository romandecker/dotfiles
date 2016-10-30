;;; package --- My custom global bindings
;;; Commentary:
;;; Code:
(provide 'my-bindings)

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    ":"     'helm-M-x
    "~"     'my/toggle-project-term
    "TAB"   'my/switch-to-last-buffer
    "b b"   'helm-buffers-list
    "b n"   'next-buffer
    "b p"   'previous-buffer
    "b d"   'kill-this-buffer
    "b m"   'my/switch-to-messages
    "j"     'previous-buffer
    "k"     'next-buffer
    "f f"   'helm-find-files
    "e l"   'eval-last-sexp
    "e b"   'eval-buffer
    "e e"   'my/eval-current-sexp
    "e f"   'eval-defun
    "e r"   'eval-region
    "f r"   'helm-recentf
    "f s"   'save-buffer
    "f d"   'dired-jump
    "g g"   'my-git/start-time-machine
    "g a"   'magit-stage-file
    "g b"   'magit-commit
    "g c"   'magit-commit
    "g d"   'magit-diff-buffer-file-popup
    "g l"   'magit-log-buffer-file-popup
    "g s"   'magit-status
    "g u"   'magit-unstage-file
    "g h s" 'git-gutter+-stage-hunks
    "g h r" 'git-gutter+-revert-hunk
    "g h h" 'git-gutter+-show-hunk-inline-at-point
    "o c"   'org-capture
    "o y"   'org-store-link
    "o p"   'org-insert-link
    "p c"   'wg-create-workgroup
    "p p"   'wg-switch-to-workgroup
    "p P"   'helm-projectile-switch-project
    "p f"   'helm-projectile-find-file
    "p d"   'helm-projectile-find-dir
    "p /"   'helm-projectile-ag
    "t u"   'undo-tree-visualize
    "t z"   'zoom-window-zoom
    "t n"   'linum-mode
    "t g"   'git-gutter+-toggle-fringe
    "u"     'universal-argument
    "w u"   'winner-undo
    "w C-r" 'winner-redo
    "w o"   'delete-other-windows
    "w q"   'evil-window-delete
    "w r"   'hydra-window-resize/body
    "w z"   'zoom-window-zoom
    "w |"   'split-window-right
    "w -"   'split-window-below
    ". s"   'my/open-snippet-dir
    ". e"   'my/open-dotfile
    ". r"   'my/reload-dotfile
    ". l d" 'add-dir-local-variable
    ". l f" 'add-file-local-variable
    ". l r" 'my/reload-dir-locals
    "? b"   'helm-descbinds
    "? f"   'describe-function
    "? F"   'describe-font
    "? k"   'describe-key
    "? m"   'describe-mode
    "? s"   'yas-describe-tables
    "? v"   'describe-variable)
  (global-evil-leader-mode))

(global-set-key (kbd "C-j") 'my/window-down)
(global-set-key (kbd "C-k") 'my/window-up)
(global-set-key (kbd "C-h") 'my/window-left)
(global-set-key (kbd "C-l") 'my/window-right)

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-l") 'buf-move-right))


(use-package which-key
  :ensure t
  :config
  (which-key-add-key-based-replacements
    "SPC SPC" "Mode-specifc things"
    "SPC TAB" "Last active buffer"
    "SPC :"   "Execute ex-command"
    "SPC ~"   "Toggle terminal"
    "SPC b"   "Buffers"
    "SPC e"   "Evaluate"
    "SPC f"   "Files"
    "SPC g"   "Git"
    "SPC o"   "Org"
    "SPC p"   "Projects"
    "SPC t"   "Toggles"
    "SPC w"   "Windows"
    "SPC ."   "Dotfiles"
    "SPC . l" "Dir/File-local variables"
    "SPC ?"   "Get help")
  (which-key-mode))
;;; my-bindings.el ends here
