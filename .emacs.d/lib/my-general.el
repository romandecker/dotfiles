;;; package --- My custom config for general
;;; Commentary:
;;; Code:


(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (setq my/leader "SPC"
        my/local-leader "C-SPC"
        general-default-keymaps 'evil-normal-state-map)

  (global-unset-key (kbd my/local-leader))

  (define-key help-mode-map (kbd my/leader) nil)

  ;; global keymaps
  (global-set-key (kbd "C-h") 'my/window-left)
  (global-set-key (kbd "C-j") 'my/window-down)
  (global-set-key (kbd "C-k") 'my/window-up)
  (global-set-key (kbd "C-l") 'my/window-right)


  (defmacro my/define-leader-map (&rest body)
    `(general-define-key
      :prefix my/leader
      :keymaps '(normal visual motion help-mode-map compilation-mode-map)
       ,@body))

  ;; leader keymaps
  (my/define-leader-map
   ":"     'helm-M-x
   "~"     'my/toggle-project-term
   "TAB"   'my/switch-to-last-buffer
   "a . ." 'my/open-current-file-with
   "a . d" 'my/open-current-dir-in-explorer
   "a c"   'calculator
   "a p"   'helm-top
   "a P"   'package-list-packages
   "a ~"   'multi-term
   "b b"   'helm-buffers-list
   "b d"   'bury-buffer
   "b c"   'my/switch-to-compilation
   "b m"   'my/switch-to-messages
   "b n"   'next-buffer
   "b p"   'previous-buffer
   "b x"   'kill-this-buffer
   "c c"   'recompile
   "c x"   'kill-compilation
   "j"     'previous-buffer
   "k"     'next-buffer
   "f f"   'helm-find-files
   "e :"   'helm-eval-expression
   "e l"   'eval-last-sexp
   "e b"   'eval-buffer
   "e f"   'eval-defun
   "e r"   'eval-region
   "f r"   'helm-recentf
   "f s"   'save-buffer
   "f d"   'dired-jump
   "i u"   'my/insert-random-uuid
   "o c"   'org-capture
   "o y"   'org-store-link
   "o p"   'org-insert-link
   "t z"   'zoom-window-zoom
   "t n"   'linum-mode
   "t g"   'my/toggle-git-gutter
   "t r"   'read-only-mode
   "t w v" 'toggle-truncate-lines
   "t w w" 'aggressive-fill-paragraph-mode
   "u"     'universal-argument
   "w u"   'winner-undo
   "w C-r" 'winner-redo
   "w o"   'delete-other-windows
   "w q"   'evil-window-delete
   "w z"   'zoom-window-zoom
   "w |"   'split-window-right
   "w -"   'split-window-below
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
   "? v"   'describe-variable)

  (general-evil-define-key 'normal tabulated-list-mode-map
    "g s"   'tabulated-list-sort
    [tab]   'tablist-forward-column
    "S-TAB" 'tablist-backward-column
    "q"     'quit-window))

(use-package which-key
  :ensure t
  :config
  (which-key-add-key-based-replacements
    "SPC TAB"   "Last active buffer"
    "SPC :"     "Execute ex-command"
    "SPC ~"     "Toggle terminal"
    "SPC a"     "Applications"
    "SPC a ."   "Current..."
    "SPC b"     "Buffers"
    "SPC c"     "Compile"
    "SPC e"     "Evaluate"
    "SPC f"     "Files"
    "SPC g"     "Git"
    "SPC i"     "Insert..."
    "SPC o"     "Org"
    "SPC p"     "Projects"
    "SPC t"     "Toggles"
    "SPC t w"   "Wrapping"
    "SPC t w v" "Visual wrapping"
    "SPC w"     "Windows"
    "SPC ."     "Dotfiles"
    "SPC . l"   "Dir/File-local variables"
    "SPC ?"     "Get help")
  (which-key-mode))
  

(provide 'my-general)
;;; my-general.el ends here
