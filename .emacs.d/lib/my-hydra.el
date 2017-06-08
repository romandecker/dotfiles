(use-package hydra
  :ensure t
  :general
  (:prefix my/leader
   :keymaps 'normal
   "w r"   'hydra-window-resize/body
   "z"     'hydra-zoom/body)
  :config
  (defun my/reset-text-scale ()
    (interactive)
    (text-scale-set 0))

  (defhydra hydra-zoom ()
    "Zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" my/reset-text-scale "reset"))

  (defhydra hydra-window-resize ()
    "Window resizing"
    ("j" my/resize-window-down "down")
    ("k" my/resize-window-up "up")
    ("l" my/resize-window-right "right")
    ("h" my/resize-window-left "left")
    ("C-j" evil-window-down  "move down")
    ("C-k" evil-window-up    "move up")
    ("C-l" evil-window-right "move right")
    ("C-h" evil-window-left  "move left")))

(provide 'my-hydra)
