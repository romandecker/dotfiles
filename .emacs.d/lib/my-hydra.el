(use-package hydra
  :ensure t
  :general
  (:prefix my/leader
   :keymaps 'normal
   "w r"   'hydra-window-resize/body
   "z"     'hydra-zoom/body)
  :config
  (defhydra hydra-zoom ()
    "Zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (lambda () (text-scale-set 0) "reset")))

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
