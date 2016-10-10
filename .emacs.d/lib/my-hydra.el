(require 'my-funcs)

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom ()
    "Zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

  (defhydra hydra-window-resize ()
    "Window resizing"
    ("j" my-funcs/resize-window-down "down")
    ("k" my-funcs/resize-window-up "up")
    ("l" my-funcs/resize-window-right "right")
    ("h" my-funcs/resize-window-left "left")))

(provide 'my-hydra)
