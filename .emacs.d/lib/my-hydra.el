(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom ()
    "Zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

  (defhydra hydra-window-resize ()
    "Window resizing"
    ("j" my/resize-window-down "down")
    ("k" my/resize-window-up "up")
    ("l" my/resize-window-right "right")
    ("h" my/resize-window-left "left")))

(provide 'my-hydra)
