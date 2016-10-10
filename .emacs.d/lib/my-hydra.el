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
    ("k" shrink-window "up")
    ("l" enlarge-window-horizontally "right")
    ("k" shrink-window "left")
    )
  )

(defun my-funcs/resize-window-down ()
  "Resize a window downwards."
  (interactive)
  (cond
   ((window-in-direction 'below) (enlarge-window 1)
    (window-in-direction 'above) (shrink-window 1)))
  )

(provide 'my-hydra)
