(defun my-window-funcs/switch-to-last-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-window-funcs/window-move (action)
  "Check whether the current frame is zoomed, and if yes, unzoom before performing the given ACTION."
  (interactive)
  (when (frame-parameter nil 'zoom-window-enabled) (zoom-window-zoom))
  (funcall action 1))

(defun my-window-funcs/window-down ()
  "Zoom-sensitive window-down move."
  (interactive)
  (my-window-funcs/window-move 'evil-window-down))

(defun my-window-funcs/window-up ()
  "Zoom-sensitive window-up move."
  (interactive)
  (my-window-funcs/window-move 'evil-window-up))

(defun my-window-funcs/window-left ()
  "Zoom-sensitive window-left move."
  (interactive)
  (my-window-funcs/window-move 'evil-window-left))

(defun my-window-funcs/window-right ()
  "Zoom-sensitive window-right move."
  (interactive)
  (my-window-funcs/window-move 'evil-window-right))

(provide 'my-window-funcs)
