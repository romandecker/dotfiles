;;; package --- My custom global bindings
;;; Commentary:
;;; Code:
(provide 'my-bindings)

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

(global-set-key (kbd "C-j") 'my/window-down)
(global-set-key (kbd "C-k") 'my/window-up)
(global-set-key (kbd "C-h") 'my/window-left)
(global-set-key (kbd "C-l") 'my/window-right)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-l") 'buf-move-right))


;;; my-bindings.el ends here
