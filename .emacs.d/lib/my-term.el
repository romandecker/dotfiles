(require 'my-term-funcs)

(add-hook 'term-mode-hook
          (lambda ()
            (evil-define-key 'insert term-raw-map (kbd "\C-j") 'evil-window-down)
            (evil-define-key 'insert term-raw-map (kbd "\C-k") 'evil-window-up)
            (evil-define-key 'normal term-raw-map (kbd "\C-j") 'evil-window-down)
            (evil-define-key 'normal term-raw-map (kbd "\C-k") 'evil-window-up)
            (evil-define-key 'insert term-raw-map (kbd "\C-a") 'my-term-funcs/send-ctrl-a)
            (evil-define-key 'insert term-raw-map (kbd "\C-e") 'my-term-funcs/send-ctrl-e)
            (evil-define-key 'insert term-raw-map (kbd "\C-r") 'my-term-funcs/send-ctrl-r)
            (evil-define-key 'insert term-raw-map (kbd "\C-p") 'my-term-funcs/send-ctrl-p)
            (evil-define-key 'insert term-raw-map (kbd "\C-n") 'my-term-funcs/send-ctrl-n)
            (evil-define-key 'insert term-raw-map (kbd "\C-c") 'my-term-funcs/send-ctrl-c)
            (evil-define-key 'insert term-raw-map (kbd "\C-d") 'my-term-funcs/send-ctrl-d)
            (evil-define-key 'insert term-raw-map (kbd "\C-z") 'my-term-funcs/send-ctrl-z)
            (evil-define-key 'insert term-raw-map (kbd "\C-w") 'term-send-backward-kill-word)))

(provide 'my-term)
