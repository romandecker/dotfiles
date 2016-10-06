(require 'my-term-funcs)

(use-package multi-term
  :ensure t
  :config
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

(add-hook 'term-mode-hook
	  (lambda ()
	    (evil-define-key 'normal term-raw-map
	      (kbd "\C-j") 'evil-window-down
	      (kbd "\C-k") 'evil-window-up
	      (kbd "p") 'term-paste)
	    (evil-define-key 'insert term-raw-map
	      (kbd "\C-j") 'evil-window-down
	      (kbd "\C-k") 'evil-window-up
	      (kbd "\C-a") 'my-term-funcs/send-ctrl-a
	      (kbd "\C-e") 'my-term-funcs/send-ctrl-e
	      (kbd "\C-r") 'my-term-funcs/send-ctrl-r
	      (kbd "\C-p") 'my-term-funcs/send-ctrl-p
	      (kbd "\C-n") 'my-term-funcs/send-ctrl-n
	      (kbd "\C-c") 'my-term-funcs/send-ctrl-c
	      (kbd "\C-d") 'my-term-funcs/send-ctrl-d
	      (kbd "\C-z") 'my-term-funcs/send-ctrl-z
	      (kbd "SPC")  'my-term-funcs/send-space    ; must use this, or else smart-space overrides space here
	      (kbd "TAB")  'my-term-funcs/send-tab
	      [tab]        'my-term-funcs/send-tab
	      (kbd "\C-w") 'term-send-backward-kill-word)))

(provide 'my-term)
