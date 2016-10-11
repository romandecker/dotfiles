(require 'my-term-funcs)

(use-package multi-term
  :ensure t
  :config
  (add-hook
   'term-mode-hook
   (lambda ()
     (yas-minor-mode -1)
     (evil-define-key 'normal term-raw-map
       (kbd "C-j") 'my-window-funcs/window-down
       (kbd "C-k") 'my-window-funcs/window-up
       (kbd "C-h") 'my-window-funcs/window-left
       (kbd "C-l") 'my-window-funcs/window-right)
     (evil-define-key 'insert term-raw-map
       (kbd "C-j") 'my-window-funcs/window-down
       (kbd "C-k") 'my-window-funcs/window-up
       (kbd "C-h") 'my-window-funcs/window-left
       (kbd "C-l") 'my-window-funcs/window-right
       (kbd "C-n") 'term-send-down
       (kbd "C-p") 'term-send-up
       (kbd "C-d") 'term-send-eof
       (kbd "C-c") 'my-term-funcs/send-ctrl-c
       (kbd "C-z") 'my-term-funcs/send-ctrl-z
       (kbd "C-w") 'term-send-backward-kill-word
       (kbd "C-r") 'term-send-reverse-search-history
       (kbd "TAB") 'my-term-funcs/send-tab
       [tab]       'my-term-funcs/send-tab
       (kbd "M-.") 'my-term-funcs/send-m-dot
       (kbd "C-]") 'my-term-funcs/send-esc
       (kbd "SPC") 'my-term-funcs/send-space)    ; must use this, or else smart-space overrides space here
   )))


(provide 'my-term)
