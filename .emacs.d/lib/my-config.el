(setq
 inhibit-startup-screen t
 x-select-enable-clipboard t
 uniquify-buffer-name-style "post-forward")

; tabs are evil
(setq-default
 indent-tabs-mode nil
 tab-width 2
 tab-stop-list (number-sequence 2 120 2))


(tool-bar-mode -1)     ; disable the tool-bar
(menu-bar-mode -1)     ; disable the menu-bar
(global-linum-mode 1)  ; show line-numbers everywhere
(show-paren-mode)
(electric-pair-mode 1)

(set-default 'truncate-lines t)
(-each '(message-mode-hook)
 (lambda (hook) (add-hook hook (lambda () (message "turning truncate off!")(setq truncate-lines nil)))))

; always wrap around in messages buffer
(with-current-buffer (messages-buffer)
  (setq-local truncate-lines nil))

(provide 'my-config)
