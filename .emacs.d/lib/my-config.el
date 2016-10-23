(setq
 inhibit-startup-screen t
 x-select-enable-clipboard t
 uniquify-buffer-name-style "post-forward"

 ;; default values for indentation (possibly overwritten by editorconfig)
 js2-basic-offset 2
 js-indent-level 2
 js-expr-indent-offset -2)

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
