(setq
 inhibit-startup-screen t
 x-select-enable-clipboard t
 visible-bell t
 ring-bell-function 'ignore)

; tabs are evil
(setq-default
 indent-tabs-mode nil
 tab-width 2
 tab-stop-list (number-sequence 2 120 2)
 indent-tabs-mode nil)

(tool-bar-mode -1)     ; disable the tool-bar
(menu-bar-mode -1)     ; disable the menu-bar
(show-paren-mode)
(electric-pair-mode 1)
(winner-mode 1)

;; make the default font smaller
(set-face-attribute 'default nil :height 115)

;; use y or n everywhere cause it's shorter to type
(defalias 'yes-or-no-p 'y-or-n-p)

(set-default 'truncate-lines t)
(-each '(message-mode-hook)
 (lambda (hook) (add-hook hook
                          (lambda ()
                            (message "turning truncate off!")
                            (setq truncate-lines nil)))))


; always wrap around in messages buffer
(add-hook 'messages-buffer-mode-hook (lambda ()
                                       (setq-local truncate-lines nil)))

(defconst my/dotfile "~/.emacs.d/init.el")
(defconst my/workgroups-file (expand-file-name "~/.emacs.d/workgroups"))
(defconst my/local-emacs-dir ".emacs.local/")

(provide 'my-config)
