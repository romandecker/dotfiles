;;; package --- Basic general-purpose config options
;;; Commentary:
;;; Code:
(setq
 inhibit-startup-screen t
 x-select-enable-clipboard t
 visible-bell t
 ring-bell-function 'ignore
 use-dialog-box nil
 gc-cons-threshold 50000000)

; tabs are evil
(setq-default
 indent-tabs-mode nil
 tab-width 2
 evil-shift-width 2
 tab-stop-list (number-sequence 2 120 2)
 indent-tabs-mode nil
 indicate-empty-lines t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

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

(defconst my/dotfile "~/.dotfiles/.emacs.d/init.el")
(defconst my/workgroups-file (expand-file-name "~/.dotfiles/.emacs.d/workgroups"))
(defconst my/last-workgroup-file (expand-file-name "~/.dotfiles/.emacs.d/last-workgroup"))
(defconst my/local-emacs-dir ".emacs.local/")

(provide 'my-config)
;;; my-config.el ends here
