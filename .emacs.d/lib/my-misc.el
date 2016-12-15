;;; package --- Additional packages that don't fit anywhere else
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package restart-emacs
  :ensure t
  :general
  (general-define-key
   :prefix my/leader
   :keymaps '(normal visual)
    "Q R"   'restart-emacs
    "Q Q" 'kill-emacs)
  :config
  (which-key-add-key-based-replacements
    "SPC Q"   "Quitting"))

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-l") 'buf-move-right))

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

(provide 'my-misc)
;;; my-misc.el ends here
