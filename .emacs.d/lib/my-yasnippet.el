(require 'my-funcs)

(use-package yasnippet
  :ensure t
  :config
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'my-funcs/tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'my-funcs/tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'my-funcs/abort-company-or-yas)

  (yas-global-mode 1))

(provide 'my-yasnippet)
