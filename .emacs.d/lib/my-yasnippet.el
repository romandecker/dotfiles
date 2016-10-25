(use-package yasnippet
  :ensure t
  :after company
  :config
  (setq
   yas-snippet-dirs "~/.emacs.d/snippets")
  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
  (add-to-list 'yas-key-syntaxes 'yas-longest-key-from-newline)

  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'my/tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'my/tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'my/abort-company-or-yas)

  (yas-global-mode 1)
  (message "yasnippet has loaded!"))


(provide 'my-yasnippet)

(defun yas-longest-key-from-newline (start-point)
  "Look for snippet keys between point and last newline, longer first."
  (if (= (point) start-point)
      (skip-chars-backward "^\n")
    (forward-char))
  (unless (<= start-point (1+ (point)))
    'again))
