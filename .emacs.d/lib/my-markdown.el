(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config)

(use-package grip-mode
  :ensure t
  :general
  (:prefix my/local-leader
   :states 'normal
   :keymaps 'markdown-mode-map
   "p" 'grip-mode)
  :config
  (setq grip-github-user "DeX3"))

(provide 'my-markdown)
