(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-default-notes-file "~/Dropbox/org/notes.org"
	org-log-done t)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (message "enabling org-bullets!") (org-bullets-mode 1))))
  (use-package org-beautify-theme
    :ensure t
    :config)
  (use-package evil-org
    :ensure t
    :after evil
    :config
    (add-hook 'org-mode-hook (lambda () (evil-org-mode 1)))

    (define-key evil-normal-state-map (kbd "g x") 'org-open-at-point-global)

    (evil-define-key 'normal org-mode-map
      (kbd "C-j") 'my/window-down
      (kbd "C-k") 'my/window-up
      (kbd "C-h") 'my/window-left
      (kbd "C-l") 'my/window-right
      (kbd "<") 'org-metaleft
      (kbd ">") 'org-metaright))

  (evil-define-key 'insert org-mode-map
    (kbd "TAB") 'org-cycle
    [tab]       'org-cycle))

(provide 'my-org)
