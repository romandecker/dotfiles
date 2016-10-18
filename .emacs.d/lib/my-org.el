(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-default-notes-file "~/notes.org"
	org-log-done t)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-beautify-theme
    :ensure t
    :config)
  (use-package evil-org
    :ensure t
    :config
    (evil-define-key 'insert org-mode-map
      (kbd "TAB") 'org-cycle
      [tab]       'org-cycle
    )))

(provide 'my-org)
