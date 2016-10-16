(use-package org
  :ensure t
  :config
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-beautify-theme
    :ensure t
    :config)
  (require 'evil-org))

(provide 'my-org)
