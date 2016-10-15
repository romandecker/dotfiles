(require 'dired-x)

(use-package dired-k
  :ensure t
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-k-human-readable t)
  (add-hook 'dired-initial-position-hook #'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(provide 'my-dired)
