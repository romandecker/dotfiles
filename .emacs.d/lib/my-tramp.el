;;; package --- My custom tramp config
;;; Commentary:
;;; Code:

(use-package helm-tramp
  :ensure t
  :config
  (my/define-leader-map
   "T"     'helm-tramp))

(use-package docker-tramp
  :ensure t
  :config)


(provide 'my-tramp)
;;; my-tramp.el ends here
