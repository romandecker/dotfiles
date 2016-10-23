;;; package --- My custom powerline config
;;; Commentary:
;;; Code:
(use-package powerline
  :ensure t
  :config
  (if (display-graphic-p)
      (progn
	(setq powerline-default-separator 'contour
	      powerline-height 25))
    (setq powerline-default-separator-dir '(right . left)))
  (powerline-vim-theme))

(provide 'my-powerline)
;;; my-powerline.el ends here
