;;; package --- My custom modeline config
;;; Commentary:
;;; Code:


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq sml/theme 'automatic)
;;   (sml/setup))



(provide 'my-modeline)
;;; my-modeline.el ends here
