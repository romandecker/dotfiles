;;; package --- My custom config for project-stuff
;;; Commentary:
;;; Code:
(require 'my-utils)
(when (file-exists-p my/custom-file)
  (load my/custom-file))

(use-package projectile
  :ensure t
  :config
  (use-package workgroups
    :ensure t
    :config
    (setq wg-morph-on nil)
    (workgroups-mode 1)
    (when (file-exists-p my/workgroups-file)
      (wg-load my/workgroups-file))
    (add-hook 'kill-emacs-hook #'my/save-workgroups))
  (projectile-mode 1))

(defun my/save-workgroups ()
  (when (wg-list t)
    (wg-update-all-workgroups-and-save)))


(provide 'my-projectile)
;;; my-projectile.el ends here
