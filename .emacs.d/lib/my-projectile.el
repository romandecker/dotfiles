;;; package --- My custom config for project-stuff
;;; Commentary:
;;; Code:
(defconst my-funcs/workgroups-file "~/.emacs.d/workgroups")

(use-package projectile
  :ensure t
  :config
  (use-package workgroups
    :ensure t
    :config
    (setq wg-morph-on nil)
    (workgroups-mode 1)
    (when (file-exists-p my-funcs/workgroups-file)
      (wg-load my-funcs/workgroups-file))
    (add-hook 'kill-emacs-hook #'my-funcs/save-workgroups))
  (projectile-mode 1))

(defun my-funcs/save-workgroups ()
  (wg-update-all-workgroups-and-save))


(provide 'my-projectile)
;;; my-projectile.el ends here
