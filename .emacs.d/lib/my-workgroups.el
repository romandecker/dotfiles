;;; package --- My custom workgroups config
;;; Commentary:
;;; Code:

(use-package workgroups
  :ensure t
  :demand t
  :after projectile
  :general
  (:prefix my/leader
   :keymaps 'normal
   "p -"   'my/switch-to-last-workgroup
   "p TAB" 'my/switch-to-last-workgroup
   "p c"   'wg-create-workgroup
   "p p"   'wg-switch-to-workgroup
   "p r"   'projectile-compile-project
   "p d"   'my/find-project-root)
  :config
  (setq wg-morph-on nil
        wg-use-faces nil)
  ;; (add-hook 'wg-switch-hook #'my/wg-switch-hook)
  (add-hook 'kill-emacs-hook #'my/save-workgroups)
  (workgroups-mode 1)

  (defun my/save-workgroups ()
    (when (wg-list t)
      (wg-update-all-workgroups-and-save)))

  (defun my/wg-switch-hook ()
    "Installed as a hook for wg-switch-hook.  When we land on an 'empty' project
  after switch, open projectile's switch dialog with the workgroup's
  name preselected."
    (when (string= "*scratch*" (buffer-name (current-buffer)))
      (helm
      :sources '(helm-source-projectile-projects)
      :input (wg-name (wg-current-workgroup))))
    (my/set-yas-snippet-dirs)
    (my/set-bookmark-file))

  (defvar my/last-workgroup nil)
  (defun my/switch-to-last-workgroup ()
    (interactive)
    (if my/last-workgroup
        (wg-switch-to-workgroup my/last-workgroup)
      (message "No last workgroup!")))
  (defun my/before-switch-workgroup (workgroup &optional base)
    "Save the old workgroup before switching, so that it can later be switched to
  more easily."
    (setq my/last-workgroup (wg-current-workgroup t)))

  (advice-add 'wg-switch-to-workgroup :before #'my/before-switch-workgroup)

  (when (file-exists-p my/workgroups-file)
    (wg-load my/workgroups-file)))

;; (use-package persp-mode
;;   :ensure t
;;   :general
;;   (:prefix my/leader
;;    :keymaps 'normal
;;    "p p" 'persp-switch
;;    "p . r" 'persp-rename)
;;   :config)

(provide 'my-workgroups)
;;; my-workgroups.el ends here
