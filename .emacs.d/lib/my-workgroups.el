;;; package --- My custom workgroups config
;;; Commentary:
;;; Code:

(defcustom projects-dir
  "~/projects"
  "The base directory where most of your projects are located")

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
   "p d"   'my/find-project-root
   "p ."   'my/goto-dotfiles-project
   "p SPC" 'my/goto-projects-dir
   )
  :config
  (setq wg-morph-on nil
        wg-use-faces nil)
  (add-hook 'kill-emacs-hook #'my/save-workgroups)
  (workgroups-mode 1)

  (defun my/goto-projects-dir ()
    (interactive)
    (find-file projects-dir))

  (defun my/create-workgroup ()
    (interactive)
    (call-interactively 'wg-create-workgroup)
    (let ((project-name (wg-get-workgroup-prop 'name (wg-current-workgroup))))
          (my/goto-project-directory project-name)))

  (defun my/goto-project-directory (project-name)
    (let ((project-dir (concat
                         (file-name-as-directory project-dir)
                         project-name)))
      (when (file-exists-p project-dir)
        (find-file project-dir))))

  (defun my/goto-dotfiles-project ()
    (interactive)
    (wg-switch-to-workgroup (wg-get-workgroup 'name "dotfiles")))

  (defun my/save-workgroups ()
    (when (wg-list t)
      (wg-update-all-workgroups-and-save))
    (with-temp-file my/last-workgroup-file
      (insert (wg-get-workgroup-prop 'name (wg-current-workgroup)))))

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
    (wg-load my/workgroups-file))


  (when (file-exists-p my/last-workgroup-file)
    (with-temp-buffer
      (insert-file-contents my/last-workgroup-file)
      (wg-switch-to-workgroup (wg-get-workgroup 'name (buffer-string))))))


(provide 'my-workgroups)
;;; my-workgroups.el ends here
