(defconst my-funcs/workgroups-file "~/.emacs.d/workgroups")

(use-package projectile
  :ensure t
  :config
  (use-package workgroups
    :ensure t
    :config
    (wg-load my-funcs/workgroups-file)
    (workgroups-mode 1))
  (setq
   projectile-switch-project-action 'my-funcs/restore-project)
  (projectile-global-mode))

(add-hook 'projectile-before-switch-project-hook
	  'my-funcs/before-switch-project)

(defun my-funcs/before-switch-project ()
  (when (wg-current-workgroup t)
    (wg-save my-funcs/workgroups-file)))

(defun my-funcs/restore-project ()
  "Try to restore a project. Used as a handler for
`projectile-switch-project-action'. If a workgroup for the project being switched
to is found, the workgroup is restored. If no workgroup exists for the current
project, the user is prompted with a file-open-dialog for the project and a new
workgroup is created for the project."
  (let* ((name (projectile-project-name))
	 (wg (wg-get-workgroup 'name name t))) ; get the current workgroup (or nil)
    (if wg                              ; if wg exists, switch to it
	(wg-switch-to-workgroup wg)
      ; else, show the open-file dialog
      (helm-projectile-find-file)    ; go ahead and open the project
      ; new file is open now, remember it (wg-create-workgroup creates a blank
      ; layout)
      (let ((buf (current-buffer)))
	(wg-create-workgroup name)
	(switch-to-buffer buf)   ; switch back to the new buffer
	(wg-update-workgroup))   ; update the workgroup
      (wg-save my-funcs/workgroups-file))))


(provide 'my-projectile)
