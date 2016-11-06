;;; package --- My custom config for project-stuff
;;; Commentary:
;;; Code:
(require 'my-utils)

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

(defun my/before-switch-workgroup (workgroup &optional base)
  "Save the old workgroup before switching, so that it can later be switched to
more easily."
  (setq my/last-workgroup (wg-current-workgroup t)))

(advice-add 'wg-switch-to-workgroup :before #'my/before-switch-workgroup)

(defun my/switch-to-last-workgroup ()
  (interactive)
  (if my/last-workgroup
      (wg-switch-to-workgroup my/last-workgroup)
    (message "No last workgroup!")))

(defconst my/global-yas-snippet-dirs '("~/.emacs.d/snippets"))
(defconst my/local-snippet-dir (concat my/local-emacs-dir "snippets"))

(defun my/set-yas-snippet-dirs ()
  "Make sure that yas-snippet-dirs always also contains the snippets from the
currently active project."
  (setq
   yas-snippet-dirs
   (if (projectile-project-p)
       (cons
        (concat (projectile-project-root) my/local-snippet-dir)
        my/global-yas-snippet-dirs)
     my/global-yas-snippet-dirs))
  (yas-reload-all))

(defconst my/local-bookmarks-file (concat my/local-emacs-dir "bookmarks"))

(defun my/set-bookmark-file ()
  "Set the currently active bookmark file depending on the current project.
If no project is active, sets it to `bookmark-default-file'."
  (bmkp-switch-bookmark-file-create
   (if (projectile-project-p)
       (concat (projectile-project-root) my/local-bookmarks-file)
     bookmark-default-file)))

(use-package projectile
  :ensure t
  :config
  (use-package workgroups
    :ensure t
    :config
    (setq wg-morph-on nil
          wg-use-faces nil)
    (add-hook 'wg-switch-hook #'my/wg-switch-hook)
    (workgroups-mode 1)
    (when (file-exists-p my/workgroups-file)
      (wg-load my/workgroups-file))
    (add-hook 'kill-emacs-hook #'my/save-workgroups)
    (evil-leader/set-key
        "p -"   'my/switch-to-last-workgroup
        "p TAB" 'my/switch-to-last-workgroup
        "p c"   'wg-create-workgroup
        "p p"   'wg-switch-to-workgroup
        "p r"   'projectile-compile-project
        "p d"   'my/find-project-root))
  (add-hook 'projectile-after-switch-project-hook #'my/set-yas-snippet-dirs)
  (add-hook 'projectile-after-switch-project-hook #'my/set-bookmark-file)
  (projectile-mode 1))


(defun my/find-project-root ()
  (interactive)
  (find-file (projectile-project-root)))

(defun my/save-workgroups ()
  (when (wg-list t)
    (wg-update-all-workgroups-and-save)))

(provide 'my-projectile)
;;; my-projectile.el ends here
