;;; package --- My custom config for project-stuff
;;; Commentary:
;;; Code:
(require 'my-utils)

(use-package projectile
  :ensure t
  :demand t
  :general
  (:prefix my/leader
   :states 'normal
   "c p"   'projectile-compile-project)
  :config
  (add-hook 'projectile-after-switch-project-hook #'my/set-yas-snippet-dirs)
  (add-hook 'projectile-after-switch-project-hook #'my/set-bookmark-file)
  (projectile-mode 1)

  (defun my/find-project-root ()
    (interactive)
    (find-file (projectile-project-root)))

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

  (defun my/set-bookmark-file ()
    "Set the currently active bookmark file depending on the current project.
If no project is active, sets it to `bookmark-default-file'."
    (bmkp-switch-bookmark-file-create
     (if (projectile-project-p)
         (concat (projectile-project-root) my/local-bookmarks-file)
       bookmark-default-file))))


(defconst my/global-yas-snippet-dirs '("~/.emacs.d/snippets"))
(defconst my/local-snippet-dir (concat my/local-emacs-dir "snippets"))


(defconst my/local-bookmarks-file (concat my/local-emacs-dir "bookmarks"))


(provide 'my-projectile)
;;; my-projectile.el ends here
