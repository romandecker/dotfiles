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
         (let ((full-snippet-path) (concat (projectile-project-root) my/local-snippet-dir))
           (if (file-exists-p full-snippet-path)
               (cons full-snippet-path my/global-yas-snippet-dirs)
             my/global-yas-snippet-dirs))
       my/global-yas-snippet-dirs))
    (yas-reload-all))

  (defun my/set-bookmark-file ()
    "Set the currently active bookmark file depending on the current project.
If no project is active, sets it to `bookmark-default-file'."
    (bmkp-switch-bookmark-file-create
     (if (projectile-project-p)
         (concat (projectile-project-root) my/local-bookmarks-file)
       bookmark-default-file)))

  (defun my/projectile-switch-buffer (nav-fn)
    (if (projectile-project-p)
        (let* ((last-root (projectile-project-root))
               (last-buffer (current-buffer)))
          (funcall nav-fn)
          (while (not (projectile-project-buffer-p (current-buffer) last-root))
            (funcall nav-fn))
          (message "Next available project buffer: %s (%s)" (current-buffer) (projectile-project-root)))

      (message "Not in a project falling back to normal function")
      (funcall nav-fn)))

  (defun my/previous-project-buffer ()
    (interactive)
    (my/projectile-switch-buffer 'previous-buffer))

  (defun my/next-project-buffer ()
    (interactive)
    (my/projectile-switch-buffer 'next-buffer))

  (defun my/bury-project-buffer ()
    (interactive)
    (if (projectile-project-p)
        (let* ((last-root (projectile-project-root))
               (last-buffer (current-buffer)))
          (bury-buffer)
          (while (or (not (projectile-project-buffer-p (current-buffer) last-root))
                     (eq (current-buffer) last-buffer))
            (next-buffer)))
      (bury-buffer))))


(defconst my/global-yas-snippet-dirs '("~/.emacs.d/snippets"))
(defconst my/local-snippet-dir (concat my/local-emacs-dir "snippets"))


(defconst my/local-bookmarks-file (concat my/local-emacs-dir "bookmarks"))


(provide 'my-projectile)
;;; my-projectile.el ends here
