(defconst dotfile "~/.emacs.d/init.el")

(defun my-funcs/reload-dotfile ()
  "Reload '~/.emacs.d/init.el'."
  (interactive)
  (load-file dotfile))

(defun my-funcs/open-dotfile ()
  "Open '~/.emacs.d/init.el'."
  (interactive)
  (find-file dotfile))

(provide 'my-funcs)
