(require 'my-term-funcs)

(use-package multi-term
  :ensure t
  :config
  (add-hook
   'term-mode-hook
   (lambda ()
     (yas-minor-mode -1)
     (evil-define-key 'normal term-raw-map
       (kbd "C-j")   'my-window-funcs/window-down
       (kbd "C-k")   'my-window-funcs/window-up
       (kbd "C-h")   'my-window-funcs/window-left
       (kbd "C-l")   'my-window-funcs/window-right)
     (evil-define-key 'insert term-raw-map
       (kbd "C-j")   'my-window-funcs/window-down
       (kbd "C-k")   'my-window-funcs/window-up
       (kbd "C-h")   'my-window-funcs/window-left
       (kbd "C-l")   'my-window-funcs/window-right
       (kbd "C-n")   'term-send-down
       (kbd "C-p")   'term-send-up
       (kbd "C-d")   'term-send-eof
       (kbd "C-c")   'my-term-funcs/send-ctrl-c
       (kbd "C-z")   'my-term-funcs/send-ctrl-z
       (kbd "C-w")   'term-send-backward-kill-word
       (kbd "C-r")   'term-send-reverse-search-history
       (kbd "TAB")   'my-term-funcs/send-tab
       [tab]         'my-term-funcs/send-tab
       (kbd "M-.")   'my-term-funcs/send-m-dot
       (kbd "C-]")   'my-term-funcs/send-esc
       (kbd "C-S-v") 'term-paste
       (kbd "SPC")   'my-term-funcs/send-space)    ; must use this, or else smart-space overrides space here
     ))
  ;(add-hook 'kill-buffer-hook #'my-funcs/kill-buffer-hook)
  )

(defun my-funcs/kill-buffer-hook ()
  (let ((buf (current-buffer)))
    (when (string= buf (my-funcs/get-project-term-name))
      (message "Buffer killed: %s!" (current-buffer)))))

(defun my-funcs/toggle-project-term ()
  (interactive)
  (if (my-funcs/get-project-term-window)
      (my-funcs/close-project-term)
    (my-funcs/open-project-term)))

(defun my-funcs/open-project-term ()
  (interactive)
  (let ((termwin (or (my-funcs/get-project-term-window)
		     (my-funcs/create-project-term-window)))
	(termbuf (my-funcs/get-project-term-buffer)))
    (select-window termwin)
    (if termbuf
	(set-window-buffer termwin termbuf t)
      (my-funcs/create-project-term-buffer))
    (set-window-dedicated-p termwin t)))

(defun my-funcs/create-project-term-window ()
  (split-window (frame-root-window)
		(- (multi-term-current-window-take-height) 20)))

(defun my-funcs/close-project-term ()
  (interactive)
  (let ((termwin (my-funcs/get-project-term-window)))
    (when termwin (delete-window termwin))))

(defun my-funcs/create-project-term-buffer ()
  (let ((name (my-funcs/get-project-term-name)))
    (multi-term)
    (rename-buffer name)
    (insert (format "cd %s" (projectile-project-root)))
    (term-send-input)))

(defun my-funcs/get-project-term-buffer ()
  (let ((name (my-funcs/get-project-term-name)))
    (get-buffer name)))

(defun my-funcs/get-project-term-window ()
  (let ((buf (my-funcs/get-project-term-buffer)))
    (when buf (get-buffer-window buf (selected-frame)))))

(defun my-funcs/project-term-exist-p ()
  (and (multi-term-buffer-exist-p (my-funcs/get-project-term-buffer))
       (multi-term-window-exist-p (my-funcs/get-project-term-window))))

(defun my-funcs/get-project-term-name ()
  (when (projectile-project-p)
    (format "*MULTI-TERM|%s*" (projectile-project-name))))

(provide 'my-term)
