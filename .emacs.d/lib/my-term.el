(use-package multi-term
  :ensure t
  :config
  (add-hook
   'term-mode-hook
   (lambda ()
     (yas-minor-mode -1)
     (evil-define-key 'normal term-raw-map
       (kbd "C-j")   'my/window-down
       (kbd "C-k")   'my/window-up
       (kbd "C-h")   'my/window-left
       (kbd "C-l")   'my/window-right)
     (evil-define-key 'insert term-raw-map
       (kbd "C-j")   'my/window-down
       (kbd "C-k")   'my/window-up
       (kbd "C-h")   'my/window-left
       (kbd "C-l")   'my/window-right
       (kbd "C-n")   'term-send-down
       (kbd "C-p")   'term-send-up
       (kbd "C-d")   'term-send-eof
       (kbd "C-c")   'my/term-send-ctrl-c
       (kbd "C-z")   'my/term-send-ctrl-z
       (kbd "C-w")   'term-send-backward-kill-word
       (kbd "C-r")   'term-send-reverse-search-history
       (kbd "TAB")   'my/term-send-tab
       [tab]         'my/term-send-tab
       (kbd "M-.")   'my/term-send-m-dot
       (kbd "C-]")   'my/term-send-esc
       (kbd "C-S-v") 'term-paste
       (kbd "SPC")   'my/term-send-space)    ; must use this, or else smart-space overrides space here
     ))
  )

(defun my/toggle-project-term ()
  (interactive)
  (if (my/get-project-term-window)
      (my/close-project-term)
    (my/open-project-term)))

(defun my/open-project-term ()
  (interactive)
  (let ((termwin (or (my/get-project-term-window)
		     (my/create-project-term-window)))
	(termbuf (my/get-project-term-buffer)))
    (select-window termwin)
    (if termbuf
	(set-window-buffer termwin termbuf t)
      (my/create-project-term-buffer))
    (set-window-dedicated-p termwin t)))

(defun my/create-project-term-window ()
  (split-window (frame-root-window)
		(- (multi-term-current-window-take-height) 20)))

(defun my/close-project-term ()
  (interactive)
  (let ((termwin (my/get-project-term-window)))
    (when termwin (delete-window termwin))))

(defun my/create-project-term-buffer ()
  (let ((name (my/get-project-term-name)))
    (multi-term)
    (rename-buffer name)
    (insert (format "cd %s" (projectile-project-root)))
    (term-send-input)))

(defun my/get-project-term-buffer ()
  (let ((name (my/get-project-term-name)))
    (get-buffer name)))

(defun my/get-project-term-window ()
  (let ((buf (my/get-project-term-buffer)))
    (when buf (get-buffer-window buf (selected-frame)))))

(defun my/project-term-exist-p ()
  (and (multi-term-buffer-exist-p (my/get-project-term-buffer))
       (multi-term-window-exist-p (my/get-project-term-window))))

(defun my/get-project-term-name ()
  (when (projectile-project-p)
    (format "*MULTI-TERM|%s*" (projectile-project-name))))

(defun my/term-send-ctrl-c ()
  "Send Ctrl+C."
  (interactive)
  (term-send-raw-string "\C-c"))

(defun my/term-send-ctrl-z ()
  "Suspend."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun my/term-send-space ()
  "Send space."
  (interactive)
  (term-send-raw-string " "))

(defun my/term-toggle-term ()
  "Toggle the dedicated terminal."
  (interactive)
  (multi-term-dedicated-toggle)
  (multi-term-dedicated-select))

(defun my/term-send-tab ()
  "Send tab."
  (interactive)
  (term-send-raw-string "\t"))

(defun my/term-send-esc ()
  "Send Meta+."
  (interactive)
  (term-send-raw-string "\e"))

(defun my/term-send-m-dot ()
  "Send Meta+."
  (interactive)
  (term-send-raw-string "\e."))

(provide 'my-term)
