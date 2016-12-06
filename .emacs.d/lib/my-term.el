(use-package multi-term
  :ensure t
  :after projectile
  :general
  (:states 'insert
   :keymaps 'term-raw-map
   "C-n"   'term-send-down
   "C-p"   'term-send-up
   "C-d"   'term-send-eof
   "C-c"   'my/term-send-ctrl-c
   "C-z"   'my/term-send-ctrl-z
   "C-w"   'term-send-backward-kill-word
   "C-r"   'term-send-reverse-search-history
   "TAB"   'my/term-send-tab
   [tab]   'my/term-send-tab
   "M-."   'my/term-send-m-dot
   "C-]"   'my/term-send-esc
   "C-S-v" 'term-paste
   "SPC"   'my/term-send-space)    ; must use this, or else smart-space overrides space here
  :config
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

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
  )


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
