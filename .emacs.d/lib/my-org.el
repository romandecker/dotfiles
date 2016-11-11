(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :config
  (setq
   org-default-notes-file "~/Dropbox/org/notes.org"
   org-log-done t)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-beautify-theme
    :ensure t
    :config)

  (evil-define-key 'normal org-mode-map
    "o" '(lambda () (interactive) (my/org-eol-call 'my/org-insert-item-dwim)))

  (evil-define-key 'normal org-mode-map
    (kbd "C-j") 'my/window-down
    (kbd "C-k") 'my/window-up
    (kbd "C-h") 'my/window-left
    (kbd "C-l") 'my/window-right
    (kbd "<")   'org-metaleft
    (kbd ">")   'org-metaright
    (kbd "S-j") 'evil-join
    (kbd "RET") 'org-open-at-point
    (kbd "TAB") 'org-cycle
    [tab]       'org-cycle)

  (evil-define-key 'insert org-mode-map
    (kbd "TAB") 'org-cycle
    [tab]       'org-cycle)

  (define-key evil-normal-state-map (kbd "RET") 'org-open-at-point-global)

  (evil-leader/set-key-for-mode 'org-mode
    "SPC l" 'org-toggle-link-display))

(defun my/org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(defun my/org-insert-item-dwim ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(provide 'my-org)
