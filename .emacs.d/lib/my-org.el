(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :general
  (:keymaps 'org-mode-map
   "o" '(lambda () (interactive) (my/org-eol-call 'my/org-insert-item-dwim))
   "C-j" 'my/window-down
   "C-k" 'my/window-up
   "C-h" 'my/window-left
   "C-l" 'my/window-right
   "<"   'org-metaleft
   ">"   'org-metaright
   "S-j" 'evil-join
   "RET" 'org-open-at-point
   "TAB" 'org-cycle
   [tab] 'org-cycle)
  (:states 'insert
   :keymaps 'org-mode-map
    "TAB" 'org-cycle
    [tab] 'org-cycle)
  (:keymaps 'normal
   "RET" 'org-open-at-point-global
   "g x" 'org-open-at-point-global)
  (:prefix my/leader
   :states 'normal
   :keymaps 'org-mode-map
   "t l" 'org-toggle-link-display)

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
    :config))


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
