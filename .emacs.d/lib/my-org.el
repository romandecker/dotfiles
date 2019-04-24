(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :general
  (:states 'normal
   :keymaps 'org-mode-map
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
  (:prefix my/local-leader
   :keymaps 'org-mode-map
   :states 'normal
   "t"   'org-todo
   "e"   'org-export-dispatch
   "c" 'hydra-org-global-cycle/body
   )
  :config
  (my/define-leader-map
   "o c"   'org-capture
   "o y"   'org-store-link
   "o p"   'org-insert-link
   "o f"   'my/org-find-file
   "o o"   'my/org-open-notes-directory
   )
  (setq
   org-directory "~/Dropbox/org"
   org-default-notes-file "~/Dropbox/org/notes.org"
   org-agenda-files '("~/Dropbox/org/agenda")
   org-log-done t
   org-image-actual-width nil)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-beautify-theme
    :ensure t
    :config)
  (use-package org-download
    :ensure t
    :general
    (:keymaps 'org-mode-map
     (kbd "<drag-n-drop>") 'org-download))
  (defhydra hydra-org-global-cycle (:pre (org-global-cycle))
    "org-global-cycle"
    ("c" org-global-cycle "Cycle")))


(defvar my-org/helm-source-org-notes
  (helm-build-in-buffer-source "org Notes"
    :data (lambda ()
            (mapcar (lambda (path)
                      (file-relative-name path org-directory))
                      (directory-files-recursively org-directory ".*")))
    :action 'my/org-open-note
    )
  "Helm source definition for files within `org-directory'")


(defun my/org-find-file (&optional initial-input)
  (interactive)
  "Start helm to search for org notes in org-directory.
If INITIAL-INPUT is given, helm will initially be filled with the
given string."
  (helm :sources '(my-org/helm-source-org-notes)
        :input ""))

(defun my/org-open-note (relative-path)
  "Open the note at the given `RELATIVE-PATH' (relative to `org-diretory')."
  (find-file (concat (file-name-as-directory org-directory) relative-path)))


(defun my/org-open-notes-directory ()
  (interactive)
  (find-file org-directory))


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
