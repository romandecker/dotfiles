;;; package --- My custom typescript config
;;; Commentary:
;;; Code:

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :general
  (:states 'normal
   :keymaps 'typescript-mode-map
   "K" 'tide-documentation-at-point)
  (:prefix my/local-leader
   :states 'normal
   :keymaps 'typescript-mode-map
   "*" 'tide-references
   "e" 'tide-project-errors
   "r" 'tide-rename-symbol
   "R" 'tide-rename-file
   "f" 'tide-format
   "c" 'tide-jsdoc-template
   "?" 'tide-verify-setup
   "i" 'tide-organize-imports)

  :config

  ;; make sure "q" works in documentation buffer
  (defun my/advice-tide-doc-buffer (&rest args)
    (with-current-buffer "*tide-documentation*"
      (evil-local-set-key 'normal "q" #'quit-window)))
  (advice-add 'tide-doc-buffer :after #'my/advice-tide-doc-buffer)

  (setq tide-completion-detailed t)

  )


(provide 'my-typescript)
;;; my-typescript.el ends here
