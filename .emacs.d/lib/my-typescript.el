;;; package --- My custom typescript config
;;; Commentary:
;;; Code:

(use-package tide
  :ensure t
  :demand t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (rjsx-mode . tide-setup))
  :general
  (:states 'normal
   :keymaps 'typescript-mode-map
   "K" 'tide-documentation-at-point
   "g d" 'tide-jump-to-definition
   )
  (:prefix my/local-leader
   :states 'normal
   :keymaps 'typescript-mode-map
   "*" 'tide-references
   "e" 'tide-project-errors
   "r" 'tide-rename-symbol
   "R" 'tide-refactor
   "f" 'tide-format
   "c" 'tide-jsdoc-template
   "?" 'tide-verify-setup
   "i" 'tide-organize-imports
   "l" 'tide-add-tslint-disable-next-line
   "f" 'tide-fix)
  (:keymaps 'tide-references-mode-map
   "q" 'quit-window)
  

  :config

  (flycheck-add-mode 'typescript-tide 'rjsx-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  (flycheck-add-mode 'jsx-tide 'rjsx-mode)

  (put 'flycheck-tslint-args 'risky-local-variable nil)
  (put 'flycheck-typescript-tslint-executable 'risky-local-variable nil)

  ;; make sure "q" works in documentation buffer
  (defun my/advice-tide-doc-buffer (&rest args)
    (with-current-buffer "*tide-documentation*"
      (evil-local-set-key 'normal "q" #'quit-window)))

  ;; make sure "q" works in references buffer
  (defun my/advice-tide-references-buffer (&rest args)
    (with-current-buffer "*tide-references*"
      (evil-local-set-key 'normal "q" #'quit-window)))

  (advice-add 'tide-doc-buffer :after #'my/advice-tide-doc-buffer)

  (advice-add 'tide-references :after #'my/advice-tide-references-buffer)

  (setq tide-completion-detailed t
        tide-always-show-documentation t
        tide-jump-to-definition-reuse-window nil
        tide-user-preferences '(
                                :includeCompletionsForModuleExports t
                                :includeCompletionsWithInsertText t
                                :allowTextChangesInNewFiles t
                                :importModuleSpecifierPreference "relative")))


(provide 'my-typescript)
;;; my-typescript.el ends here
