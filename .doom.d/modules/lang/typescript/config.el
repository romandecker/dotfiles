;;; lang/typescript/config.el -*- lexical-binding: t; -*-


;; (use-package! lsp-mode
;;   :after typescript-mode
;;   :hook ((typescript-mode . lsp-mode)
;;          (typescript-mode . lsp-ui-mode)
;;          (typescript-mode . lsp)))

(use-package! tide
  :demand t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (rjsx-mode . tide-setup))
  :general
  ;; (:states 'normal
  ;;  :keymaps 'typescript-mode-map
  ;;  "K" 'tide-documentation-at-point
  ;;  "g d" 'tide-jump-to-definition
  ;;  )
  ;; (:states 'insert
  ;;  :keymaps 'typescript-mode-map
  ;; ;; this removes the possibility to auto-convert to template
  ;; ;; literals, but makes evil-mc work properly when inserting quotes
  ;;  "'" 'self-insert-command)

  ;; (:prefix my/local-leader
  ;;  :states 'normal
  ;;  :keymaps 'typescript-mode-map
  ;;  "*" 'tide-references
  ;;  "e" 'tide-project-errors
  ;;  "r" 'tide-rename-symbol
  ;;  "R" 'tide-refactor
  ;;  "f" 'tide-format
  ;;  "c" 'tide-jsdoc-template
  ;;  "?" 'tide-verify-setup
  ;;  "i" 'tide-organize-imports
  ;;  "l" 'my/add-eslint-disable-next-line
  ;;  "f" 'tide-fix)
  ;; (:keymaps 'tide-references-mode-map
  ;;  "q" 'quit-window)

  :config
  (map! :localleader :mode tide-mode "f" #'tide-fix)
  (add-to-list '+company-backend-alist '(typescript-mode company-tide))

  (flycheck-add-mode 'typescript-tide 'rjsx-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  (flycheck-add-mode 'jsx-tide 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-next-checker 'javascript-eslint 'typescript-tide)

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
                                :allowTextChangesInNewfiles t
                                :importModuleSpecifierPreference "relative")))



(defconst typescript/eslint-disable-next-line-regexp
  "\/\/\s*eslint-disable-next-line\s+\\(.*\\)"
  "Regexp matching an eslint flag disabling rules on the next line.")

(defun typescript/add-eslint-disable-next-line ()
  "Add an eslint flag to disable rules generating errors at point.

This function adds or modifies a flag of this form to the
previous line:

  // eslint-disable-next-line [rule1] [rule2] [...]

The line will be indented according to the current indentation
settings.  This function generates rule1, rule2 to cover all the
errors present at point.

If the previous line does not already contain a disable-next-line
flag, a new line is added to hold the new flag.  If the previous
line already contains a disable-next-line flag, the rule is added
to the flag.  Note that this function does not preserve the
formatting of the already existing flag.  The resulting flag will
always be formatted as described above."
  (interactive)
  (let ((error-ids (delq nil (tide-get-flycheck-errors-ids-at-point)))
        (start (point)))
    (when error-ids
      (save-excursion
        (if (and (eq 0 (forward-line -1))
                 (looking-at tide-tslint-disable-next-line-regexp))
            ;; We'll update the old flag.
            (let ((old-list (split-string (match-string 1))))
              (delete-region (point) (point-at-eol))
              (setq error-ids (append old-list error-ids)))
          ;; We'll create a new flag.
          (goto-char start)
          (beginning-of-line)
          (open-line 1))
        (insert "// eslint-disable-next-line "
                (string-join error-ids " "))
        (typescript-indent-line)))))
