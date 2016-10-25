(setq    ;; default values for indentation (possibly overwritten by editorconfig)
 js2-basic-offset 2
 js-indent-level 2
 js-expr-indent-offset -2)

(use-package js2-mode
  :ensure t
  :config
  (js2-mode-hide-warnings-and-errors)          ; do not show errors (use flycheck for that)
  (setq
   js2-skip-preprocessor-directives nil)       ; allow shebangs in js-files (for node)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (use-package js2-refactor
    :ensure t
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (evil-define-key 'normal js2-mode-map
      (kbd "] ]") 'js2r-forward-slurp
      (kbd "] [") 'js2r-forward-barf)
    (evil-leader/set-key-for-mode 'js2-mode
      "SPC e f" 'js2r-extract-function
      "SPC e m" 'js2r-extract-method
      "SPC e v" 'js2r-extract-var
      "SPC l"   'js2r-log-this
      "SPC s o" 'js2r-expand-object
      "SPC s a" 'js2r-expand-array
      "SPC s f" 'js2r-expand-function
      "SPC s s" 'js2r-split-string
      "SPC j o" 'js2r-contract-object
      "SPC j a" 'js2r-contract-array
      "SPC j f" 'js2r-contract-function
      "SPC r"   'js2r-rename-var
      "SPC ."   'js2r-var-to-this
      "SPC 3"   'js2r-ternary-to-if)
    (which-key-add-key-based-replacements
      "SPC SPC e" "Extract..."
      "SPC SPC j" "Join..."
      "SPC SPC s" "Split...")))

(use-package mocha
  :ensure t
  :config)


(provide 'my-javascript)
