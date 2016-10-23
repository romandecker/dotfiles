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
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(use-package mocha
  :ensure t
  :config)

(provide 'my-javascript)
