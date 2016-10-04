(use-package js2-mode
  :ensure t
  :config
  (js2-mode-hide-warnings-and-errors)          ; do not show errors (use flycheck for that)
  (setq
   js2-skip-preprocessor-directives nil)       ; allow shebangs in js-files (for node)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(provide 'my-javascript)
