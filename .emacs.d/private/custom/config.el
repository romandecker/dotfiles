
;; do not update clipboard just by selecting text
(fset 'evil-visual-update-x-selection 'ignore)

(setq
 x-select-enable-clipboard t
 js2-skip-preprocessor-directives t
 exec-path-from-shell-check-startup-files nil
 )

(setq-default
 indent-tabs-mode nil
 ;; restore session after restart
 dotspacemacs-auto-resume-layouts t
 js2-basic-offset 2
 js2-strict-inconsistent-return-warning nil)

;; show line numbers by default
(global-linum-mode)

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)

(electric-pair-mode 1)

(add-hook 'js2-mode-hook
          (lambda ()
            (js-expr-indent-offset -2)
            (setq js2-basic-offset 2)))

(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist
      `(("." . ,(concat spacemacs-cache-directory "undo"))))

(unless (file-exists-p (concat spacemacs-cache-directory "undo"))
  (make-directory (concat spacemacs-cache-directory "undo")))

;;; config.el ends here
