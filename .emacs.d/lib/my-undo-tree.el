;;; package --- My custom undo tree config
;;; Commentary:
;;; Code:

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-visualizer-timestamps t)
  (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-mode-map (kbd "h") 'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)
  (add-hook 'undo-tree-visualizer-mode-hook #'turn-off-evil-mode)
  (global-undo-tree-mode))

(provide 'my-undo-tree)
;;; my-undo-tree.el ends here
