;;; package --- My custom origami config (for folding stuff)
;;; Commentary:
;;; Code:
(use-package origami
  :ensure t
  :general
  (:keymaps 'origami-mode-map
   :states 'normal
   "z a" 'origami-toggle-node)
  :config
  (global-origami-mode))

(provide 'my-origami)
;;; my-origami.el ends here
