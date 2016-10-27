;;; package --- My custom origami config (for folding stuff)
;;; Commentary:
;;; Code:
(use-package origami
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "z a") 'origami-toggle-node)
  (global-origami-mode))

(provide 'my-origami)
;;; my-origami.el ends here
