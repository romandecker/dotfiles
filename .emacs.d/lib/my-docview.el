;;; package --- My custom docview config
;;; Commentary:
;;; Code:

(defun my/evil-doc-view-hook ()
  (turn-off-evil-mode)
  (delim-pad-mode -1)
  (define-key doc-view-mode-map (kbd "j")   'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k")   'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-d") 'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "h")   'image-backward-hscroll)
  (define-key doc-view-mode-map (kbd "l")   'image-forward-hscroll))

(add-hook 'doc-view-mode-hook #'my/evil-doc-view-hook)


(provide 'my-docview)
;;; my-docview.el ends here
