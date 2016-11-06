;;; package --- My custom bookmarks config
;;; Commentary:
;;; Code:

(setq bookmark-default-file "~/.emacs.d/bookmarks")

(use-package bookmark+
  :ensure t
  :after evil-leader
  :config
  (setq
   bmkp-last-as-first-bookmark-file nil)
  (evil-leader/set-key
    "m" 'helm-bookmarks)
  (which-key-add-key-based-replacements
    "SPC m" "Bookmarks"))

(provide 'my-bookmarks)
;;; my-bookmarks.el ends here
