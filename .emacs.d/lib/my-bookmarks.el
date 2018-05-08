;;; package --- My custom bookmarks config
;;; Commentary:
;;; Code:

(setq bookmark-default-file "~/.emacs.d/bookmarks")

(require 'bookmark+)

(my/define-leader-map
 "m" 'helm-bookmarks)

(setq
  bmkp-last-as-first-bookmark-file nil)

(which-key-add-key-based-replacements
  (concat my/leader " m") "Bookmarks")

(provide 'my-bookmarks)
;;; my-bookmarks.el ends here
