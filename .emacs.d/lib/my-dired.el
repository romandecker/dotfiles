(require 'dired-x)

(setq dired-listing-switches "-alh"
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)

(eval-after-load 'evil
  '(progn
    (evil-define-key 'normal dired-mode-map
      (kbd "h")   'my/dired-up-directory
      (kbd "RET") 'dired-find-alternate-file
      (kbd "l")   'dired-find-alternate-file
      (kbd "c")   'dired-do-rename
      (kbd "m")   'dired-mark
      (kbd "u")   'dired-unmark
      (kbd "U")   'dired-unmark-all-marks
      (kbd "o")   'my/dired-create-file
      (kbd "O")   'dired-create-directory
      (kbd "n")   'evil-search-next
      (kbd "N")   'evil-search-previous
      (kbd "y")   'dired-do-copy
      (kbd "q")   'kill-this-buffer)))

(provide 'my-dired)
