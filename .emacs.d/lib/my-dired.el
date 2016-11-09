;;; package --- My custom dired config
;;; Commentary:
;;; Code:
(require 'dired-x)

(setq dired-listing-switches "-alh"
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(setq-default dired-omit-mode t
        dired-omit-verbose nil
        dired-omit-files "^\\.\\.?$")

(put 'dired-find-alternate-file 'disabled nil)

(eval-after-load 'evil
  '(progn
    (evil-define-key 'normal dired-mode-map
      (kbd "h")   'my/dired-up-directory
      (kbd "DEL") 'my/dired-up-directory
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

(eval-after-load 'dired-aux
 '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))
(use-package dired-details+
  :ensure t
  :config)

(use-package dired-subtree
  :ensure t
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle))

(defun my/dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternative-file (leave no orphan buffer)"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(provide 'my-dired)
;;; my-dired.el ends here
