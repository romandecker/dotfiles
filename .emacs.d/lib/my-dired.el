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

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
  "h"   'my/dired-up-directory
  "DEL" 'my/dired-up-directory
  "RET" 'dired-find-alternate-file
  "l"   'dired-find-alternate-file
  "c"   'dired-do-rename
  "m"   'dired-mark
  "u"   'dired-unmark
  "U"   'dired-unmark-all-marks
  "o"   'my/dired-create-file
  "O"   'dired-create-directory
  "n"   'evil-search-next
  "N"   'evil-search-previous
  "y"   'dired-do-copy
  "q"   'kill-this-buffer
  "!"   'dired-do-shell-command)

;; take care not to override global leader
(general-emacs-define-key dired-mode-map my/leader nil)

(eval-after-load 'dired-aux
 '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))
(use-package dired-details+
  :ensure t
  :config)

(use-package dired-subtree
  :ensure t
  :general
  (:keymaps 'dired-mode-map
   "TAB" 'dired-subtree-toggle))

(defun my/dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternative-file (leave no orphan buffer)"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(provide 'my-dired)
;;; my-dired.el ends here
