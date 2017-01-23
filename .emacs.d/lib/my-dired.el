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
  "!"   'dired-do-shell-command
  "DEL" 'my/dired-up-directory
  "RET" 'dired-find-alternate-file
  "b"   'evil-backward-little-word-begin
  "c"   'my/dired-start-change
  "e"   'evil-forward-little-word-end
  "g g" 'evil-goto-first-line
  "G"   'evil-goto-line
  "h"   'my/dired-up-directory
  "l"   'dired-find-alternate-file
  "m"   'my/dired-toggle-mark
  "M"   'dired-do-rename
  "n"   'evil-search-next
  "N"   'evil-search-previous
  "o"   'my/dired-create-file
  "O"   'dired-create-directory
  "q"   'kill-this-buffer
  "u"   'dired-unmark
  "U"   'dired-unmark-all-marks
  "v"   'evil-visual-char
  "V"   'evil-visual-line
  "w"   'evil-forward-little-word-begin
  "y"   'dired-do-copy)

(defun my/dired-marked-p ()
  (= dired-marker-char (string-to-char (thing-at-point 'line))))

(defun my/dired-toggle-mark()
  (interactive)
  (if (my/dired-marked-p)
      (call-interactively 'dired-unmark)
    (call-interactively 'dired-mark)))

(defun my/dired-start-change ()
  (interactive)
  (dired-toggle-read-only)
  (call-interactively 'evil-change))



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
