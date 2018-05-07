;;; package --- My custom dired config
;;; Commentary:
;;; Code:
(require 'dired-x)
(require 'dired-details-plus)

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
  "C-j" 'my/window-down
  "RET" 'my/dired-enter
  "b"   'evil-backward-little-word-begin
  "c"   'my/dired-start-change
  "e"   'evil-forward-little-word-end
  "g g" 'evil-goto-first-line
  "G"   'evil-goto-line
  "h"   'my/dired-up-directory
  "i"   'dired-toggle-read-only
  "l"   'my/dired-enter
  "m"   'my/dired-toggle-mark
  "M"   'dired-do-rename
  "n"   'evil-search-next
  "N"   'evil-search-previous
  "o"   'my/dired-create-file
  "O"   'dired-create-directory
  "q"   'my/quit-dired-window
  "r"   'revert-buffer
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

(defun my/quit-dired-window ()
  (interactive)
  (let ((ws (get-buffer-window-list)))
    (if (eq 1 (length ws))
        (kill-this-buffer)
      (quit-window))))

(defun my/dired-enter ()
  (interactive)
  (let ((count (length (get-buffer-window-list))))
    (if (eq 1 count)
        (call-interactively 'dired-find-alternate-file)
      (find-file (dired-get-file-for-visit)))))


;; take care not to override global leader
(general-emacs-define-key dired-mode-map my/leader nil)

(eval-after-load 'dired-aux
 '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))

(use-package dired-subtree
  :ensure t
  :general
  (:keymaps 'dired-mode-map
   "TAB" 'dired-subtree-toggle))

(defun my/dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternative-file (leave no orphan buffer)"
  (interactive)
  (let* ((old (current-buffer))
         (count (length (get-buffer-window-list old))))
    (dired-up-directory)
    (when (eq 1 count)
        (kill-buffer old)))) 

(provide 'my-dired)
;;; my-dired.el ends here
