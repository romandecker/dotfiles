;;; tools/+ivy/config.el -*- lexical-binding: t; -*-

(map! :after ivy
      :map ivy-mode-map
      "C-d" #'ivy-scroll-up-command
      "C-u" #'ivy-scroll-down-command)

(defun ivy-ffow-done ()
  "Exit the minibuffer, opening candidate in other window."
  (interactive)
  (ivy-set-action #'find-file-other-window)
  (ivy-done))

(map! :after counsel
      :map counsel-find-file-map
      "C-RET" #'ivy-ffow-done)
