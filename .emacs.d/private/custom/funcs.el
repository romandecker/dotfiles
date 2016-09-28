;;; funcs.el --- Utility functions for my custom spacemacs config
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Roman Decker <roman@roman-ubuntu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(defun term-send-ctrl-a ()
  "Go to beginning of line."
  (interactive)
  (term-send-raw-string "\C-a"))

(defun term-send-ctrl-e ()
  "Go to end of line."
  (interactive)
  (term-send-raw-string "\C-e"))
(defun term-send-ctrl-r ()
  "Start reverse history search."
  (interactive)
  (term-send-raw-string "\C-r"))

(defun term-send-ctrl-p ()
  "Go back in history."
  (interactive)
  (term-send-raw-string "\C-p"))

(defun term-send-ctrl-n ()
  "Go forward in history."
  (interactive)
  (term-send-raw-string "\C-n"))

(defun term-send-ctrl-c ()
  "Send Ctrl+C."
  (interactive)
  (term-send-raw-string "\C-c"))

(defun term-send-ctrl-d ()
  "Send EOF."
  (interactive)
  (term-send-raw-string "\C-d"))

(defun term-send-ctrl-z ()
  "Suspend."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun custom-zoom-sensitive-window-move (action)
  "Check whether the current frame is zoomed, and if yes, unzoom before performing the given ACTION."
  (interactive)
  (when (frame-parameter nil 'zoom-window-enabled) (zoom-window-zoom))
  (funcall action 1))

(defun custom-zoom-sensitive-window-down ()
  "Zoom-sensitive window-down move."
  (interactive)
  (custom-zoom-sensitive-window-move 'evil-window-down))
(defun custom-zoom-sensitive-window-up ()
  "Zoom-sensitive window-up move."
  (interactive)
  (custom-zoom-sensitive-window-move 'evil-window-up))
(defun custom-zoom-sensitive-window-left ()
  "Zoom-sensitive window-left move."
  (interactive)
  (custom-zoom-sensitive-window-move 'evil-window-left))
(defun custom-zoom-sensitive-window-right ()
  "Zoom-sensitive window-right move."
  (interactive)
  (custom-zoom-sensitive-window-move 'evil-window-right))

(provide 'funcs)
;;; funcs.el ends here
