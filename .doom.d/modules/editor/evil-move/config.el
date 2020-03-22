;;; editor/evil-move/config.el -*- lexical-binding: t; -*-
;;; This module makes all deletions via "d" go to black-hole
;;; Instead, it maps the "m" key ("move") to cut

(defvar +my/is-move nil
  "Temporary variable to tell if the current evil-delete command is actually a move command")

(defun +my/evil-delete-advice
    (orig-fn beg end &optional type register &rest args)
  "Advice to add around evil-delete that will check `+my/is-move' and only if it
is set to t will it write to the passed register. If not, the deletion will go
to the blackhole register."
  (apply orig-fn beg end type (if +my/is-move register ?_) args))

(after! evil
  (advice-add 'evil-delete :around '+my/evil-delete-advice))

(defun +my/evil-move ()
  (interactive)
  (let ((+my/is-move t))
    (call-interactively #'evil-delete)))

(map! :n "m" #'+my/evil-move)
