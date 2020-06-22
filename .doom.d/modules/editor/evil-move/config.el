;;; editor/evil-move/config.el -*- lexical-binding: t; -*-
;;; This module makes all deletions via "d" go to black-hole
;;; Instead, it maps the "m" key ("move") to cut

(after! evil
  (evil-define-operator evil-destroy (beg end type)
    "Delete text from BEG to END with TYPE. Just like evil-delete but always
deletes to black-hole"
    (evil-delete beg end type ?_ nil))

  (map! :n "d" #'evil-destroy)
  (map! :n "m" #'evil-delete))

(after! evil-surround
  (add-to-list 'evil-surround-operator-alist '(evil-destroy . delete)))
