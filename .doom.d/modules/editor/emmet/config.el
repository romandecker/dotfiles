;;; editor/++evil/config.el -*- lexical-binding: t; -*-

(use-package! emmet-mode
  :config
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (map! :map emmet-mode-keymap "C-j" #'evil-window-down))
