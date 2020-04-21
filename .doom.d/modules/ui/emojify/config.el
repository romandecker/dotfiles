;;; ui/emojify/config.el -*- lexical-binding: t; -*-

(use-package! emojify
  :config
  (global-emojify-mode))

(map! :leader "i e" #'emojify-insert-emoji)
