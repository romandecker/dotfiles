(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lib")

(setq
 package-enable-at-startup nil
 inhibit-startup-screen t
 x-select-enable-clipboard t
 uniquify-buffer-name-style "post-forward")

(set-default
 'truncate-lines t)

(tool-bar-mode -1)     ; disable the tool-bar
(menu-bar-mode -1)     ; disable the menu-bar
(global-linum-mode 1)  ; show line-numbers everywhere
(show-paren-mode)
(electric-pair-mode 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(require 'my-evil)
(require 'my-term)
(require 'my-helm)
(require 'my-javascript)
(require 'my-editorconfig)
(require 'my-flycheck)
(require 'my-yasnippet)
(require 'my-company)
(require 'my-hydra)
(require 'my-backup)
(require 'my-projectile)
(require 'my-dired)
(require 'my-markdown)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

; general-purpose string-manipulation library
(use-package s
  :ensure t
  :config)

  
(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color "blue"))

;; default values for indentation (possibly overwritten by editorconfig)
(setq
 js2-basic-offset 2
 js-indent-level 2
 js-expr-indent-offset -2)

(use-package rainbow-delimiters
  :ensure t
  :config)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t
	undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package flatui-theme
  :ensure t
  :config
  (load-theme 'flatui t))

; for keeping track of recent files, provides helm-recentf with data
(use-package recentf
  :ensure t
  :config)

(use-package elisp-slime-nav
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode) (eldoc-mode))))

(add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(put 'dired-find-alternate-file 'disabled nil)

(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)
