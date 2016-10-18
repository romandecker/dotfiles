(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lib")

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  :ensure t
  :config)

(require 'my-config)
(require 'my-utils)
(require 'my-evil)
(require 'my-dired)
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
(require 'my-markdown)
(require 'my-rainbow-delimiters)
(require 'my-magit)
(require 'my-paredit)

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
