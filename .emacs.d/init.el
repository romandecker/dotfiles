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
 make-backup-files nil)

(set-default
 'truncate-lines t)

(tool-bar-mode -1)     ; disable the tool-bar
(menu-bar-mode -1)     ; disable the menu-bar
(desktop-save-mode 1)  ; restore last active session
(global-linum-mode 1)  ; show line-numbers everywhere
(show-paren-mode)
(electric-pair-mode 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(require 'dired-x)
(require 'my-evil)
(require 'my-term)
(require 'my-helm)
(require 'my-javascript)
(require 'my-editorconfig)

(use-package projectile
  :ensure t
  :config)
  
(use-package which-key
  :ensure t
  :config
  (which-key-add-key-based-replacements
    "SPC TAB" "Last active buffer"
    "SPC :"   "Execute ex-command"
    "SPC ~"   "Toggle terminal"
    "SPC b"   "Buffers"
    "SPC f"   "Files"
    "SPC p"   "Projects"
    "SPC t"   "Toggles"
    "SPC w"   "Windows"
    "SPC ."   "Dotfiles"
    "SPC ?"   "Get help")
  (which-key-mode))

(use-package multi-term
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
	undo-tree-history-directory-alist '("~/.emacs.d/undo"))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flatui-theme zoom-window which-key use-package rainbow-delimiters multi-term js2-mode helm-projectile helm-ag evil-surround evil-numbers evil-mc evil-matchit evil-leader evil-args elisp-slime-nav editorconfig color-theme avk-emacs-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
