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


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit jiggle evil-paredit markdown-mode zoom-window yasnippet workgroups which-key use-package rainbow-delimiters persp-mode multi-term mocha hydra helm-projectile helm-ag flycheck flatui-theme exec-path-from-shell evil-surround evil-numbers evil-mc evil-matchit evil-leader evil-exchange evil-commentary evil-args elisp-slime-nav editorconfig dired-k company ag)))
 '(safe-local-variable-values
   (quote
    ((helm-ag--extra-options . "--hidden")
     (ag-arguments "--smart-case" "--stats" "--hidden")
     (eval setq yas-snippet-dirs
	   (\`
	    ("~/.emacs.d/snippets"
	     (\,
	      (concat
	       (file-name-directory
		(dir-locals-find-file "."))
	       ".emacs.d/snippets")))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
