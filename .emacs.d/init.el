(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lib")

(setq
 package-enable-at-startup nil
 inhibit-startup-screen t
 x-select-enable-clipboard t)

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

(require 'my-window-funcs)
(require 'my-term)
(require 'my-funcs)

(defconst my-funcs/pairs '(("(" . ")") ("[" . "]") ("{" . "}")))

(defun my-funcs/smart-space ()
  (interactive)
  (let ((before (string (char-before)))
	(after (string (char-after))))
    (let ((match (cdr (assoc before my-funcs/pairs))))
	(when (equal after match)
	    (insert " ")
	    (backward-char))))
  (insert " "))

(defun my-funcs/smart-delete ()
  (interactive)
  (let ((before (string (char-before)))
	(after (string (char-after)))
	(before2 (string (char-before 2)))
	(after2 (string (char-after 2))))
    (if (and (equal before " ") (equal after " "))
	(let ((match (cdr (assoc before2 my-funcs/pairs))))
	  (when (equal after2 match)
	    (message "Now!")))))
  (electric-pair-delete-pair))


(use-package evil
  :ensure t
  :config
  ; (define-key evil-insert-state-map (kbd "SPC") 'my-funcs/smart-space)
  ; (define-key evil-insert-state-map (kbd "DEL") 'my-funcs/smart-delete)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))
  (use-package evil-args
    :ensure t
    :config
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))
  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      ":" 'helm-M-x
      "~" 'my-term-funcs/toggle-term
      "TAB" 'my-window-funcs/switch-to-last-buffer
      "b b" 'helm-buffers-list
      "b n" 'next-buffer
      "b p" 'previous-buffer
      "f f" 'helm-find-files
      "f s" 'save-buffer
      "p p" 'helm-projectile-switch-project
      "p f" 'helm-projectile-find-file-dwim
      "p d" 'helm-projectile-find-dir
      "p a" 'helm-projectile-ag
      "t u" 'undo-tree-visualize
      "w q" 'evil-window-delete
      "z"   'zoom-window-zoom
      ". e" 'my-funcs/open-dotfile
      ". r" 'my-funcs/reload-dotfile
      "? k" 'describe-key
      "? v" 'describe-variable
      "? f" 'describe-function
      "? m" 'describe-mode)
    (global-set-key (kbd "C-j") 'my-window-funcs/window-down)
    (global-set-key (kbd "C-k") 'my-window-funcs/window-up)
    (global-set-key (kbd "C-h") 'my-window-funcs/window-left)
    (global-set-key (kbd "C-l") 'my-window-funcs/window-right)
    (global-evil-leader-mode))
  (evil-mode 1)) ; evil-leader must be enabled before evil

(use-package projectile
  :ensure t
  :config)

(use-package helm
  :ensure t
  :config
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action) ; complete with tab
  (helm-mode 1)
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on))
  (use-package helm-ag
    :ensure t
    :config))
  
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
  (setq zoom-window-mode-line-color "darkgreen"))

(use-package js2-mode
  :ensure t
  :config)

(use-package rainbow-delimiters
  :ensure t
  :config)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package editorconfig
  :ensure t
  :config
  (add-hook
   'editorconfig-custom-hooks
   (lambda (hash)
     (setq js-indent-level (string-to-number (gethash 'indent_size hash)))
     (setq js-expr-indent-offset (- js-indent-level))))
  (add-to-list 'editorconfig-indentation-alist
	       '(js2-mode js2-basic-offset))
  :init
  (add-hook 'js2-mode-hook (progn
			     (editorconfig-mode 1)
			     (setq js-indent-level js2-basic-offset)
			     (setq js-expr-indent-offset (- js-indent-level)))))

(add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
