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

(tool-bar-mode -1)    ; disable the tool-bar
(menu-bar-mode -1)    ; disable the menu-bar
(desktop-save-mode 1) ; restore last active session
(global-linum-mode 1) ; show line-numbers everywhere

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'my-window-funcs)
(require 'my-term)
(require 'my-funcs)

(use-package evil
  :ensure t
  :config
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
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
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)))
  
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
