;;; package --- My custom config for general programming stuff
;;; Commentary:
;;; Code:
(use-package highlight-symbol
  :ensure t
  :after evil
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (define-key evil-normal-state-map (kbd "] o") 'highlight-symbol-next)
  (define-key evil-normal-state-map (kbd "[ o") 'highlight-symbol-prev))

(use-package indent-guide
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'indent-guide-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'whitespace-cleanup-mode))

(use-package aggressive-fill-paragraph
  :ensure t
  :config
  (add-to-list 'afp-fill-comments-only-mode-list 'js2-mode)
  (add-to-list 'afp-fill-comments-only-mode-list 'yaml-mode)
  (add-to-list 'afp-fill-comments-only-mode-list 'snippet-mode)
  (afp-setup-recommended-hooks))

(evil-leader/set-key
  "c c" 'recompile
  "c p" 'projectile-compile-project
  "c x" 'kill-compilation)

(which-key-add-key-based-replacements
  "SPC c" "Compile")

(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail))

(defun my/hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'prog-mode-hook (lambda () (linum-mode t)))
(add-hook 'minibuffer-setup-hook #'my/hide-trailing-whitespace)
(add-hook 'helm-mode-hook #'my/hide-trailing-whitespace)
(add-hook 'term-mode-hook #'my/hide-trailing-whitespace)

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/toggle-compilation-scroll ()
  (interactive)
  (setq compilation-scroll-output (not compilation-scroll-output))
  (message "compilation-scroll-output: %s" compilation-scroll-output))

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)

(defun my/init-compilation-mode ()
  (local-unset-key "g")
  (local-unset-key "h")
  (evil-define-key 'motion compilation-mode-map
    (kbd "r" )  'recompile
    (kbd "h" )  'evil-backward-char
    (kbd "C-u") 'evil-scroll-page-up))

(add-hook 'compilation-mode-hook #'my/init-compilation-mode)

(define-key compilation-mode-map (kbd "s") 'my/toggle-compilation-scroll)

(provide 'my-prog)
;;; my-prog.el ends here
