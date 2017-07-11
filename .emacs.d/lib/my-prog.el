;;; package --- My custom config for general programming stuff
;;; Commentary:
;;; Code:

;; set compilation-error-regexp-alist to not contain anything I never
;; actually edit
(setq compilation-error-regexp-alist
      '(
        ant
        maven
        bash
        borland
        python-tracebacks-and-caml
        cucumber
        gcc-include
        ruby-Test::Unit
        ;; gnu ; gnu clashes with node
        lcc
        perl
        phpweblint
        ))

(use-package highlight-symbol
  :ensure t
  :general
  (:keymaps 'normal
   "] o" 'highlight-symbol-next
   "[ o" 'highlight-symbol-prev)
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

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
  (add-to-list 'afp-fill-comments-only-mode-list 'elisp-mode)
  (afp-setup-recommended-hooks))

(use-package dtrt-indent
  :ensure t
  :demand t
  :config
  (dtrt-indent-mode))

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

(defun my/send-input-to-compilation (input &optional nl)
  "Send INPUT to the compilation process. Interactively also sends a terminating newline."
  (interactive "MInput: \nd")
  (let* ((string (concat input (if nl "\n")))
         (compilation-buffer (get-buffer "*compilation*")))
    (process-send-string (get-buffer-process compilation-buffer) string)))

; (my/define-leader-map
;  "c i"     'my/send-input-to-compilation)

(defun my/init-compilation-mode ()
  (local-unset-key "g")
  (local-unset-key "h")
  (local-unset-key "SPC")
  (setq truncate-lines nil)
  (evil-define-key 'motion compilation-mode-map
    (kbd "r" )  'recompile
    (kbd "h" )  'evil-backward-char
    (kbd "C-u") 'evil-scroll-page-up
    (kbd "i")   'my/send-input-to-compilation))

(add-hook 'compilation-mode-hook #'my/init-compilation-mode)

(define-key compilation-mode-map (kbd "s") 'my/toggle-compilation-scroll)
(general-evil-define-key '(normal insert visual) 'debugger-mode-map "q" 'quit-window)


(provide 'my-prog)
;;; my-prog.el ends here
