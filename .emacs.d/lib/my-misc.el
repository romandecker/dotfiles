;;; package --- Additional packages that don't fit anywhere else
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package restart-emacs
  :ensure t
  :general
  (general-define-key
   :prefix my/leader
   :keymaps '(normal visual)
    "Q R"   'restart-emacs
    "Q Q" 'kill-emacs)
  :config
  (which-key-add-key-based-replacements
    "SPC Q"   "Quitting"))

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-l") 'buf-move-right))

(defun my/isearch-backward-kill-word ()
  "Just like `kill-backward-word', but for the isearch minibuffer."
  (interactive)
  (if (null (cdr isearch-cmds))
      (ding)
    (let* ((current (string-reverse (isearch--state-string (car isearch-cmds))))
           (match (string-match "[[:word:]][^[:word:]]" current)))
      (while (and (not (null (cdr isearch-cmds)))
                  (or (null match)
                      (>= match 0)))
        (setf isearch-cmds (cdr isearch-cmds)
              match (if (null match) match (1- match))))
      (isearch--set-state (car isearch-cmds))))
  (isearch-update))

(use-package vlf
  :ensure t
  :config)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)
(define-key minibuffer-local-map (kbd "C-v") 'evil-paste-after)
(define-key isearch-mode-map (kbd "C-w") 'my/isearch-backward-kill-word)

(provide 'my-misc)
