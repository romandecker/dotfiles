;; window movement
(global-set-key (kbd "C-j") (kbd "SPC w j"))
(global-set-key (kbd "C-k") (kbd "SPC w k"))
(global-set-key (kbd "C-h") (kbd "SPC w h"))
(global-set-key (kbd "C-l") (kbd "SPC w l"))

;; multiple-cursors
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
(define-key evil-visual-state-map "C-n" 'evil-mc-make-and-goto-next-match)
(define-key evil-visual-state-map "C-p" 'evil-mc-make-and-goto-prev-match)
(define-key evil-visual-state-map "C-x" 'evil-mc-skip-and-goto-prev-match)

;; stop multi-cursor mode (TODO change this as soon as I know how to specify a better keymap)
(define-key evil-normal-state-map "S" 'evil-mc-undo-all-cursors)

(with-eval-after-load 'evil
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
  )

(with-eval-after-load 'evil-org
  (define-key evil-org-mode-map (kbd "<normal-state> C-j") 'evil-window-down)
  (define-key evil-org-mode-map (kbd "<normal-state> C-k") 'evil-window-up)
  )

(spacemacs/set-leader-keys "wq" 'delete-window)



(add-hook 'term-mode-hook
          (lambda ()
            (evil-define-key 'insert term-raw-map (kbd "\C-j") 'evil-window-down)
            (evil-define-key 'insert term-raw-map (kbd "\C-k") 'evil-window-up)
            (evil-define-key 'normal term-raw-map (kbd "\C-j") 'evil-window-down)
            (evil-define-key 'normal term-raw-map (kbd "\C-k") 'evil-window-up)
            (evil-define-key 'insert term-raw-map (kbd "\C-a") 'term-send-ctrl-a)
            (evil-define-key 'insert term-raw-map (kbd "\C-e") 'term-send-ctrl-e)
            (evil-define-key 'insert term-raw-map (kbd "\C-r") 'term-send-ctrl-r)
            (evil-define-key 'insert term-raw-map (kbd "\C-p") 'term-send-ctrl-p)
            (evil-define-key 'insert term-raw-map (kbd "\C-n") 'term-send-ctrl-n)
            (evil-define-key 'insert term-raw-map (kbd "\C-c") 'term-send-ctrl-c)
            (evil-define-key 'insert term-raw-map (kbd "\C-d") 'term-send-ctrl-d)
            (evil-define-key 'insert term-raw-map (kbd "\C-z") 'term-send-ctrl-z)
            (evil-define-key 'insert term-raw-map (kbd "\C-w") 'term-send-backward-kill-word)))
