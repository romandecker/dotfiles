;; window movement
(global-set-key (kbd "C-j") (kbd "SPC w j"))
(global-set-key (kbd "C-k") (kbd "SPC w k"))
(global-set-key (kbd "C-h") (kbd "SPC w h"))
(global-set-key (kbd "C-l") (kbd "SPC w l"))

;; multiple-cursors
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; (define-key evil-visual-state-map "C-n" 'evil-mc-make-and-goto-next-match)
;; (define-key evil-visual-state-map "C-p" 'evil-mc-make-and-goto-prev-match)
;; (define-key evil-visual-state-map "C-x" 'evil-mc-skip-and-goto-prev-match)

;; (define-key evil-normal-state-map "S" 'evil-mc-undo-all-cursors)
