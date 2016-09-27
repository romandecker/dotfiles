;; (setq-default indent-tabs-mode nil)
;; (setq default-tab-width 2)
;; (setq tab-width 2)

;; (defun term-send-ctrl-a ()
;;   "Go to beginning of line"
;;   (interactive)
;;   (term-send-raw-string "\C-a"))

;;   (add-hook 'term-mode-hook
;;             (lambda ()
;;               (add-to-list 'term-bind-key-alist '("C-w" . term-send-backward-kill-word))
;;               evil-define-key 'insert term-raw-map (kbd "\C-a") 'term-send-ctrl-a))
