;;; package --- My custom eshell config
;;; Commentary:
;;; Code:

(defun my/eshell-kill-line ()
  (interactive)
  (eshell-bol)
  (call-interactively 'evil-delete-line))

(defun my/eshell-change-line ()
  (interactive)
  (my/eshell-kill-line)
  (evil-insert-state))

(defun my/get-eshell-prompt-length ()
  (let ((prompt (funcall eshell-prompt-function)))
    (length prompt)))

(defun my/eshell-eop-p ()
  "Return t when point is at the end of the prompt."
  (let ((i (current-column))
        (l (my/get-eshell-prompt-length)))
    (eq (+ i 1) l)))

(defun my/eshell-i-dwim ()
  (interactive)
  (message "p: %s" (my/eshell-eop-p))
  (if (my/eshell-eop-p)
      (call-interactively 'evil-append)
    (call-interactively 'evil-insert)))

(defun my/setup-eshell ()
  (interactive)
  (company-mode 0)

  (general-define-key
   :keymaps 'local
   :states 'normal
    "0"   'eshell-bol
    "c c" 'my/eshell-change-line
    "i"   'my/eshell-i-dwim
    )
  (general-define-key
   :keymaps 'local
   :states 'insert
    "C-a"   'eshell-bol
    "C-e"   'move-end-of-line
    "C-c"   'eshell-kill-process
    "C-u"   'eshell-kill-input
    "C-p"   'eshell-previous-input
    "C-n"   'eshell-next-input
    "C-r"   'eshell-previous-matching-input
    "C-d"   'eshell-send-eof-to-process
    "TAB"   'eshell-pcomplete
    [tab]   'eshell-pcomplete
    ))


(provide 'my-eshell)
;;; my-eshell.el ends here
