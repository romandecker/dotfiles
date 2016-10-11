(defun my-term-funcs/send-ctrl-c ()
  "Send Ctrl+C."
  (interactive)
  (term-send-raw-string "\C-c"))

(defun my-term-funcs/send-ctrl-z ()
  "Suspend."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun my-term-funcs/send-space ()
  "Send space."
  (interactive)
  (term-send-raw-string " "))

(defun my-term-funcs/toggle-term ()
  "Toggle the dedicated terminal."
  (interactive)
  (multi-term-dedicated-toggle)
  (multi-term-dedicated-select))

(defun my-term-funcs/send-tab ()
  "Send tab."
  (interactive)
  (term-send-raw-string "\t"))

(defun my-term-funcs/send-esc ()
  "Send Meta+."
  (interactive)
  (term-send-raw-string "\e"))

(defun my-term-funcs/send-m-dot ()
  "Send Meta+."
  (interactive)
  (term-send-raw-string "\e."))

(provide 'my-term-funcs)
