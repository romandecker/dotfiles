(defun my-term-funcs/send-ctrl-a ()
  "Go to beginning of line."
  (interactive)
  (term-send-raw-string "\C-a"))

(defun my-term-funcs/send-ctrl-e ()
  "Go to end of line."
  (interactive)
  (term-send-raw-string "\C-e"))

(defun my-term-funcs/send-ctrl-r ()
  "Start reverse history search."
  (interactive)
  (term-send-raw-string "\C-r"))

(defun my-term-funcs/send-ctrl-p ()
  "Go back in history."
  (interactive)
  (term-send-raw-string "\C-p"))

(defun my-term-funcs/send-ctrl-n ()
  "Go forward in history."
  (interactive)
  (term-send-raw-string "\C-n"))

(defun my-term-funcs/send-ctrl-c ()
  "Send Ctrl+C."
  (interactive)
  (term-send-raw-string "\C-c"))

(defun my-term-funcs/send-ctrl-d ()
  "Send EOF."
  (interactive)
  (term-send-raw-string "\C-d"))

(defun my-term-funcs/send-ctrl-z ()
  "Suspend."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun my-term-funcs/toggle-term ()
  "Toggle the dedicated terminal."
  (interactive)
  (multi-term-dedicated-toggle)
  (multi-term-dedicated-select))

(provide 'my-term-funcs)
