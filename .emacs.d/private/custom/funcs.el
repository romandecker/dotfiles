(defun term-send-ctrl-a ()
  "Go to beginning of line"
  (interactive)
  (term-send-raw-string "\C-a"))

(defun term-send-ctrl-e ()
  "Go to end of line"
  (interactive)
  (term-send-raw-string "\C-e"))

(defun term-send-ctrl-r ()
  "Start reverse history search"
  (interactive)
  (term-send-raw-string "\C-r"))

(defun term-send-ctrl-p ()
  "Go back in history"
  (interactive)
  (term-send-raw-string "\C-p"))

(defun term-send-ctrl-n ()
  "Go forward in history"
  (interactive)
  (term-send-raw-string "\C-n"))

(defun term-send-ctrl-c ()
  "Send Ctrl+C"
  (interactive)
  (term-send-raw-string "\C-c"))

(defun term-send-ctrl-d ()
  "Send EOF"
  (interactive)
  (term-send-raw-string "\C-d"))

(defun term-send-ctrl-z ()
  "Suspend"
  (interactive)
  (term-send-raw-string "\C-z"))
