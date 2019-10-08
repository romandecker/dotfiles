;;; package --- My custom code for inserting box-drawing characters
;;; Commentary:
;;; Code:

(defun my/insert-box-t ()
  "Helper to insert the box-drawing character ┬"
  (interactive)
  (insert "┬"))

(defun my/insert-box-inverse-t ()
  "Helper to insert the box-drawing character ┴"
  (interactive)
  (insert "┴"))

(defun my/insert-box-E ()
  "Helper to insert the box-drawing character ┬"
  (interactive)
  (insert "├"))

(defun my/insert-box-3 ()
  "Helper to insert the box-drawing character ┤"
  (interactive)
  (insert "┤"))

(defun my/insert-box-+ ()
  "Helper to insert the box-drawing character ┼"
  (interactive)
  (insert "┼"))

(defun my/insert-box-dash ()
  "Helper to insert the box-drawing character ─"
  (interactive)
  (insert "─"))

(defun my/insert-box-pipe ()
  "Helper to insert the box-drawing character ─"
  (interactive)
  (insert "│"))

(defun my/insert-box-top-left ()
  "Helper to insert the box-drawing character ┌"
  (interactive)
  (insert "┌"))

(defun my/insert-box-top-right ()
  "Helper to insert the box-drawing character ┐"
  (interactive)
  (insert "┐"))

(defun my/insert-box-bottom-left ()
  "Helper to insert the box-drawing character └"
  (interactive)
  (insert "└"))

(defun my/insert-box-bottom-right ()
  "Helper to insert the box-drawing character ┘"
  (interactive)
  (insert "┘"))

(defun my/insert-box-arrow-up ()
  "Helper to insert the character ▲"
  (interactive)
  (insert "▲"))

(defun my/insert-box-arrow-down ()
  "Helper to insert the character ▼"
  (interactive)
  (insert "▼"))

(defun my/insert-box-arrow-left ()
  "Helper to insert the character ◄"
  (interactive)
  (insert "◄"))

(defun my/insert-box-arrow-right ()
  "Helper to insert the character ►"
  (interactive)
  (insert "►"))

(defun my/insert-o ()
  "Helper to insert the character ○"
  (interactive)
  (insert "○"))

(defun my/insert-c-down()
  "Helper to insert the character ◠"
  (interactive)
  (insert "◠"))

(defun my/insert-c-up ()
  "Helper to insert the character ◠"
  (interactive)
  (insert "◡"))


(defun my/insert-box ()
  "Helper to insert a fully drawn box"
  (interactive)
  (let ((indentation (loop repeat (current-column) concat " ")))
    (save-excursion
      (insert           "┌───────────┐\n")
      (insert (format "%s│           │\n" indentation))
      (insert (format "%s└───────────┘\n" indentation)))
    (next-line)
    (forward-char 2)))

(defun my/insert-cylinder ()
  "Helper to insert a fully drawn box"
  (interactive)
  (let ((indentation (loop repeat (current-column) concat " ")))
    (save-excursion
      (insert           " ,──────────.\n")
      (insert (format "%s│`──────────'│\n" indentation))
      (insert (format "%s│            │\n" indentation))
      (insert (format "%s `──────────'\n" indentation)))
  (next-line 2)
  (forward-char 2)))


(my/define-leader-map
  "i a e"   'my/insert-box-E
  "i a S-e" 'my/insert-box-3
  "i a 3"   'my/insert-box-3
  "i a +"   'my/insert-box-+
  "i a -"   'my/insert-box-dash
  "i a |"   'my/insert-box-pipe
  "i a t l" 'my/insert-box-top-left
  "i a r"   'my/insert-box-top-left
  "i a t r" 'my/insert-box-top-right
  "i a S-r" 'my/insert-box-top-right
  "i a b l" 'my/insert-box-bottom-left
  "i a l"   'my/insert-box-bottom-left
  "i a b r" 'my/insert-box-bottom-right
  "i a j"   'my/insert-box-bottom-right
  "i a k"   'my/insert-box-arrow-up
  "i a j"   'my/insert-box-arrow-down
  "i a >"   'my/insert-box-arrow-right
  "i a <"   'my/insert-box-arrow-left
  "i a t t" 'my/insert-box-t
  "i a T"   'my/insert-box-inverse-t
  "i a b b" 'my/insert-box
  "i a o"   'my/insert-o
  "i a c k" 'my/insert-c-up
  "i a c j" 'my/insert-c-down
  "i a c c" 'my/insert-cylinder)

(provide 'my-ascii-art)
;;; my-boxes.el ends here
