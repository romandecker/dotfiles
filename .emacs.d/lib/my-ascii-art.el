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

(provide 'my-ascii-art)
;;; my-boxes.el ends here
