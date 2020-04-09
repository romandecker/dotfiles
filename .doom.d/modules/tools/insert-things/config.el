;;; tools/insert-things/config.el -*- lexical-binding: t; -*-

(defun insert-things/uuid ()
  (interactive)
  (let ((uuid (shell-command-to-string "uuidgen")))
    (insert (s-trim-right (downcase uuid)))))

(defun insert-things/iso8601-timestamp ()
  (interactive)
  (insert
    (format-time-string "%Y-%m-%dT%T.%3NZ" nil t)))


(map! :leader "i t" #'insert-things/iso8601-timestamp)
(map! :leader "i u" #'insert-things/uuid)
