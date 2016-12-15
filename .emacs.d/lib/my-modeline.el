;;; package --- My custom modeline config
;;; Commentary:
;;; Code:
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'automatic)
  (sml/setup))




;; TODO some WIP-code for custom modeline below

;; use force-mode-line-update to update modeline!
(require 's)
(defcustom
  my/path-transformers
  '(my/shorten-home my/shorten-projects my/shorten-dotfiles)
  "List of replacers that will be applied in order to (buffer-file-name) before displaying it in the mode-line")


(defun my/shorten-home (full-path)
  "Transform /Users/roman/foo or /home/roman/foo to ~/foo."
  (let ((home (expand-file-name "~")))
    (if (s-starts-with? home full-path)
        (concat "~" (substring full-path (length home)))
      full-path)))

(defun my/shorten-projects (path)
  (replace-regexp-in-string "~/projects/" ":P:/" path))

(defun my/shorten-dotfiles (path)
  (replace-regexp-in-string "~/.dotfiles/" ":.:/" path))

(defun my/replacer-pipeline ()
  (let ((path (buffer-file-name)))
    (dolist (replacer my/path-transformers)
      (if (functionp replacer)
          (progn
            (message "applying %s" replacer)
            (setq path (apply replacer `(,path))))))
    path))

;; (setq mode-line-format
;;       (list
;;        " "
;;        '(:eval (propertize (my/replacer-pipeline) 'face 'font-lock-keyword-face))))

(force-mode-line-update)


(provide 'my-modeline)
;;; my-modeline.el ends here
