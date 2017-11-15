;;; package --- My custom abbrev config
;;; Commentary:
;;; Code:

(setq
 abbrev-file-name "~/.emacs.d/.emacs-abbreviations")

(setq-default
 abbrev-mode t)

(defun my/save-abbrevs ()
  (interactive)
  (write-abbrev-file))

(defun my/reload-abbrevs ()
  (interactive)
  (message "Reloading abbrevs")
  (quietly-read-abbrev-file)
  (if (projectile-project-p)
      (let ((project-abbrevs-file (my/get-local-abbrev-file-name)))
        (when (file-exists-p project-abbrevs-file)
          (quietly-read-abbrev-file project-abbrevs-file)))))

(defun my/add-global-abbrev (abbreviation expansion)
  "Add an ABBREVIATION that expands to EXPANSION."
  (interactive "MAbbreviation: \nMExpansion: ")
  (define-abbrev global-abbrev-table abbreviation expansion))

(defun my/get-local-abbrev-file-name ()
  (concat (projectile-project-root) ".emacs-abbreviations"))

(defun my/open-local-abbrev-file ()
  (interactive)
  (find-file (my/get-local-abbrev-file-name)))



(my/reload-abbrevs)

;; (add-hook 'projectile-after-switch-project-hook #'my/reload-abbrevs)

(my/define-leader-map "? a a" 'list-abbrevs)
(my/define-leader-map "? a l" 'list-abbrevs)
(my/define-leader-map "? a e" 'edit-abbrevs)
(my/define-leader-map ". a" 'my/open-local-abbrev-file)

(provide 'my-abbrev)
;;; my-abbrev.el ends here
