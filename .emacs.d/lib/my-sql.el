;;; package --- My custom sql config
;;; Commentary:
;;; Code:

(use-package sqlup-mode
  :ensure t
  :config
  (add-hook 'sql-mode-hook (lambda () (sqlup-mode 1))))

(provide 'my-sql)
;;; my-sql.el ends here
