;;; package --- My custom config for editing vtl files
;;; Commentary:
;;; Code:

(require 'vtl)

(add-to-list 'my/auto-minor-mode-alist '("\\.vtl\\'" . vtl-mode))

(provide 'my-vtl)
;;; my-vtl.el ends here
