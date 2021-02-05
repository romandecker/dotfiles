;;; package --- polymode
;;; Commentary: My custom polymode config
;;; Code:

(use-package polymode
  :ensure t
  :config

  (defcustom pm-host/rjsx
    (pm-host-chunkmode :name "rjsx"
                       :mode 'rjsx-mode)
    "RJSX host chunkmode"
    :group 'poly-hostmodes
    :type 'object)

  )

(use-package poly-markdown
  :ensure t
  :config)


(provide 'my-polymode)
;;; my-polymode.el ends here
