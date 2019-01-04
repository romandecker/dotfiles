;;; package --- Various visual tweaks
;;; Commentary:
;;; Code:

;; (use-package flatui-theme
;;   :ensure t
;;   :config
;;   (load-theme 'flatui t))

;; (use-package spacemacs-theme
;;   :ensure t
;;   :config
;;   (load-theme 'spacemacs-light t))

;; (use-package twilight-bright-theme
;;   :ensure t
;;   :config
;;   (load-theme 'twilight-bright t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-day t))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn)
;;   (load-theme 'org-beautify))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula)
  (load-theme 'org-beautify))

(use-package beacon
  :ensure t
  :defer 6
  :config
  (setq beacon-color "#cccccc")
  (beacon-mode 1))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook #'rainbow-mode)
  (add-hook 'emacs-lisp-mode #'rainbow-mode)
  (add-hook 'css-mode #'rainbow-mode)
  (add-hook 'html-mode #'rainbow-mode)
  (add-hook 'web-mode #'rainbow-mode))

(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color "blue"))

;; for keeping track of recent files, provides helm-recentf with data
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1))

(use-package atomic-chrome
  :ensure t
  :defer 15
  :config
  (atomic-chrome-start-server)
  (message "atomic-chrome started!"))

(require 'zoom-frm)

(defhydra hydra-zoom ()
  "Zoom"
  ("+" zoom-frm-in "in")
  ("-" zoom-frm-out "out")
  ("0" zoom-frm-unzoom "reset"))


(setq scroll-conservatively 10000
      scroll-margin 5)

(provide 'my-visuals)
;;; my-visuals.el ends here
