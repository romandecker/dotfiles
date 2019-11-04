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

;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula)
;;   (load-theme 'org-beautify))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-oceanic-next)
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
  (setq recentf-max-saved-items 1000)
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

(when (window-system)
  (set-frame-font "Fira Code"))

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))


(add-hook 'helm-major-mode-hook
      (lambda ()
        (setq auto-composition-mode nil)))

(provide 'my-visuals)
;;; my-visuals.el ends here
