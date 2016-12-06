;;; package --- Initial setup
;;; Commentary:
;;; Code:

;; add some package archives
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)


(setq package-enable-at-startup nil)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; load customize file
(defvaralias 'my/custom-file 'custom-file)
(defconst my/custom-file "~/.emacs.d/customize.el")
(when (file-exists-p my/custom-file)
  (load my/custom-file))

;; Ability to eliminate minor modes from the modeline
(use-package diminish :ensure t :config)

;; general-purpose string-manipulation library
(use-package s :ensure t :config)

;; general-purpose list library (-map, etc...)
(use-package dash :ensure t :config)

(use-package restart-emacs
  :ensure t
  :after evil-leader
  :config
  (evil-leader/set-key
    "Q R"   'restart-emacs
    "Q Q" 'kill-emacs)
  (which-key-add-key-based-replacements
    "SPC q" "Quitting"))

(provide 'my-setup)
;;; my-setup.el ends here
