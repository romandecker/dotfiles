;;; package --- Initial setup
;;; Commentary:
;;; Code:

;; add some package archives
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)


(setq package-enable-at-startup nil)

;; prefer horizontal splits over vertical splits
(setq split-height-threshold nil
      split-width-threshhold 0)

(setq large-file-warning-threshold
      (* 1024 100) ; 100 KiB is already a huge file
      )

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

(use-package tablist
  :ensure t
  :config)

(require 'add-hook-x)
(require 'help-fns+)

(provide 'my-setup)
;;; my-setup.el ends here
