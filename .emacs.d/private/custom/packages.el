;;; packages.el --- custom layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Roman Decker <roman@roman-ubuntu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `custom-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `custom/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `custom/pre-init-PACKAGE' and/or
;;   `custom/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq custom-packages
      '(
        company
        zoom-window
        ))

(defun custom/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0
            company-minimum-prefix-length 1
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil)

      (defvar-local company-fci-mode-on-p nil)

      (defun company-turn-off-fci (&rest ignore)
        (when (boundp 'fci-mode)
          (setq company-fci-mode-on-p fci-mode)
          (when fci-mode (fci-mode -1))))

      (defun company-maybe-turn-on-fci (&rest ignore)
        (when company-fci-mode-on-p (fci-mode 1)))

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")

      ;; key bindings
      (defun spacemacs//company-complete-common-or-cycle-backward ()
        "Complete common prefix or cycle backward."
        (interactive)
        (company-complete-common-or-cycle -1))
      (spacemacs//auto-completion-set-RET-key-behavior 'company)
      (spacemacs//auto-completion-set-TAB-key-behavior 'company)
      (spacemacs//auto-completion-setup-key-sequence 'company)
      (let ((map company-active-map))
        (define-key map (kbd "C-/") 'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d") 'company-show-doc-buffer)
        (define-key map (kbd "C-j") 'company-select-next)
        (define-key map (kbd "C-k") 'company-select-previous)
        (define-key map (kbd "C-l") 'company-complete-selection))
      ;; Nicer looking faces
      (custom-set-faces
       '(company-tooltip-common
         ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection
         ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
      ;; Transformers
      (defun spacemacs//company-transformer-cancel (candidates)
        "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
        (unless (member company-prefix company-mode-completion-cancel-keywords)
          candidates))
      (setq company-transformers '(spacemacs//company-transformer-cancel
                                   company-sort-by-occurrence)))))
;;; packages.el ends here
