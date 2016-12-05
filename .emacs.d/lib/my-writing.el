;;; package --- My custom config for editing LaTeX
;;; Commentary:
;;; Code:

(use-package latex-preview-pane :ensure t :config
  (latex-preview-pane-enable))

(require 'langtool)

(setq langtool-language-tool-jar
      "~/bin/LanguageTool-3.5/languagetool-commandline.jar")
(setq langtool-default-language "en-US")

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)

(evil-leader/set-key-for-mode 'latex-mode
  "SPC s" 'langtool-check
  "SPC S" 'langtool-check-done)

(add-hook 'latex-mode-hook '(flyspell-mode t))

(evil-leader/set-key
  "t s" 'flyspell-prog-mode
  "t S" 'flyspell-mode)

(evil-define-key 'normal flyspell-mode-map
  (kbd "] s") 'flyspell-goto-next-error)

(use-package flyspell-popup
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  (evil-define-key 'normal flyspell-mode-map
    (kbd "g =") 'flyspell-popup-correct))

(use-package synosaurus
  :ensure t
  :config
  (setq synosaurus-choose-method 'popup)
  (define-key evil-normal-state-map (kbd "g s") 'synosaurus-choose-and-replace)
  (evil-leader/set-key
    "a s" 'synosaurus-lookup))

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "de"
        google-translate-translation-directions-alist '(("en" . "de") ("de" . "en")))
  (define-key evil-normal-state-map (kbd "g t") 'google-translate-at-point)
  (define-key evil-normal-state-map (kbd "g T") 'google-translate-at-point-reverse)
  (evil-leader/set-key
    "a t" 'google-translate-smooth-translate))

(use-package helm-bibtex
  :ensure t
  :config
  (defvar my-writing/helm-source-bibtex
    (helm-build-sync-source "BibTeX entries"
      :init 'bibtex-completion-init
      :candidates 'bibtex-completion-candidates
      :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
      :action (helm-make-actions
               "Insert BibTeX key"          'helm-bibtex-insert-key
               "Insert BibTeX entry"        'helm-bibtex-insert-bibtex))
    "Helm source for searching through bibtex entries.")
  
  (defun my/helm-bibtex (&optional arg)
    "Search BibTeX entries.

    With a prefix ARG, the cache is invalidated and the bibliography
    reread."
    (interactive "P")
    (when arg
        (bibtex-completion-clear-cache))
    (helm :sources (list my-writing/helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :candidate-number-limit 500)))

(use-package writegood-mode
  :ensure t
  :config)

(provide 'my-writing)
;;; my-writing.el ends here
