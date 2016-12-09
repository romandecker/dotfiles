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

(general-define-key
 :prefix my/local-leader
 :keymaps 'latex-mode
 :states 'normal
  "SPC s" 'langtool-check
  "SPC S" 'langtool-check-done)

(add-hook 'latex-mode-hook '(flyspell-mode t))

(general-define-key
 :prefix my/leader
 :states 'normal
  "t s" 'flyspell-prog-mode
  "t S" 'flyspell-mode)

(general-define-key
 :states 'normal
 :keymaps 'flyspell-mode-map
  "] s" 'flyspell-goto-next-error)

(use-package flyspell-popup
  :ensure t
  :general
  (:keymaps 'flyspell-mode-map
   :states 'normal
   "g =" 'flyspell-popup-correct)
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(use-package synosaurus
  :ensure t
  :general
  (:keymaps 'normal
   "g s" 'synosaurus-choose-and-replace)
  (:prefix my/leader
   :states 'normal
    "a s" 'synosaurus-lookup)
  :config
  (setq synosaurus-choose-method 'popup))

(use-package google-translate
  :ensure t
  :demand t
  :general
  (:keymaps 'normal
   "g t" 'google-translate-at-point
   "g T" 'google-translate-at-point-reverse)
  (:prefix my/leader
   :keymaps 'normal
    "a t" 'google-translate-smooth-translate)
  :config
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "de"
        google-translate-translation-directions-alist '(("en" . "de") ("de" . "en"))))

(require 'my-company-bibtex)
(add-to-list 'company-backends 'company-bibtex)

(use-package writegood-mode
  :ensure t
  :config)

(provide 'my-writing)
;;; my-writing.el ends here
