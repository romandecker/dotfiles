;;; package --- My custom config for editing LaTeX
;;; Commentary:
;;; Code:

(require 'langtool)

(setq langtool-language-tool-jar
      "~/bin/LanguageTool-4.0/languagetool-commandline.jar")
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

(general-evil-define-key 'normal 'LaTeX-mode-map
  "C-j" 'my/window-down)

(general-define-key
 :prefix my/local-leader
 :states 'normal
 :keymaps 'LaTeX-mode-map
  "s"  'langtool-check
  "S"  'langtool-check-done)


(require 'my-company-bibtex)

(defun my/setup-latex-mode ()
  (flyspell-mode t)

  ;; this will make paragraph-text-objects behave like everywhere else
  ;; (normally the LaTeX-mode redefines them to be what LaTex would
  ;; render as paragraphs, but this behaviour is kind of confusing
  ;; when editing...)
  (setq-local paragraph-start "\\|[ 	]*$")
  (setq-local paragraph-separate "[ 	]*$")

  ;; add company-bibtex to the front of available company backends
  (make-local-variable 'company-backends)
  (setq company-backends (copy-tree company-backends))
  (add-to-list 'company-backends 'company-bibtex))


(add-hook 'LaTeX-mode-hook #'my/setup-latex-mode)

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


(use-package writegood-mode
  :ensure t
  :config)

(setq TeX-PDF-mode t
      TeX-parse-self t
      TeX-auto-save t)

(use-package company-auctex
  :ensure t
  :config
  (setq company-bibtex-key-regex "[[:alnum:]:_-]*")
  (company-auctex-init))

(use-package pdf-tools
  :ensure t
  :general
  (:keymaps 'pdf-view-mode-map
   :states 'normal
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page
   "C-d" 'pdf-view-next-page
   "C-u" 'pdf-view-previous-page
   "/" 'isearch-forward
   "?" 'isearch-backward)
  :config
  (evil-set-initial-state 'pdf-view-mode 'normal))

(provide 'my-writing)
;;; my-writing.el ends here
