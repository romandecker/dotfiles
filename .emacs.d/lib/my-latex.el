;;; package --- My custom config for editing LaTeX
;;; Commentary:
;;; Code:

(use-package latex-preview-pane
  :ensure t
  :config
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
  "t s" 'flyspell-prog-mode)

(evil-define-key 'normal flyspell-mode-map
  (kbd "] s") 'flyspell-goto-next-error)

(use-package flyspell-popup
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  (evil-define-key 'normal flyspell-mode-map
    (kbd "g =") 'flyspell-popup-correct))

(provide 'my-latex)
;;; my-latex.el ends here
