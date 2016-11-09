;;; package --- My custom config for editing LaTeX
;;; Commentary:
;;; Code:

(require 'langtool)

;; (setq langtool-language-tool-jar
;;       "~/bin/LanguageTool-3.5/languagetool-commandline.jar")

;; (setq langtool-default-language "en-US")

;; (defun langtool-autoshow-detail-popup (overlays)
;;   (when (require 'popup nil t)
;;     ;; Do not interrupt current popup
;;     (unless (or popup-instances
;;                 ;; suppress popup after type `C-g` .
;;                 (memq last-command '(keyboard-quit)))
;;       (let ((msg (langtool-details-error-message overlays)))
;;         (popup-tip msg)))))

;; (setq langtool-autoshow-message-function
;;       'langtool-autoshow-detail-popup)

;; (evil-leader/set-key-for-mode 'tex-mode
;;     "SPC t s"   'langtool-check)

(provide 'my-latex)
;;; my-latex.el ends here
