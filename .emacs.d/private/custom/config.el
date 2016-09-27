(setq default-tab-width 2)
(setq tab-width 2)

(setq x-select-enable-clipboard t)

;; do not update clipboard just by selecting text
(fset 'evil-visual-update-x-selection 'ignore)

(setq-default
 indent-tabs-mode nil
 ;; restore session after restart
 dotspacemacs-auto-resume-layouts t
 )
