(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq tab-width 2)

(setq x-select-enable-clipboard t)

;; do not update clipboard just by selecting text
(fset 'evil-visual-update-x-selection 'ignore)
