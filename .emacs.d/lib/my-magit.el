;;; package --- My custom magit config
;;; Commentary:
;;; Code:
(use-package evil-magit
  :ensure t
  :defer 5
  :config
  (message "evil-magit has loaded!"))

(use-package git-timemachine
  :ensure t
  :after hydra
  :config
  (defhydra hydra-git-timemachine ()
    "git-timemachine"
    ("j" git-timemachine-show-next-revision "Next commit")
    ("k" git-timemachine-show-previous-revision "Previous commit")
    ("q" git-timemachine-quit "Quit timemachine")))

(use-package gitconfig-mode
  :ensure t
  :config)

(use-package gitignore-mode
  :ensure t
  :config)

(use-package gitattributes-mode
  :ensure t
  :config)

(use-package git-gutter-fringe+
  :ensure t
  :config
  (setq git-gutter-fr+-side 'right-fringe)
  (set-face-foreground 'git-gutter+-modified "#ea8212")
  (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
  (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
  (add-hook 'prog-mode-hook #'git-gutter+-mode)
  (evil-leader/set-key
    "g h s" 'git-gutter+-stage-hunks
    "g h r" 'git-gutter+-revert-hunk
    "g h h" 'git-gutter+-show-hunk-inline-at-point
    "g h H" 'git-gutter+-show-hunk)
  (which-key-add-key-based-replacements
    "SPC g h"   "Hunks"))

(fringe-helper-define 'git-gutter-fr+-modified nil
  "...x...."
  "....xx.."
  "......x."
  "....xx.."
  "..xx...."
  ".x......"
  "..xx...."
  "....x...")

(defun my-git/start-time-machine ()
  (interactive)
  (git-timemachine)
  (hydra-git-timemachine/body))

(provide 'my-magit)
;;; my-magit.el ends here
