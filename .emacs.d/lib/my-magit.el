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
  (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
  (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
  )

(defun my-git/start-time-machine ()
  (interactive)
  (git-timemachine)
  (hydra-git-timemachine/body))


(provide 'my-magit)
;;; my-magit.el ends here
