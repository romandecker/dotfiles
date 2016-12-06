;;; package --- My custom config for docker-related stuff
;;; Commentary:
;;; Code:

(use-package dockerfile-mode
  :ensure t
  :config)

(use-package docker
  :ensure t
  :config
  :general
  (:prefix my/leader
           :keymaps 'normal
           "a d p" 'docker-containers
           "a d i" 'docker-images)
  (:states 'normal
   :keymaps 'docker-containers-mode-map
    "d" 'docker-containers-rm-selection
    "r" 'docker-containers-restart-selection
    "s" 'docker-containers-start-selection
    "S" 'docker-containers-stop-selection
    "!" 'docker-containers-shell-selection
    "L" 'docker-containers-logs-selection))

(provide 'my-docker)
;;; my-docker.el ends here

  (evil-set-initial-state 'tabulated-list-mode 'emacs)
