;;; package --- Start of a JIRA interface
;;; Commentary:
;;; Code:

(use-package helm-jira
  :config

  (setq
   ;; URL of your JIRA instance (should not end in a slash)
   helm-jira-url "https://tracker.zooplus.de"

   ;; The ID of the board you want to interact with
   helm-jira-board-id 763

   ;; The username to use to log in to JIRA
   helm-jira-username "romande"

   ;; The JIRA-project you want to interact with
   helm-jira-project "prs"


   ;; URL of the stash/bitbucket API (should not end in a slash)
   helm-jira-stash-url "https://src.private.zooplus.net"

   ;; The stash/bitbucket repo you want to interact with
   helm-jira-repo "prs")

  (my/define-leader-map
   "J p" 'helm-jira-helm-pull-requests
   "J i" 'helm-jira-helm-issues))


(provide 'my-jira)
;;; my-jira.el ends here
