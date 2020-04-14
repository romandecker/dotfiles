;;; tools/+magit/config.el -*- lexical-binding: t; -*-

(after! magit
  (defvar git-hosting-provider nil)
  (defvar bitbucket-baseurl nil)
  (defvar bitbucket-project nil)
  (defvar bitbucket-repo nil)
  (defvar github-baseurl "https://github.com")
  (defvar github-user nil)
  (defvar github-repo nil)

  (defun +magit/guess-provider ()
    "Guess the git hosting provider based on the configured origin url."
    (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
      (cond
       ((string-match-p "github.com" origin-url) 'github)
       ((string-match-p "gitlab.com" origin-url) 'gitlab)
       (t (error
           "Cannot guess git hosting provider from configured origin url (%s), please set bitbucket-provider manually"
           origin-url)))))

  (defun +magit/guess-bitbucket-url ()
    "Guess the bitbucket base-url based on the configured origin url."
    (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
      (if (string-match "ssh://\\(:?.*?\\)@\\(.*?\\)/" origin-url)
          (format "https://%s" (match-string 2 origin-url))
        (error "Protocols other than ssh not yet supported"))))

  (defun +magit/guess-bitbucket-project ()
    "Guess the bitbucket project based on the configured origin url."
    (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
      (if (string-match "ssh://\\(.*?\\)@\\(.*\\)/\\(.*?\\)/" origin-url)
          (match-string 3 origin-url)
        (error "Unsupported origin config - Please set bitbucket-project manually"))))

  (defun +magit/guess-bitbucket-repo ()
    "Guess the bitbucket repo based on the configured origin url."
    (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
      (if (string-match "ssh://\\(.*?\\)@\\(.*\\)/\\(.*?\\)/\\(.*?\\)\\.git" origin-url)
          (match-string 4 origin-url)
        (error "Unsupported origin config - Please set bitbucket-repo manually"))))

  (defun +magit/build-bitbucket-line-link (ref path line)
    "Build a bitbucket-web-link for the given ref, path and line"
    (let* ((baseurl (or bitbucket-baseurl (+magit/guess-bitbucket-url)))
           (project (or bitbucket-project (+magit/guess-bitbucket-project)))
           (repo (or bitbucket-repo (+magit/guess-bitbucket-repo)))
           (query-string (if ref (format "?at=%s" ref) "")))
      (format "%s/projects/%s/repos/%s/browse/%s%s#%s" baseurl project repo path query-string line)))

  (defun +magit/guess-github-user ()
    "Guess the github user based on the configured origin url."
    (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
      (if (string-match "git@github.com:\\(.*?\\)/" origin-url)
          (match-string 1 origin-url)
        (error "Unsupported origin config - Please set github-user manually"))))

  (defun +magit/guess-github-repo ()
    "Guess the github repo based on the configured origin url."
    (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
      (if (string-match "git@github.com:\\(.*?\\)/\\(.*?\\)\\.git$" origin-url)
          (match-string 2 origin-url)
        (error "Unsupported origin config - Please set github-repo manually"))))

  (defun +magit/build-github-line-link (ref path line)
    "Build a github-web-link for the given ref, path an line"
    (let* ((user (or github-user (+magit/guess-github-user)))
           (repo (or github-repo (+magit/guess-github-repo))))
      (format "%s/%s/%s/blob/%s/%s#L%d" github-baseurl user repo (or ref "master") path line)))

  (defvar git-hosting-providers
    '((bitbucket . ((build-line-link . +magit/build-bitbucket-line-link)))
      (github    . ((build-line-link . +magit/build-github-line-link)))))

  (defun +magit/get-line-link (&optional omit-ref)
    (let* ((ref (if omit-ref nil (magit-git-string "rev-parse" "--abbrev-ref" "HEAD")))
           (provider (or git-hosting-provider (+magit/guess-provider)))
           (hosting-provider-config (alist-get provider git-hosting-providers))
           (build-line-link-fn (alist-get 'build-line-link hosting-provider-config))
           (path (file-relative-name (buffer-file-name) (projectile-project-root)))
           (line-number (line-number-at-pos (point))))
      (funcall build-line-link-fn ref path line-number)))

  (defun +magit/open-current-line-in-browser (omit-branch)
    (interactive "P")
    (browse-url (+magit/get-line-link omit-branch))))

