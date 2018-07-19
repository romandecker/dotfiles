;;; -*- lexical-binding: t -*-
;;; package --- Start of a JIRA interface
;;; Commentary:
;;; Code:


(defvar jira/url nil "JIRA url to use, should include protocol and not end in a slash.")
(defvar jira/board-id nil "ID of the JIRA-board you want to work with.")
(defvar jira/username nil "Username to use when logging in to JIRA.")
(defvar jira/password nil "Password to use when logging in to JIRA.")
(defvar jira/stash-url nil "Stash url to use (for bitbucket API, should include protocol and not end in a slash).")
(defvar jira/project nil "The JIRA project to use for bitbucket API requests.")
(defvar jira/repo nil "The BitBucket repo to use for bitbucket API requests.")

(setq jira/url "https://tracker.zooplus.de"
      jira/board-id 763
      jira/username "romande"
      jira/stash-url "https://src.private.zooplus.net"
      jira/project "prs"
      jira/repo "prs")


(defun jira/build-basic-auth-token ()
  "Build the base64-encoded auth token from `jira/username' and `jira/password'."
  (base64-encode-string (format "%s:%s" jira/username jira/password)))

(defun jira/build-auth-header ()
  "Build the Authorization-Header for JIRA requests."
  (format "Basic %s" (jira/build-basic-auth-token)))

(defun jira/ensure-password ()
  "Ensures that `jira/password' is set."
  (when (not jira/password)
    (jira/read-password)))

(defun jira/read-password ()
  "Read a new value for `jira/password'."
  (setq jira/password (read-passwd (format "JIRA-Password for %s: " jira/username)))
  nil)

(defun jira/logout ()
  "Unset `jira/password'."
  (interactive)
  (setq jira/password nil)
  (message "Cleared JIRA password"))

(defun jira/request (&rest args)
  "Call `request' with the supplied `ARGS', but ensure that a password is set and credentials are supplied."
  (jira/ensure-password)
  (apply 'request (append args
                          `(:headers (("Authorization" . ,(jira/build-auth-header)))))))

(defun jira/fetch-issues (callback)
  "Fetch all open issues for the configured board and call `CALLBACK' with the resulting list."
  (jira/request
   (format "%s/rest/agile/1.0/board/%s/issue" jira/url jira/board-id)
   :params '(("fields" . "summary")
             ("maxResults" . "200")
             ("jql" . "sprint in openSprints()"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (funcall callback (alist-get 'issues data))))))

(defun jira/fetch-pull-requests (callback)
  "Fetch all open pull requests for the configured project and repo and call `CALLBACK' with the resulting list."
  (jira/request
   (format "%s/rest/api/1.0/projects/%s/repos/%s/pull-requests" jira/stash-url jira/project jira/repo)
   :parser 'json-read
   :success (function* (lambda (&key data &allow-other-keys)
                         (funcall callback (alist-get 'values data))))))

(defun jira/get-issue-details (issue-id callback)
  "Fetch the details for a single issue by its `ISSUE-ID' (=purely numeric, not its key), and call `CALLBACK' with the resulting list of issues."
  (request
   (format "%s/rest/dev-status/latest/issue/detail" jira/url)
   :params `(("issueId" . ,issue-id)
             ("applicationType" . "stash")
             ("dataType" . "pullrequest"))
   :success (function* (lambda (&key data &allow-other-keys)
                         (funcall callback data)))))

(defun jira/build-candidate-list-from-issues (issues)
  "Take `ISSUES' as returned by jira/fetch-issues and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (issue)
     (let* ((key (alist-get 'key issue))
            (fields (alist-get 'fields issue))
            (summary (alist-get 'summary fields)))
       `(,(format "%s: %s" key summary) . ,issue)))
   issues))


(defun jira/build-candidate-list-from-pull-requests (pull-requests)
  "Take `PULL-REQUESTS' as returned by jira/fetch-pull-requests and build a suitable candidate list for helm with it."
  (mapcar
   (lambda (pr)
     (let* ((title (alist-get 'title pr)))
       `(,(format "%s" title) . ,pr)))
   pull-requests))

(defun jira/browse-issue ()
  "Fetch a list of issues from JIRA and prompt for selection of one to open in the browser."
  (interactive)
  (jira/fetch-issues
   (lambda (issues)
     (let* ((helm-source
             (helm-build-sync-source "jira-issues-source"
               :candidates (jira/build-candidate-list-from-issues issues)
               :action (helm-make-actions "Open in browser" #'jira/helm-action-open-issue-in-browser))))
       (helm :sources helm-source)))))

(defun jira/browse-pull-request ()
  "Fetch a list of pull-requests from Bitbucket and prompt for selection of one to open in the browser."
  (interactive)
  (jira/fetch-pull-requests
   (lambda (pull-requests)
     (let* ((helm-source
             (helm-build-sync-source "jira-pull-requests-source"
               :candidates (jira/build-candidate-list-from-pull-requests pull-requests)
               :action (helm-make-actions "Open in browser" #'jira/helm-action-open-pull-request-in-browser))))
       (helm :sources helm-source)))))

(defun jira/magit-checkout-pull-request ()
  "Fetch a list of pull-requests from Bitbucket and prompt for selection of one to open in the browser."
  (interactive)
  (jira/fetch-pull-requests
   (lambda (pull-requests)
     (let* ((helm-source
             (helm-build-sync-source "jira-pull-requests-source"
               :candidates (jira/build-candidate-list-from-pull-requests pull-requests)
               :action (helm-make-actions "Check-out" #'jira/helm-action-checkout-pull-request))))
       (helm :sources helm-source)))))


(defun jira/helm-action-open-issue-in-browser (issue)
  "Open the given `ISSUE' in the browser."
  (let ((key (alist-get 'key issue)))
    (browse-url (format "%s/browse/%s" jira/url key))))

(defun jira/helm-action-open-pull-request-in-browser (pull-request)
  "Open the given `PULL-REQUEST' in the browser."
  (let* ((links (alist-get 'links pull-request))
         (self (elt (alist-get 'self links) 0))
         (href (alist-get 'href self)))
    (browse-url href)))

(defun jira/helm-action-checkout-pull-request (pull-request)
  "Check-out the given `PULL-REQUEST' using magit (branch has to already exist currently)."
  (let* ((from-ref (alist-get 'fromRef pull-request))
         (display-id (alist-get 'displayId from-ref)))
    (magit-checkout display-id)))

(provide 'my-jira)
;;; my-jira.el ends here