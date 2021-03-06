;;; package --- My custom magit config ;;; Commentary:
;;; Code:

;; disable "VC" (emacs internal version control stuff)
(setq vc-handled-backends nil)

;; make sure magit doesn't override global leader
(general-emacs-define-key
    '(magit-status-mode-map
      magit-diff-mode-map
      magit-log-mode-map)
  my/leader nil)

(general-define-key :prefix my/local-leader
                    :keymaps '(smerge-basic-map smerge-mode-map)
  "m m" 'smerge-keep-mine
  "m a" 'smerge-keep-all
  "m b" 'smerge-keep-base
  "m c" 'smerge-keep-current
  "m o" 'smerge-keep-other)

(which-key-add-key-based-replacements
  (concat my/local-leader " m") "Merging")

(use-package evil-magit
  :ensure t
  :demand t
  :after magit
  :general
  (:keymaps '(magit-mode-map magit-status-mode-map magit-diff-mode-map)
   :states 'normal
   "C-j" 'my/window-down
   "C-k" 'my/window-up
   )
  :config)

(use-package gitlab
  :ensure t
  :config
  (use-package git-commit-insert-issue
    :ensure t
    :general
    (:prefix my/leader
     :keymaps 'normal
     "i i" 'git-commit-insert-issue-gitlab-insert)
    :config
    (add-hook 'git-commit-mode-hook #'git-commit-insert-issue-mode)))

(use-package git-timemachine
  :ensure t
  :demand t
  :after hydra
  :general
  (:prefix my/leader
   :keymaps 'normal
   "g g"   'my-git/start-time-machine)
  :config
  (defhydra hydra-git-timemachine ()
    "git-timemachine"
    ("j" git-timemachine-show-next-revision "Next commit")
    ("k" git-timemachine-show-previous-revision "Previous commit")
    ("q" git-timemachine-quit "Quit timemachine"))
  (defun my-git/start-time-machine ()
    (interactive)
    (git-timemachine)
    (hydra-git-timemachine/body)))

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
  :demand t
  :general
  (:prefix my/leader
   :keymaps 'normal
   "g h s" 'git-gutter+-stage-hunks
   "g h r" 'git-gutter+-revert-hunk
   "g h _" 'git-gutter+-revert-hunk
   "g h h" 'git-gutter+-show-hunk-inline-at-point
   "g h H" 'git-gutter+-show-hunk)
  (:keymaps 'normal
   "] h" 'git-gutter+-next-hunk
   "[ h" 'git-gutter+-previous-hunk)
  :config
  (setq git-gutter-fr+-side 'right-fringe)
  (set-face-foreground 'git-gutter+-added "#22be22")
  (set-face-foreground 'git-gutter+-modified "#fb9323")
  (add-hook 'prog-mode-hook (lambda () (git-gutter+-mode 1)))

  (which-key-add-key-based-replacements
    (concat my/leader " g h") "Hunks")

  (fringe-helper-define 'git-gutter-fr+-modified nil
    "..xxx..."
    "...xxxx."
    ".....xx."
    "...xxxx."
    ".xxxx..."
    ".xx....."
    ".xxxx..."
    "...xxx..")

  (defun my/toggle-git-gutter ()
    "Handler for toggling git gutter"
    (interactive)
    (git-gutter+-toggle-fringe)))

(general-define-key
 :prefix my/leader
 :keymaps 'normal
  "g a"   'magit-stage-file
  "g b"   'magit-commit
  "g c"   'magit-commit
  "g d"   'magit-diff-buffer-file-popup
  "g l"   'magit-log-buffer-file-popup
  "g s"   'magit-status
  "g u"   'magit-unstage-file
  "g h s" 'git-gutter+-stage-hunks
  "g h r" 'git-gutter+-revert-hunk
  "g h h" 'git-gutter+-show-hunk-inline-at-point)

(general-define-key
 :keymaps 'normal
  "] c" 'smerge-next
  "[ c" 'smerge-prev)

(use-package emojify
  :ensure t
  :demand t
  :general
  (:prefix my/leader
   :keymaps 'normal
   "i e" 'emojify-insert-emoji
   "i g" 'my/insert-gitmoji
   "t e" 'emojify-mode)
  :init
  ;; for some reason, emojify doesn't have the :memo: emoji registered
  ;; correctly, so add it here as a user-emoji
  (setq emojify-user-emojis '((":memo:" . (("name" . "Memo")
                                           ("image" . "~/.emacs.d/emojis/emojione-v2.2.6-22/1f4dd.png")
                                           ("style" . "github")))))
  :config
  (global-emojify-mode))


(defun my/insert-gitmoji ()
  "Interactively prompt for Emojis and insert them in the current buffer.

This respects the `emojify-emoji-styles' variable."
  (interactive)
  (emojify-create-emojify-emojis)
  (let* ((emojify-minibuffer-reading-emojis-p t)
         (line-spacing 7)
         (completion-ignore-case t)
         (candidates '(":art: - Improving structure / format of the code."
                       ":zap: - Improving performance."
                       ":fire: - Removing code or files."
                       ":truck: - Moving or renaming files."
                       ":bug: - Fixing a bug."
                       ":ambulance: - Critical hotfix."
                       ":sparkles: - Introducing new features."
                       ":memo: - Writing docs."
                       ":rocket: - Deploying stuff."
                       ":lipstick: - Updating the UI and style files."
                       ":tada: - Initial commit."
                       ":white-check-mark: - Adding tests."
                       ":lock: - Fixing security issues."
                       ":apple: - Fixing something on macOS."
                       ":penguin: - Fixing something on Linux."
                       ":checkered-flag: - Fixing something on Windows."
                       ":bookmark: - Releasing / Version tags."
                       ":rotating_light: - Removing linter warnings."
                       ":construction: - Work in progress."
                       ":green_heart: - Fixing CI Build."
                       ":arrow_down: - Downgrading dependencies."
                       ":arrow_up: - Upgrading dependencies."
                       ":construction_worker: - Adding CI build system."
                       ":chart_with_upwards_trend: - Adding analytics or tracking code."
                       ":hammer: - Heavy refactoring."
                       ":heavy_minus_sign: - Removing a dependency."
                       ":whale: - Work about Docker."
                       ":heavy_plus_sign: - Adding a dependency."
                       ":wrench: - Changing configuration files."
                       ":globe_with_meridians: - Internationalization and localization."
                       ":pencil: - Fixing typos."
                       ":hankey: - Writing bad code that needs to be improved."
                       ":twisted_rightwards_arrows: - Merge commit."
                       ":speech_balloon: - Updating text and literals."
                       ":wheelchair: - Improving accessibility."
                       ":ok_hand: - Updating code due to code review changes."
                       ":bento: - Adding or updating assets."
                       ":page_facing_up: - Adding or updating license."
                       ":boom: - Introducing breaking changes."
                       ":alien: - Updating code due to external API changes."
                       ":package: - Updating compiled files or packages."
                       ":rewind: - Reverting changes."
                       ":bookmark: - Releasing / Version tags."
                       ":keyboard: - Work on key bindings."
                       ))
         ;; Vanilla Emacs completion and Icicles use the completion list mode to display candidates
         ;; the following makes sure emojify is enabled in the completion list
         (completion-list-mode-hook (cons #'emojify--completing-read-minibuffer-setup-hook
                                          completion-list-mode-hook))
         ;; (Vertical) Ido and Ivy displays candidates in minibuffer this makes sure candidates are emojified
         ;; when Ido or Ivy are used
         (minibuffer-setup-hook (cons #'emojify--completing-read-minibuffer-setup-hook
                                      minibuffer-setup-hook))
         (helm-after-initialize-hook (cons #'emojify--completing-read-helm-hook
                                           (bound-and-true-p helm-after-initialize-hook))))
    (insert (car (split-string (completing-read "Insert Emoji: " candidates) " ")))))

(defcustom my/autoinsert-gitmoji
  nil
  "If set to t, will automatically prompt for a gitmoji insertion when committing.")

(defun my/autoinsert-gitmoji ()
  "Automatically prompt for a gitmoji when `my/autoinsert-gitmoji' is set to t and point is on an empty line.
Used for automatically inserting on magit-commit."
  ;; make sure dir-locals are respected
  (hack-dir-local-variables)
  (hack-local-variables-apply)
  (when (and my/autoinsert-gitmoji
             (looking-at "[[:space:]]*$"))
    (my/insert-gitmoji)))

(add-hook 'git-commit-mode-hook #'my/autoinsert-gitmoji)

(defun my/magit-yank-head-info ()
  "Yank the current commit HEAD info to the default register."
  (interactive)
  (my/yank-to-clipboard (magit-git-string "log" "--format=%h %s")))

(defun my/magit-yank-head-sha ()
  "Yank the current commit HEAD sha to the default register."
  (interactive)
  (my/yank-to-clipboard (magit-git-string "rev-parse" "HEAD")))

(defun my/magit-yank-branch ()
  "Yank the current branch name to the default register."
  (interactive)
  (my/yank-to-clipboard (magit-git-string "rev-parse" "--abbrev-ref" "HEAD")))

(defvar git-hosting-provider nil)
(defvar bitbucket-baseurl nil)
(defvar bitbucket-project nil)
(defvar bitbucket-repo nil)
(defvar github-baseurl "https://github.com")
(defvar github-user nil)
(defvar github-repo nil)

(defun my/magit-guess-provider ()
  "Guess the git hosting provider based on the configured origin url."
  (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
    (cond 
     ((string-match-p "github.com" origin-url) 'github)
     ((string-match-p "gitlab.com" origin-url) 'gitlab)
     (t (error
         "Cannot guess git hosting provider from configured origin url (%s), please set bitbucket-provider manually"
         origin-url)))))

(defun my/magit-guess-bitbucket-url ()
  "Guess the bitbucket base-url based on the configured origin url."
  (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
    (if (string-match "ssh://\\(:?.*?\\)@\\(.*?\\)/" origin-url)
        (format "https://%s" (match-string 2 origin-url))
      (error "Protocols other than ssh not yet supported"))))

(defun my/magit-guess-bitbucket-project ()
  "Guess the bitbucket project based on the configured origin url."
  (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
    (if (string-match "ssh://\\(.*?\\)@\\(.*\\)/\\(.*?\\)/" origin-url)
        (match-string 3 origin-url)
      (error "Unsupported origin config - Please set bitbucket-project manually"))))

(defun my/magit-guess-bitbucket-repo ()
  "Guess the bitbucket repo based on the configured origin url."
  (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
    (if (string-match "ssh://\\(.*?\\)@\\(.*\\)/\\(.*?\\)/\\(.*?\\)\\.git" origin-url)
        (match-string 4 origin-url)
      (error "Unsupported origin config - Please set bitbucket-repo manually"))))

(defun my/build-bitbucket-line-link (ref path line)
  "Build a bitbucket-web-link for the given ref, path and line"
  (let* ((baseurl (or bitbucket-baseurl (my/magit-guess-bitbucket-url)))
         (project (or bitbucket-project (my/magit-guess-bitbucket-project)))
         (repo (or bitbucket-repo (my/magit-guess-bitbucket-repo))))
    (format "%s/projects/%s/repos/%s/browse/%s?at=%s#%s" baseurl project repo path ref line)))


(defun my/magit-guess-github-user ()
  "Guess the github user based on the configured origin url."
  (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
    (if (string-match "git@github.com:\\(.*?\\)/" origin-url)
        (match-string 1 origin-url)
      (error "Unsupported origin config - Please set github-user manually"))))

(defun my/magit-guess-github-repo ()
  "Guess the github repo based on the configured origin url."
  (let ((origin-url (magit-git-string "config" "--get" "remote.origin.url")))
    (if (string-match "git@github.com:\\(.*?\\)/\\(.*?\\)\\.git$" origin-url)
        (match-string 2 origin-url)
      (error "Unsupported origin config - Please set github-repo manually"))))

(defun my/build-github-line-link (ref path line)
  "Build a github-web-link for the given ref, path an line"
  (let* ((user (or github-user (my/magit-guess-github-user)))
         (repo (or github-repo (my/magit-guess-github-repo))))
    (format "%s/%s/%s/blob/%s/%s#L%d" github-baseurl user repo ref path line)))

(defvar git-hosting-providers
  '((bitbucket . ((build-line-link . my/build-bitbucket-line-link)))
    (github    . ((build-line-link . my/build-github-line-link)))))

(defun my/magit-get-line-link ()
  (let* ((ref (magit-git-string "rev-parse" "--abbrev-ref" "HEAD"))
         (provider (or git-hosting-provider (my/magit-guess-provider)))
         (hosting-provider-config (alist-get provider git-hosting-providers))
         (build-line-link-fn (alist-get 'build-line-link hosting-provider-config))
         (path (file-relative-name (buffer-file-name) (projectile-project-root)))
         (line-number (line-number-at-pos (point))))
    (funcall build-line-link-fn ref path line-number)))

(defun my/magit-yank-line-link ()
  (interactive)
  (my/yank-to-clipboard (my/magit-get-line-link)))

(defun my/magit-open-current-line-in-browser ()
  (interactive)
  (browse-url (my/magit-get-line-link)))

(provide 'my-magit)
;;; my-magit.el ends here
