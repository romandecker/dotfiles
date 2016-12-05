;;; package --- My custom magit config
;;; Commentary:
;;; Code:

;; disable "VC" (emacs internal version control stuff)
(setq vc-handled-backends nil)

(use-package evil-magit
  :ensure t
  :defer 5
  :config
  (message "evil-magit has loaded!"))

(use-package git-timemachine :ensure t
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
  (set-face-foreground 'git-gutter+-added "#22be22")
  (set-face-foreground 'git-gutter+-modified "#fb9323")
  (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
  (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
  (add-hook 'prog-mode-hook (lambda () (interactive) (git-gutter+-mode 1)))
  (evil-leader/set-key
    "g h s" 'git-gutter+-stage-hunks
    "g h r" 'git-gutter+-revert-hunk
    "g h _" 'git-gutter+-revert-hunk
    "g h h" 'git-gutter+-show-hunk-inline-at-point
    "g h H" 'git-gutter+-show-hunk)
  (which-key-add-key-based-replacements
    "SPC g h"   "Hunks"))


(define-key evil-normal-state-map (kbd "] c") 'smerge-next)
(define-key evil-normal-state-map (kbd "[ c") 'smerge-prev)

(fringe-helper-define 'git-gutter-fr+-modified nil
  "..xxx..."
  "...xxxx."
  ".....xx."
  "...xxxx."
  ".xxxx..."
  ".xx....."
  ".xxxx..."
  "...xxx..")

(defun my-git/start-time-machine ()
  (interactive)
  (git-timemachine)
  (hydra-git-timemachine/body))

(use-package emojify
  :ensure t
  :config
  (global-emojify-mode)
  )

(defun my/insert-gitmoji ()
  "Interactively prompt for Emojis and insert them in the current buffer.

This respects the `emojify-emoji-styles' variable."
  (interactive)
 (emojify-create-emojify-emojis)
  (let* ((emojify-in-insertion-command-p t)
         (styles (mapcar #'symbol-name emojify-emoji-styles))
         (line-spacing 7)
         (completion-ignore-case t)
         (candidates '(":art: - Improving structure / format of the code."
                       ":zap: - Improving performance."
                       ":fire: - Removing code or files."
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
                       ))
         ;; Vanilla Emacs completion and Icicles use the completion list mode to display candidates
         ;; the following makes sure emojify is enabled in the completion list
         (completion-list-mode-hook (cons #'emojify--insert-minibuffer-setup-hook completion-list-mode-hook))
         ;; (Vertical) Ido and Ivy displays candidates in minibuffer this makes sure candidates are emojified
         ;; when Ido or Ivy are used
         (minibuffer-setup-hook (cons #'emojify--insert-minibuffer-setup-hook minibuffer-setup-hook))
         (helm-after-initialize-hook (cons #'emojify--insert-helm-hook (bound-and-true-p helm-after-initialize-hook))))
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

(evil-leader/set-key
  "i e" 'emojify-insert-emoji
  "i g" 'my/insert-gitmoji)

(add-hook 'git-commit-mode-hook #'my/autoinsert-gitmoji)

(provide 'my-magit)
;;; my-magit.el ends here
