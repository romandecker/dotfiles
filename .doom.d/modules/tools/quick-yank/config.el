;;; tools/quick-yank/config.el -*- lexical-binding: t; -*-

(defun quick-yank/yank-to-clipboard (str)
  "Put `STR` into the defaut register and log a message saying what was yanked."
  (evil-set-register ?\" str)
  (message "Yanked: %s" str))

(defun quick-yank/yank-filename ()
  (interactive)
  (quick-yank/yank-to-clipboard (buffer-file-name)))

(defun quick-yank/yank-iso8601-timestamp ()
  (interactive)
  (quick-yank/yank-to-clipboard
    (format-time-string "%Y-%m-%dT%T.%3NZ" nil t)))

(defun quick-yank/yank-package-filename ()
    (interactive)
    (let* ((nearest-package-json (quick-yank/search-file-upward default-directory "package.json"))
           (package-directory (file-name-directory nearest-package-json))
           (path (file-relative-name (buffer-file-name) package-directory)))
      (quick-yank/yank-to-clipboard path)))


(map! :leader "y P" #'quick-yank/yank-package-filename)

(defun quick-yank/search-file-upward (directory file)
  "Search DIRECTORY for FILE and return its full path if found, or NIL if not.
If FILE is not found in DIRECTORY, the parent of DIRECTORY will be searched."
  (let ((parent-dir (file-truename (concat (file-name-directory directory) "../")))
        (current-path (if (not (string= (substring directory (- (length directory) 1)) "/"))
                          (concat directory "/" file)
                        (concat directory file))))
    (if (file-exists-p current-path)
        current-path
      (when (and (not (string= (file-truename directory) parent-dir))
                 (< (length parent-dir) (length (file-truename directory))))
        (quick-yank/search-file-upward parent-dir file)))))

(after! magit

  (map! :leader "y g g" #'quick-yank/magit-yank-head-info)
  (map! :leader "y g b" #'quick-yank/magit-yank-branch)
  (map! :leader "y g h" #'quick-yank/magit-yank-head-sha)

  (defun quick-yank/magit-yank-head-info ()
    "Yank the current commit HEAD info to the default register."
    (interactive)
    (quick-yank/yank-to-clipboard (magit-git-string "log" "--format=%h %s")))

  (defun quick-yank/magit-yank-head-sha ()
    "Yank the current commit HEAD sha to the default register."
    (interactive)
    (quick-yank/yank-to-clipboard (magit-git-string "rev-parse" "HEAD")))

  (defun quick-yank/magit-yank-branch ()
    "Yank the current branch name to the default register."
    (interactive)
    (quick-yank/yank-to-clipboard (magit-git-string "rev-parse" "--abbrev-ref" "HEAD"))))


(after! projectile

  (map! :leader "y p f" #'quick-yank/yank-project-filename)

  (defun quick-yank/yank-project-filename ()
    (interactive)
    (quick-yank/yank-to-clipboard
     (file-relative-name (buffer-file-name) (projectile-project-root)))))
