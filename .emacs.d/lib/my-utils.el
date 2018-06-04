;;; package --- Utility functions, variables & constants
;;; Commentary:
;;; Code:


(defun my/reload-dotfile ()
  "Reload '~/.emacs.d/init.el'."
  (interactive)
  (load-file my/dotfile))

(defun my/open-dotfile ()
  "Open '~/.emacs.d/init.el'."
  (interactive)
  (find-file my/dotfile))

(defun my/check-expansion ()
  "checks wether or not expansion should be done"
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
  (backward-char 1)
  (if (looking-at "->") t nil)))))

(defun my/do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun my/at-completion-marker ()
  (or
   (save-excursion (backward-char 1) (looking-at "\\."))
   (save-excursion (backward-char 2) (looking-at "->"))
   ))

(defun my/try-expand-snippet ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun my/tab-dwim ()
  (interactive)
  (cond
   ;; when in minibuffer, perform minibuffer-completion
   ((minibufferp)
    (minibuffer-complete))

   ;; we're currently expanding a snippet
   ((my/is-snippet-active)       
    (cond
     ((my/try-expand-snippet))     ; try to expand an inner snippet
     ((company-tooltip-visible-p)   ; if completion is active, do completion
      (company-complete))
     (t                             ; else, move to the next field
      (yas-next-field))))

   ;; a completion has already started and there were changes to the selection
   ((and company-mode (company-tooltip-visible-p) (not (null company-candidates)) company-selection-changed)
    (company-complete))

   ;; a snippet can potentially be expanded, try it
   ((and (yas-minor-mode) (my/do-yas-expand)))

   ;; try completion
   ((and company-mode (my/at-completion-marker))
    (when (not (my/try-completion))
      (indent-for-tab-command)))

   ((and company-mode
         (or (company-tooltip-visible-p)
             (eq (safe-length company-candidates) 1))
         (company-complete)))

   (t
    (insert-tab))))

(defun my/try-completion ()
  (company-manual-begin)
  (if (null company-candidates)
      (progn (company-abort)
             nil)
    t))

(defun my/is-snippet-active ()
  (and
   (yas-minor-mode)
   (not (null (car (yas-active-snippets))))))

(defun my/abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(defun my/open-snippet-dir ()
  (interactive)
  (let* ((dir (file-name-as-directory (car yas-snippet-dirs)))
   (path (concat dir (symbol-name major-mode))))
    (dired path)))

(defun my/resize-window-down ()
  "Resize a window downwards."
  (interactive)
  (if (window-in-direction 'below)
      (enlarge-window 1)
    (shrink-window 1)))

(defun my/resize-window-up ()
  "Resize a window upwards."
  (interactive)
  (if (window-in-direction 'above)
      (enlarge-window 1)
    (shrink-window 1)))

(defun my/resize-window-left ()
  "Resize a window leftwards."
  (interactive)
  (if (window-in-direction 'left)
      (enlarge-window-horizontally 1)
    (shrink-window-horizontally 1)))

(defun my/resize-window-right ()
  "Resize a window rightwards."
  (interactive)
  (if (window-in-direction 'right)
      (enlarge-window-horizontally 1)
    (shrink-window-horizontally 1)))

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar my/useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar my/useful-buffers-regexp '()
  "Regexp used to define buffers that are useful despite matching
  `my/useless-buffers-regexp'.")

(defun my/useful-buffer-p (buffer)
  "Determines whether or not the given BUFFER is useful."
  (let ((buf-name (buffer-name buffer)))
    (or (with-current-buffer buffer
    (derived-mode-p 'comint-mode))
  (cl-loop for useful-regexp in my/useful-buffers-regexp
     thereis (string-match-p useful-regexp buf-name))
  (cl-loop for useless-regexp in my/useless-buffers-regexp
     never (string-match-p useless-regexp buf-name)))))

(let ((buf-pred-entry (assq 'buffer-predicate default-frame-alist)))
  (if buf-pred-entry
      ;; `buffer-predicate' entry exists, modify it
      (setcdr buf-pred-entry #'my/useful-buffer-p)
    ;; `buffer-predicate' entry doesn't exist, create it
    (push '(buffer-predicate . my/useful-buffer-p) default-frame-alist)))

(defun my/smart-c-n ()
  "Better C-n in normal mode. Will select the current word if not
already using multiple cursors. Else, keep adding cursors in multiple
cursor mode."
  (interactive)
  (if (evil-mc-has-cursors-p)
      (evil-mc-make-and-goto-next-match)
    (my/mark-current-word)))

(defun my/mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
     (or (and (eq last-command this-command) (mark t))
         (region-active-p)))
      (set-mark
       (save-excursion
   (when (< (mark) (point))
     (setq arg (- arg)))
   (goto-char (mark))
   (forward-word arg)
   (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
  (error "No word at point"))
      (if (>= arg 0)
    (goto-char (car wbounds))
  (goto-char (cdr wbounds)))
      (push-mark (save-excursion
       (forward-word arg)
       (point)))
      (activate-mark))))

(defun my/dired-create-file (file)
  (interactive
   (list
    (read-file-name "Create file: " (dired-current-directory))))
  (write-region "" nil (expand-file-name file) t)
  (dired-add-file file)
  (revert-buffer)
  (dired-goto-file (expand-file-name file)))

(defun my/switch-to-last-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/window-move (action)
  "Check whether the current frame is zoomed, and if yes, unzoom before performing the given ACTION."
  (interactive)
  (when (frame-parameter nil 'zoom-window-enabled) (zoom-window-zoom))
  (funcall action 1))

(defun my/window-down ()
  "Zoom-sensitive window-down move."
  (interactive)
  (my/window-move 'evil-window-down))

(defun my/window-up ()
  "Zoom-sensitive window-up move."
  (interactive)
  (my/window-move 'evil-window-up))

(defun my/window-left ()
  "Zoom-sensitive window-left move."
  (interactive)
  (my/window-move 'evil-window-left))

(defun my/window-right ()
  "Zoom-sensitive window-right move."
  (interactive)
  (my/window-move 'evil-window-right))

(defun my/switch-to-messages ()
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun my/switch-to-compilation ()
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun my/ctrlp-dwim ()
  (interactive)
  (if (and (boundp 'evil-mc-has-cursors-p)
           (evil-mc-has-cursors-p))
      (progn
  (evil-mc-make-and-goto-prev-match))
    (helm-projectile-find-file)))


(defun my/goto-bol-dwim ()
  "Move point to `beginning-of-line`.
If repeated, cycle position between `back-to-indentation` and `beginning of line`"
  (interactive "^")
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun my/reload-dir-locals ()
  "Reload all directory-local-variables. Also reloads all snippets."
  (interactive)
  (hack-dir-local-variables)
  (hack-local-variables-apply)
  (yas-reload-all))

(defun my/column-at (pos)
  "Get the column corresponding to the given buffer position POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun my/open-current-file-with (&optional ask)
  "Open the current file or dired marked files in external app.  The
app is chosen from your OS's preference.  URL
`http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (-fpath)
           (w32-shell-execute "open"
                              (replace-regexp-in-string "/" "\\" -fpath t t)))
         -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (-fpath)
           (shell-command
            (concat "open " (shell-quote-argument -fpath))))
         -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (-fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" -fpath)))
         -file-list))))))

(defun my/open-current-dir-in-explorer ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ➢ for example: with nautilus
    )))

(defun my/download-file (url target)
  "Download a file from an url to a local file. If point is on an url,
do not prompt for the url and take that url instead.  If prompt is on
a file name, do not prompt for a file name and take that instead.
Return the path that the file has been downloaded to."
  (interactive
   (let* ((url (or (thing-at-point 'url) (read-string "Url: ")))
          (path (read-file-name
                 "Download to:"
                 default-directory
                 (when url (file-name-nondirectory url)))))
     (list url path)))
  (url-copy-file url target 1)
  target)

(defun my/relpath-from-current (path)
  "Calculate the relative path to the given PATH from the current file."
  (let ((relpath (file-relative-name path default-directory)))
    (if (string-match "^\\.\\./" relpath)
        relpath
      (concat "./" relpath))))

(defun my/download-file-and-insert-path ()
  "Use `my/download-file' to download a file and insert its local path
into the current buffer."
  (interactive)
  (let* ((path (call-interactively 'my/download-file))
         (relpath (file-relative-name path default-directory))
         (relpath (if (string-match "^\\.\\./" relpath)
                      relpath
                    (concat "./" relpath))))
  (when (thing-at-point 'url)
    (evil-open-below 1))
  (insert (my/relpath-from-current path))))

(my/define-leader-map
 "i d" 'my/download-file-and-insert-path)

(defun my/generic-slurp-right ()
  (interactive)
  (my/generic-slurp-barf-right 'forward-word))

(defun my/generic-barf-right ()
  (interactive)
  (my/generic-slurp-barf-right 'backward-word))

(defun my/generic-slurp-left ()
  (interactive)
  (my/generic-slurp-barf-left 'backward-word))

(defun my/generic-barf-left ()
  (interactive)
  (my/generic-slurp-barf-left 'forward-word))

(defun my/generic-slurp-barf (regex-fun regexp use-char-fn delete-char-fn move-word-fn)
  (save-excursion
    (when (funcall regex-fun regexp)
      (let ((ch (funcall use-char-fn)))
        (funcall delete-char-fn 1)
        (funcall move-word-fn)
        (insert (char-to-string ch))))))

(defun my/generic-slurp-barf-right (move-word-fn)
  (my/generic-slurp-barf 're-search-forward
                         "[\]\)\}\"\']"
                         'char-before
                         'delete-backward-char
                         move-word-fn))

(defun my/generic-slurp-barf-left (move-word-fn)
  (my/generic-slurp-barf 're-search-backward
                         "[\[\(\{\"\']"
                         'char-after
                         'delete-char
                         move-word-fn))

(defun my/evil-select-pasted ()
  "Mark the last pasted text."
  (interactive)
  (let ((start-marker (evil-get-marker (string-to-char "[")))
        (end-marker (evil-get-marker (string-to-char "]"))))
    (evil-visual-select start-marker end-marker)))

(defun my/indent-last-paste ()
  "Re-indent the last pasted text."
  (interactive)
  (let ((start-marker (evil-get-marker (string-to-char "[")))
        (end-marker (evil-get-marker (string-to-char "]"))))
    (evil-indent start-marker end-marker)))

(defun my/in-c-block-comment-p ()
  "Return `t' if point is currently inside a c-style block-comment."
  (when (my/in-c-block-comment-prefix)
    t))
  

(defun my/in-c-block-comment-prefix ()
  "Return the block comment prefix."
  (save-excursion
    (save-match-data
      (when (nth 4 (syntax-ppss))
        (move-beginning-of-line nil)
        (when (looking-at " *\\* *")
          (match-string-no-properties 0))))))

(defun my/swap-chars ()
  "Swap the two characters before point."
  (interactive)
  (save-excursion
    (evil-forward-char)
    (transpose-chars nil)))

(defun my/insert-random-uuid ()
  (interactive)
  (let ((uuid (shell-command-to-string "uuidgen")))
    (insert (s-trim-right (downcase uuid)))))

(defun my/yank-filename ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (evil-set-register ?\" file-name)
    (message "Yanked \"%s\"" file-name)))
                     

(defvar my/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")

(defun my/enable-minor-mode-based-on-extension ()
  "check file name against my/auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in my/auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist my/auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'my/enable-minor-mode-based-on-extension)


(provide 'my-utils)
;;; my-utils.el ends here
