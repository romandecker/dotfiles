;;; package --- Utility functions, variables & constants
;;; Commentary:
;;; Code:
(defconst my/dotfile "~/.emacs.d/init.el")
(defconst my/electric-pairs '(("(" . ")") ("[" . "]") ("{" . "}")))
(defconst my/workgroups-file (expand-file-name "~/.emacs.d/workgroups"))


(defun my/reload-dotfile ()
  "Reload '~/.emacs.d/init.el'."
  (interactive)
  (load-file my/dotfile))

(defun my/open-dotfile ()
  "Open '~/.emacs.d/init.el'."
  (interactive)
  (find-file my/dotfile))

(defun my/get-pair ()
  "Get the according pair around the point or return nil if point is not inside an adjacent pair."
  (let ((preceding (string (preceding-char)))
	(following (string (following-char))))
    (let ((match (cdr (assoc preceding my/electric-pairs))))
      (if (equal following match)
	  match
	nil))))

(defun my/smart-space ()
  "Insert a space at point, and if inside and adjacent pair, also insert another space to keep whitespace balanced."
  (interactive) (when (my/get-pair)
		  (insert " ")
		  (backward-char))
  (insert " "))

(defun my/smart-delete ()
  "Delete a character. If inside an adjacent pair, also delete the according closing character.
If inside a pair with spaces, e.g. `( | )` delete both spaces symmetrically''"
  (interactive)
  (let ((preceding (string (preceding-char)))
	(following (string (following-char))))
    (if (and (equal preceding " ") (equal following " "))
	(let ((before (string (char-before (- (point) 1))))
	      (after (string (char-after (+ (point) 1)))))
	  (let ((match (cdr (assoc before my/electric-pairs))))
	    (if (equal after match)
		(progn
					; between spaces and brackets -> delete both spaces first
		  (delete-backward-char 1)
		  (delete-char 1))
					; between spaces, but not between brackets -> normal delete
	      (delete-backward-char 1))))
      ;; we're not even between spaces, perform "normal" delete, optionally deleting a pair
      (if (my/get-pair)
	  (electric-pair-delete-pair 1)
	(delete-backward-char 1)))))


(defun my/dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternative-file (leave no orphan buffer)"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

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

(defun my/tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas-minor-mode)
	    (null (my/do-yas-expand)))
	(if (my/check-expansion)
	    (progn
	      (company-manual-begin)
	      (if (null company-candidates)
		  (progn
		    (company-abort)
		    (indent-for-tab-command)))))))))

(defun my/tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas-minor-mode)
	  (null (my/do-yas-expand)))
      (if company-candidates
	  (company-complete-selection)
	(if (my/check-expansion)
	    (progn
	      (company-manual-begin)
	      (if (null company-candidates)
		  (progn
		    (company-abort)
		    (yas-next-field))))
	  (yas-next-field)))))

(defun my/expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode)
	  (null (my/do-yas-expand))
	  (company-abort))
      (company-complete-selection)))

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
(defvar my/useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers that are useful despite matching `my/useless-buffers-regexp'.")

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
  "Better C-n in normal mode. Will select the current word if not already using multiple cursors. Else,
keep adding cursors in multiple cursor mode."
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
    (read-file-name "Create file: "
		    (dired-current-directory))))
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

(defun my/switch-to-messages()
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun my/ctrlp-dwim ()
  (interactive)
  (if (evil-mc-has-cursors-p)
      (progn 
	(message "Fake cursors exist!")
	(evil-mc-make-and-goto-prev-match))
    (helm-projectile-find-file)))


(provide 'my-utils)
;;; my-utils.el ends here
