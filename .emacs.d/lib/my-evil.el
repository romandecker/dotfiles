(use-package evil
  :ensure t
  :config
  (define-key evil-insert-state-map [tab] 'my/tab-indent-or-complete)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)
  (define-key evil-insert-state-map (kbd "TAB") 'my/tab-indent-or-complete)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

  ;; camel-case-motion
  (define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "e") 'evil-forward-little-word-end)
  (define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "e") 'evil-forward-little-word-end)
  (define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "e") 'evil-forward-little-word-end)
  (define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map
    (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map
    (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Take care to keep evil-next-line here, or else `dk`, `dj`, etc.. won't work
  (define-key evil-motion-state-map
    (kbd "<remap> <evil-next-line>") 'evil-next-line)
  (define-key evil-motion-state-map
    (kbd "<remap> <evil-previous-line>") 'evil-previous-line)

  ;; smart go-to-bol (toggle between true BOL and first significant character)
  (define-key evil-normal-state-map (kbd "0") 'my/goto-bol-dwim)

  ;; quicker digraphs for german (just like it works in default mac apps)
  (define-key evil-insert-state-map (kbd "M-u") 'my/quick-digraph)
  (define-key evil-insert-state-map (kbd "M-s") 'my/insert-sharp-s)

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  (define-key evil-normal-state-map (kbd "m") 'my/evil-move)

  (setq evil-insert-state-cursor '((bar . 3) "red")
        evil-normal-state-cursor '(box "black"))

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)

  (define-key evil-normal-state-map (kbd "] p") 'evil-paste-pop)
  (define-key evil-normal-state-map (kbd "[ p") 'evil-paste-pop-next)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))
  (use-package evil-args
    :ensure t
    :config
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))
  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))
  (use-package evil-mc
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-n") 'my/smart-c-n)
    (define-key evil-normal-state-map (kbd "C-p") 'my/ctrlp-dwim)
    (define-key evil-visual-state-map (kbd "i") 'my/place-cursors-along-region)
    (evil-define-key 'normal evil-mc-key-map
      (kbd "C-n") 'my/smart-c-n
      (kbd "C-p") 'my/ctrlp-dwim
      [escape] 'evil-mc-undo-all-cursors
      (kbd "C-s") 'evil-mc-skip-and-goto-next-match
      (kbd "C-S-p") 'evil-mc-skip-and-goto-prev-cursor)
    (global-evil-mc-mode 1)
    (add-to-list 'evil-mc-incompatible-minor-modes 'delim-pad-mode))
  (use-package evil-exchange
    :ensure t
    :config
    (setq evil-previous-key (kbd "g x"))
    (evil-exchange-install))
  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))
  (use-package evil-quickscope
    :ensure t
    :config
    (global-evil-quickscope-mode 1))
  (require 'evil-little-word)
  (require 'my-bindings)
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

  (require 'delim-pad)
  (delim-pad-mode 1)
  (add-hook 'help-mode-hook (lambda () (delim-pad-mode -1)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (advice-add 'evil-delete :around 'my/evil-delete)

  (evil-mode 1)) ; evil-leader must be enabled before evil

;;; Make ESC quit most things
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; d deletes to black-hole, m ("move" deletes and yanks)
(defun my/evil-delete (orig-fn beg end &optional type register &rest args)
  (apply orig-fn beg end type ?_ args))

;; basically, this is the original, unadvised evil-delete operator
(evil-define-operator my/evil-move (beg end type register yank-handler)
  "Delete and yank text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank))
  )

(defun my/quick-digraph-read-key ( &optional prompt )
  "To be used as an advice overriding `read-key`. Always returns the
`:` key.  Will de-register itself, causing the next call to `read-key`
to reach the original implementation again."
  (advice-remove 'read-key 'my/quick-digraph-read-key) ; un-advice self
  ?:)                                                  ; return `:`

(defun my/quick-digraph ()
  "Enters a pending digraph-state, where only a couple of characters can be
pressed which will result in german Umlauts. This is done by advising `read-key`
which is called by `evil-insert-digraph` to act as though the user has navigated
to the appropriate digraph-group by pressing `:`."
  (interactive)
  (advice-add 'read-key :override 'my/quick-digraph-read-key)
  (evil-insert-digraph 1))

(defun my/insert-sharp-s ()
  "Quick helper to insert the `ß` character into the buffer at point."
  (interactive)
  (insert "ß"))


(defun my/place-cursors-along-region (start end)
  "Place cursors along the given region given by START and END (or
interactively along the current region)."
  (interactive "r")
  (let ((col (my/column-at start))
        (last (line-number-at-pos end)))
    (evil-normal-state)
    (evil-mc-pause-cursors)
    (goto-char start)
    (save-excursion
      (while (< (line-number-at-pos (point)) last)
          (evil-next-visual-line)
          (evil-mc-make-cursor-here)
          (move-to-column col)))
    (evil-mc-resume-cursors)))

(provide 'my-evil)
