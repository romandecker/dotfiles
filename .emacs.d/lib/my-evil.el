(use-package evil
  :ensure t
  :demand t

  :general
  (:keymaps 'insert
   [tab] 'my/tab-indent-or-complete
   "TAB" 'my/tab-indent-or-complete
   "C-l" 'evil-delete-char)
  (:keymaps '(normal visual operator)
   "C-u" 'evil-scroll-up
   "w"   'evil-forward-little-word-begin
   "e"   'evil-forward-little-word-end
   "b"   'evil-backward-little-word-begin)
  (:keymaps '(normal visual)
   ;; smart go-to-bol (toggle between true BOL and first significant character)
   "0"   'my/goto-bol-dwim)
  (:keymaps 'insert
   ;; quicker digraphs for german (just like it works in default mac apps)
   "M-u" 'my/quick-digraph
   "M-s" 'my/insert-sharp-s
   )
  (:keymaps 'normal
   "] p" 'evil-paste-pop
   "[ p" 'evil-paste-pop-next)
  :config
  (message "configuring evil")
  (add-to-list 'evil-insert-state-modes 'calculator-mode)
  (add-to-list 'evil-normal-state-modes 'package-menu-mode)

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
  
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  (setq evil-insert-state-cursor '((bar . 3) "red")
        evil-normal-state-cursor '(box "black"))
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-numbers
    :ensure t
    :general
    (:keymaps 'normal
     "C-a" 'evil-numbers/inc-at-pt
     "C-x" 'evil-numbers/dec-at-pt))
  (use-package evil-args
    :ensure t
    :general
    (:keymaps '(evil-inner-text-objects-map) "a" 'evil-inner-arg)
    (:keymaps '(evil-outer-text-objects-map) "a" 'evil-outer-arg))
  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))
  (message "configuring evil-mc")
  (use-package evil-mc
    :ensure t
    :demand t
    :general
    (:keymaps 'normal
     "C-n" 'my/smart-c-n
     "C-p" 'my/ctrlp-dwim)
    (:keymaps 'visual
     "|" 'my/place-cursors-along-region)
    (:states 'normal
     :keymaps 'evil-mc-key-map
     "C-n"    'my/smart-c-n
     "C-p"    'my/ctrlp-dwim
     [escape] 'evil-mc-undo-all-cursors
     "C-s"    'evil-mc-skip-and-goto-next-match
     "C-S-p"  'evil-mc-skip-and-goto-prev-cursor)
    :config
    (message "enabling global evil-mc-mode")
    (global-evil-mc-mode 1)
    (add-to-list 'evil-mc-incompatible-minor-modes 'delim-pad-mode))
  (use-package evil-exchange
    :ensure t
    :config
    (evil-exchange-install)

    ;; TODO: sadly, this breaks evil-mc, figure out a workaround
    ;; (general-define-key :keymaps 'normal "c"
    ;;   (general-key-dispatch 'evil-change
    ;;     "s" 'evil-surround-change
    ;;     "x" 'evil-exchange
    ;;     "X" 'evil-exchange-cancel)))
    )
  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))
  (use-package evil-quickscope
    :ensure t
    :config
    (global-evil-quickscope-mode 1))
  (require 'evil-little-word)

  (require 'delim-pad)
  (add-hook 'prog-mode-hook (lambda () (delim-pad-mode 1)))
  (add-hook 'help-mode-hook (lambda () (delim-pad-mode -1)))

  ;; make Esc quit most things
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (advice-add 'evil-delete :around 'my/evil-delete)

  (define-key evil-normal-state-map (kbd "m") 'my/evil-move)

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
