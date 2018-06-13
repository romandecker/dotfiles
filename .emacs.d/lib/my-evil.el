;; -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :demand t

  :general
  (:keymaps 'insert
   [tab] 'my/tab-dwim
   "TAB" 'my/tab-dwim
   "C-l" 'evil-delete-char

   ;; quicker digraphs for german (just like it works in default mac apps)
   "M-u" 'my/quick-digraph
   "M-s" 'my/insert-sharp-s

   "C-x C-l" 'hippie-expand)

  (:keymaps '(normal visual operator)
   "C-u" 'evil-scroll-up
   "w"   'evil-forward-little-word-begin
   "e"   'evil-forward-little-word-end
   "b"   'evil-backward-little-word-begin)
  (:keymaps 'normal
   ;; smart go-to-bol (toggle between true BOL and first significant character)
   "0"   'my/goto-bol-dwim)
  (:keymaps 'normal
   "] p" 'evil-paste-pop
   "[ p" 'evil-paste-pop-next
   "] P" 'helm-show-kill-ring
   "[ P" 'helm-show-kill-ring
   "g p" 'my/evil-select-pasted
   "C-o" 'goto-last-change           ; make c-i/c-o always stay in the
   "C-i" 'goto-last-change-reverse   ; current file
   "g ;" 'evil-jump-forward          ; make g ;/g , do what C-o/C-i did before
   "g ," 'evil-jump-backward
   "Q"   'my/edit-evil-macro
   "="   (general-key-dispatch 'evil-indent
           "p" 'my/formatted-paste-after
           "P" 'my/formatted-paste-before))
  :config
  (message "configuring evil")
  (add-to-list 'evil-insert-state-modes 'calculator-mode)
  (add-to-list 'evil-normal-state-modes 'package-menu-mode)
  (add-to-list 'evil-normal-state-modes 'debugger-mode)

  (evil-define-command my/formatted-paste-after (count &optional register yank-handler)
    :suppress-operator t
    (interactive "P<x>")
    (evil-paste-after count register yank-handler)
    (my/indent-last-paste))

  (evil-define-command my/formatted-paste-before (count &optional register yank-handler)
    :suppress-operator t
    (interactive "P<x>")
    (evil-paste-before count register yank-handler)
    (my/indent-last-paste))

  (evil-define-command my/edit-evil-macro (register)
    "Edit keyboard macro MACRO. MACRO is read from a register
                     when called interactively."
    :keep-visual t
    :suppress-operator t
    (interactive
     (let ((register (or evil-this-register (read-char))))
       (when (eq register ?@)
         (unless evil-last-register
           (user-error "No previously executed keyboard macro"))
         (setq register evil-last-register))
       (list register)))
    ;; this is not yet working, it doesn't seem to correctly write back
    ;; to the original register, and evil doesn't pick up the change
    (lexical-let ((macro (evil-get-register register t)))
      (edit-kbd-macro
       macro
       nil
       nil
       (lambda (new-macro)
         (evil-set-register register new-macro)))))


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
  
  ;; take care not to override global leader
  (define-key evil-motion-state-map (kbd my/leader) nil)

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  (setq evil-insert-state-cursor '((bar . 3) "red")
        evil-normal-state-cursor '(box "white"))
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
  (use-package evil-mc
    :ensure t
    :demand t
    :general
    (:keymaps 'normal
     "C-n" 'my/smart-c-n
     "C-p" 'my/ctrlp-dwim)
    (:keymaps 'visual
     "|" 'my/place-cursors-along-region)
    :config

    (defun my/with-case-sensitivity (orig-fn &rest args)
      "Advice around evil-mc functions to enforce case-sensitivity. Can be disabled by using the universal argument."
      (let ((evil-ex-search-case (if current-prefix-arg
                                     'insensitive
                                   'sensitive)))
        (apply orig-fn args)))
    (advice-add 'evil-mc-make-and-goto-next-match :around #'my/with-case-sensitivity)
    (advice-add 'evil-mc-make-and-goto-prev-match :around #'my/with-case-sensitivity)
    (advice-add 'evil-mc-skip-and-goto-next-match :around #'my/with-case-sensitivity)
    (advice-add 'evil-mc-skip-and-goto-prev-match :around #'my/with-case-sensitivity)

    ;; for some reason, this cannot be done via general-define-key
    ;; (causes emacs to hang?!?)
    (evil-define-key
      'normal
      evil-mc-key-map
      (kbd "C-n")    'my/smart-c-n
      (kbd "C-p")    'my/ctrlp-dwim
      [escape] 'evil-mc-undo-all-cursors
      (kbd "C-s")    'evil-mc-skip-and-goto-next-match
      (kbd "C-S-p")  'evil-mc-skip-and-goto-prev-cursor)
    (global-evil-mc-mode 1)
    (add-to-list 'evil-mc-incompatible-minor-modes 'delim-pad-mode))

  (use-package evil-exchange
    :ensure t
    :config
    (evil-exchange-install)

    ;; evil-exchange breaks under evil-mc, so take care to disable it
    ;; when evil-mc is active
    (defun my/enable-evil-exchange ()
      "Enable c x to trigger evil-exchange. Wrapped in a function to be easily enabled for evil-mc."
      (general-evil-define-key nil evil-normal-state-map
        "c" (general-key-dispatch 'evil-change
              ;; c x x and c x X should swap chars
              "x" (general-key-dispatch 'evil-exchange
                    "x" 'my/swap-chars
                    "X" 'transpose-chars)
              "X" 'evil-exchange-cancel))
      (general-evil-define-key nil evil-visual-state-map
        "c" 'evil-change))


    ;; reinstate the normal evil-change binding so that evil-mc works
    (defun my/disable-evil-exchange ()
      (general-nmap "c" 'evil-change))

    (my/enable-evil-exchange)
    (add-hook 'evil-mc-before-cursors-created 'my/disable-evil-exchange)
    (add-hook 'evil-mc-after-cursors-deleted 'my/enable-evil-exchange)

    ;; general-key-dispatch makes macros record keys twice, so we
    ;; have to disable it again when recording macros
    (advice-add 'start-kbd-macro :before (lambda (arg) (my/disable-evil-exchange)))
    (advice-add 'end-kbd-macro :after #'my/disable-evil-exchange))



  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

  (use-package evil-goggles
    :ensure t
    :config
    (evil-goggles-mode))

  (use-package evil-quickscope
    :ensure t
    :config
    (global-evil-quickscope-mode 1))

  ;; 'x' text object for xml/html attributes
  (use-package exato
    :ensure t
    :config)


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

(defun my/open-around (orig-fun &rest args)
  "Advice to be added around `evil-open-below' and `evil-open-above' that will
handle insertion of block comment prefixes when inside of a block-comment."
  (let ((prefix (my/in-c-block-comment-prefix)))
    (if prefix
        (let ((evil-auto-indent nil))
          (apply orig-fun args)
          (insert prefix))
      (apply orig-fun args))))

(advice-add 'evil-open-below :around #'my/open-around)
(advice-add 'evil-open-above :around #'my/open-around)

(defun my/newline (orig-fun &rest args)
  "Advice to be added around `newline' that will handle insertion of
block comment prefixes when inside of a block-comment."
  (let ((prefix (my/in-c-block-comment-prefix)))
    (apply orig-fun args)
    (when prefix
      (insert prefix))))

(advice-add 'newline :around #'my/newline)

(use-package linum-relative
  :ensure t
  :general
  :config
  )

(provide 'my-evil)
