;;; editor/++evil/config.el -*- lexical-binding: t; -*-

(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'")

(map! :n "C-u" #'evil-scroll-up)
(use-package! evil-numbers
  :config
  (map! :n "C-a" #'evil-numbers/inc-at-pt)
  (map! :n "C-s" #'evil-numbers/dec-at-pt))


(when (featurep! :editor multiple-cursors)

  (defun +evil/place-cursors-along-region (start end)
    "Place cursors along the given region given by START and END (or
interactively along the current region)."
    (interactive "r")
    (let ((col (save-excursion
                 (goto-char start)
                 (current-column)))
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

  (after! evil-mc
    (map! :v "|" #'+evil/place-cursors-along-region))
  )
