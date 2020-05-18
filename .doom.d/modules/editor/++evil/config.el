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

(use-package evil-numbers
  :config
  (map! :n "C-a" #'evil-numbers/inc-at-pt)
  (map! :n "C-s" #'evil-numbers/dec-at-pt))
