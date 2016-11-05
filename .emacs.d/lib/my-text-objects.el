;;; package --- My custom text objects
;;; Commentary:
;;; Code:

(require 'thingatpt-plus)

(defun my/test ()
  (interactive)
  (let* ((bounds (tap-bounds-of-thing-nearest-point 'string))
         (start (car bounds))
         (end (cdr bounds)))
    (goto-char start)))

(define-key evil-normal-state-map (kbd "g t") 'my/test)

(evil-define-text-object evil-outer-nearest-string (count &optional beg end type)
  "Select the nearest string."
  (let* ((bounds (tap-bounds-of-thing-nearest-point 'string))
         (start (car bounds))
         (end (cdr bounds)))
    (evil-range start end)))

(evil-define-text-object evil-inner-nearest-string (count &optional beg end type)
  "Select the nearest string."
  (let* ((bounds (tap-bounds-of-thing-nearest-point 'string-contents))
         (start (car bounds))
         (end (cdr bounds)))
    (evil-range start end)))

(define-key evil-outer-text-objects-map (kbd "s") 'evil-outer-nearest-string)
(define-key evil-inner-text-objects-map (kbd "s") 'evil-inner-nearest-string)

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


(define-and-bind-text-object "$" "\\$" "\\$")
(define-and-bind-text-object "." "\\." "\\.")
(define-and-bind-text-object "/" "/" "/")
(define-and-bind-text-object "%" "%" "%")
(define-and-bind-text-object "," "," ",")
(define-and-bind-text-object ":" ":" ":")

;; line text object (inner line is without whitespace)
(define-and-bind-text-object "l" "^\\s-*" "\\s-*$")

;; "entire buffer" text object (inner entire buffer is without whitespace)
(define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'")


(provide 'my-text-objects)
;;; my-text-objects.el ends here
