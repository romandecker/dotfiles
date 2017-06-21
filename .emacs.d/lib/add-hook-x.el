;;; add-hook-x.el --- a convenience macro for add-hook -*- lexical-binding: t; -*-

;; Copyright (c) 2017 Ivan Sokolov

;; Author: Ivan Sokolov <sirikid@openmailbox.org>
;; Keywords: lisp
;; Version: 0.0.0
;; Package-Requires: (cl-lib seq)

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; A convenience macro for `add-hook', yet another missing part of Emacs.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'seq))

(defun add-hook-x--mk-hook (symbol)
  `(quote ,(intern (format "%s-hook" symbol))))

(defun add-hook-x--hookify (object)
  (pcase object
    ((pred symbolp)
     (add-hook-x--mk-hook object))
    (`(quote ,(pred symbolp))
     object)
    (_
     (error "Expected a symbol or a quoted symbol, but got %s" object))))

(defun add-hook-x--parse-hooks (object)
  (pcase object
    ((pred symbolp)
     (list (add-hook-x--mk-hook object)))
    (`(quote ,(pred symbolp))
     (list object))
    ((pred seqp)
     (if (seq-empty-p object)
         (error "Hooks sequence can not be empty")
       (seq-map #'add-hook-x--hookify object)))
    (_
     (error "Expected a symbol, a quoted symbol or a sequence, but got %s" object))))

(defun add-hook-x--mk-func (symbol)
  `(function ,symbol))

(defun add-hook-x--funcify (object)
  (pcase object
    ((pred functionp)
     (add-hook-x--mk-func object))
    (`(quote ,(pred functionp))
     (add-hook-x--mk-func (cl-second object)))
    (`(function ,(pred functionp))
     object)
    (_
     (error "Expected a function or a quoted function, but got: %s" object))))

(defun add-hook-x--parse-funcs (object)
  (pcase object
    ((pred functionp)
     (list (add-hook-x--mk-func object)))
    (`(quote ,(pred functionp))
     (list (add-hook-x--mk-func (cl-second object))))
    (`(function ,(pred functionp))
     (list object))
    ((pred seqp)
     (if (seq-empty-p object)
         (error "Functions sequence can not be empty")
       (seq-map #'add-hook-x--funcify object)))
    (_
     (error "Expected a function, a quoted function or a sequence, but got: %s" object))))

(defun add-hook-x--generalized (action hook-or-hooks func-or-funcs extra-args)
  "Since `add-hook-x' and `remove-hook-x' has a very similar structure, I
extracted the common part into this function.

ACTION - I was assumed that this would be an `add-hook' or `remove-hook'.
HOOK-OR-HOOKS - a symbol, a quoted symbol, or a sequence of them.
FUNC-OR-FUNCS - a symbol, a quoted symbol, a function, or a sequence of them.
EXTRA-ARGS - additional arguments for the ACTION, list."
  (unless (functionp action)
    (error "ACTION should be a function"))
  (when (null hook-or-hooks)
    (error "HOOK-OR-HOOKS can not be NIL"))
  (when (null func-or-funcs)
    (error "FUNC-OR-FUNCS can not be NIL"))
  (let ((hooks (add-hook-x--parse-hooks hook-or-hooks))
        (funcs (add-hook-x--parse-funcs func-or-funcs))
        result)
    (seq-doseq (hook hooks)
      (seq-doseq (func funcs)
        (cl-pushnew `(,action ,hook ,func ,@extra-args) result)))
    (if (= 1 (length result))
        (cl-first result)
      `(progn ,@(nreverse result)))))

;;;###autoload
(defmacro add-hook-x (hook-or-hooks func-or-funcs &optional append local)
  "A convenience macro for `add-hook'.
Calling this macro is equivalent to calling `add-hook' for each pair from
Cartesian product of HOOK-OR-HOOKS and FUNC-OR-FUNCS.

HOOK-OR-HOOKS - a symbol, a quoted symbol, or a sequence of them.
FUNC-OR-FUNCS - a symbol, a quoted symbol, a function, or a sequence of them."
  (declare (indent defun))
  (add-hook-x--generalized #'add-hook hook-or-hooks func-or-funcs (list append local)))

;;;###autoload
(defmacro remove-hook-x (hook-or-hooks func-or-funcs &optional local)
  "Dual macro for `add-hook-x'.
Calling this macro is equivalent to calling `remove-hook' for each pair from
Cartesian product of HOOK-OR-HOOKS and FUNC-OR-FUNCS.

HOOK-OR-HOOKS - a symbol, a quoted symbol, or a list of them.
FUNC-OR-FUNCS - a symbol, a quoted symbol, a function, or a list of them."
  (declare (indent defun))
  (add-hook-x--generalized #'remove-hook hook-or-hooks func-or-funcs (list local)))

(provide 'add-hook-x)
;;; add-hook-x.el ends here
