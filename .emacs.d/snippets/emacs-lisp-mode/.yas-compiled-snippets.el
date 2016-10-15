;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
		     '(("use" "(use-package ${1:}\n  :ensure t\n  :config${2:})" "use-package" nil nil nil "/Users/roman/.emacs.d/snippets/emacs-lisp-mode/use-package.yasnippet" nil nil)
		       ("provide" "(provide '${1:`(s-dashed-words (file-name-base buffer-file-name))`})" "provide" nil nil nil "/Users/roman/.emacs.d/snippets/emacs-lisp-mode/provide.yasnippet" nil nil)
		       ("defun" "(defun ${1:my-funcs/${2:}} (${3:})\n  ${4:})\n" "defun" nil nil nil "/Users/roman/.emacs.d/snippets/emacs-lisp-mode/defun.el" nil nil)
		       ("add-hook" "(add-hook '${1:hook-name} ${2:#'fn})" "add-hook" nil nil nil "/Users/roman/.emacs.d/snippets/emacs-lisp-mode/add-hook.el" nil nil)))


;;; Do not edit! File generated at Fri Oct 14 22:22:40 2016
