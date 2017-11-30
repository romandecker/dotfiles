;;; package --- My custom abbrev config
;;; Commentary:
;;; Code:

(setq
 global-abbrev-file-name "~/.emacs.d/.emacs.abbreviations"
 abbrev-file-name "~/.emacs.d/.emacs.abbreviations")

(defun my/save-abbrevs ()
  (interactive)
  (write-abbrev-file))


(defun my/reload-abbrevs ()
  (interactive)
  (message "Reloading abbrevs from %s" global-abbrev-file-name)
  (quietly-read-abbrev-file global-abbrev-file-name)
  (message "Loaded global abbrevs")
  (if (projectile-project-p)
      (let ((project-abbrevs-file (my/get-local-abbrev-file-name)))
        (message "Loading local abbrevs")
        (when (file-exists-p project-abbrevs-file)
          (quietly-read-abbrev-file project-abbrevs-file)))
    (message "No local abbrevs file to load")
    ))

(defun my/add-global-abbrev (abbreviation expansion)
  "Add an ABBREVIATION that expands to EXPANSION."
  (interactive "MAbbreviation: \nMExpansion: ")
  (define-abbrev global-abbrev-table abbreviation expansion))

(defun my/get-local-abbrev-file-name ()
  (concat (projectile-project-root) ".emacs.abbreviations"))

(defun my/edit-global-abbrevs ()
  (interactive)
  (message "Clearing tables")
  (mapcar (lambda (table) (clear-abbrev-table (symbol-value table))) abbrev-table-name-list)
  (message "Loading global abbrevs only")
  (quietly-read-abbrev-file)
  (message "Editing loaded abbrevs")
  (edit-abbrevs)
  (advice-add 'edit-abbrevs-redefine :after 'my/edit-abbrevs-redefine-global-after-advice))

(defun my/edit-local-abbrevs ()
  (interactive)
  (message "Clearing tables")
  (mapcar (lambda (table) (clear-abbrev-table (symbol-value table))) abbrev-table-name-list)
  (message "Loading local abbrevs only")
  (when (file-exists-p (my/get-local-abbrev-file-name))
    (quietly-read-abbrev-file (my/get-local-abbrev-file-name)))
  (message "Editing loaded abbrevs")
  (edit-abbrevs)
  (advice-add 'edit-abbrevs-redefine :after 'my/edit-abbrevs-redefine-local-after-advice))

(defun my/edit-abbrevs-redefine-global-after-advice ()
  (message "Writing global abbrevs")
  (write-abbrev-file)
  (advice-remove 'edit-abbrevs-redefine 'my/edit-abbrevs-redefine-global-after-advice)
  (my/reload-abbrevs))

(defun my/edit-abbrevs-redefine-local-after-advice ()
  (message "Writing local abbrevs")
  (write-abbrev-file (my/get-local-abbrev-file-name))
  (advice-remove 'edit-abbrevs-redefine 'my/edit-abbrevs-redefine-local-after-advice)
  (my/reload-abbrevs))



(my/define-leader-map "? a a" 'list-abbrevs)
(my/define-leader-map "? a l" 'list-abbrevs)
(my/define-leader-map "? a e" 'my/edit-global-abbrevs)
(my/define-leader-map ". a" 'my/edit-local-abbrevs)

(defcustom abbrev-additional-chars
  '((t ?-))
  "Alist that maps major mode symbols to lists of characters that may appear in abbreviations.
The chars of the special major mode symbol `t' are active in all modes."
  :group 'abbrev
  :type '(repeat :tag "List of modes"
                 (cons :tag "Map major mode symbols to lists of additional chars in abbrevs"
                       (symbol :tag "Mode symbol (`t' stands for all modes)")
                       (repeat :tag "List of additional word-consistent characters" character))))

(defvar-local T-abbrev-syntax-table nil
  "List of additional characters in abbreviations.")

(defun T-abbrev-mode-hook-fun ()
  "Populate T-abbrev-syntax-table with the local syntax table modfied by
the characters in `abbrev-additional-chars'."
  (when abbrev-mode
    (message "abbrev-mode active, building char-list")
    (let ((char-list (append (cdr (assoc major-mode abbrev-additional-chars))
                             (cdr (assoc 't abbrev-additional-chars)))))
      (message "creating normal syntax table")
      (setq T-abbrev-syntax-table (make-syntax-table (syntax-table)))
      (message "extending syntax table")
      (mapcar (lambda (char)
                (message "Adding entry for '%c'" char)
                (modify-syntax-entry char "w" T-abbrev-syntax-table))
              char-list))
    (my/reload-abbrevs)))

;; Wrapping functions of the `abbrev` package with the local syntax table.
;; I'm not sure I captured all fun's that need to run with the local syntax-table.
;; Adding further functions is easy.
;; Just add them to the list at the end of the next form.
(mapcar
 (lambda (fun)
   (let ((newfun (intern (concat "T-ad-" (symbol-name fun)))))
     (eval
      `(progn
         (defun ,newfun (oldfun &rest args)
           ,(concat "This function evaluates `" (symbol-name fun) "' with `T-abbrev-syntax-table' as active syntax table.
It is used for the advicing `" (symbol-name fun) "'.")
           (if T-abbrev-syntax-table
               (with-syntax-table T-abbrev-syntax-table
                 (apply oldfun args))
             (apply oldfun args)))
         (advice-add (quote ,fun) :around (quote ,newfun))))))
 '(define-mode-abbrev abbrev--before-point))

(defun my/abbrev-mode-on ()
  (abbrev-mode 1))

(add-hook 'abbrev-mode-hook #'T-abbrev-mode-hook-fun)
(add-hook 'prog-mode-hook #'my/abbrev-mode-on)
(add-hook 'projectile-after-switch-project-hook #'my/reload-abbrevs)


(provide 'my-abbrev)
;;; my-abbrev.el ends here
