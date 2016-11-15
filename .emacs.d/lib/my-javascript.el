;;; package --- My custom config for editing JS
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :config
  ;; do not show errors (use flycheck for that)
  (js2-mode-hide-warnings-and-errors)

  (setq
   js2-skip-preprocessor-directives nil   ; allow shebangs in js-files (for node)

   ;; default values for indentation (possibly overwritten by editorconfig)
   js2-basic-offset 2
   js-indent-level 2
   js-expr-indent-offset -2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

  (use-package js2-refactor
    :ensure t
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (evil-define-key 'normal js2-mode-map
      (kbd "] ]") 'js2r-forward-slurp
      (kbd "] [") 'js2r-forward-barf)
    (evil-leader/set-key-for-mode 'js2-mode
      "SPC r e f" 'js2r-extract-function
      "SPC r e m" 'js2r-extract-method
      "SPC r e v" 'js2r-extract-var
      "SPC r l"   'js2r-log-this
      "SPC r s o" 'js2r-expand-object
      "SPC r s a" 'js2r-expand-array
      "SPC r s f" 'js2r-expand-function
      "SPC r s s" 'js2r-split-string
      "SPC r j o" 'js2r-contract-object
      "SPC r j a" 'js2r-contract-array
      "SPC r j f" 'js2r-contract-function
      "SPC r r"   'js2r-rename-var
      "SPC r ."   'js2r-var-to-this
      "SPC r 3"   'js2r-ternary-to-if)
    (which-key-add-key-based-replacements
      "SPC SPC r"   "Refactor..."
      "SPC SPC r e" "Extract..."
      "SPC SPC r j" "Join..."
      "SPC SPC r s" "Split...")))

;; Add NodeJS error format so that files can be jumped to in compilation-mode
(pushnew '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                1 ;; file
                2 ;; line
                3 ;; column
                )
         compilation-error-regexp-alist-alist)
(pushnew 'node compilation-error-regexp-alist)

(pushnew '(jshint "^\\(.+\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), .*$"
                  1 ;; file
                  2 ;; line
                  3 ;; column
                  )
         compilation-error-regexp-alist-alist)

(pushnew 'jshint compilation-error-regexp-alist)

(defun my-javascript/requirable-files ()
  "Get all project files that are requirable with node's `require`."
  (-filter
   (lambda (path)
     (string-match-p ".js\\(on\\)?$" path))
   (projectile-current-project-files)))

(defvar my-javascript/helm-source-requirable-project-files
  (helm-build-in-buffer-source "Requirable files"
    :data (lambda ()
            (condition-case nil
                (my-javascript/requirable-files)
              (error nil)))
    :fuzzy-match helm-projectile-fuzzy-match
    :action my/helm-action-return-candidate
    )
  "Helm source definition for files that can be required using node's `require`.")

(defun my-javascript/helm-get-requirable-project-file (&optional initial-input)
  "Start helm to search for requirable project files and return the selected
candidate.
If INITIAL-INPUT is given, helm will initially be filled with the
given string."
  (let* ((path-from-root
          (helm :sources my-javascript/helm-source-requirable-project-files
                :input initial-input)))
    (when path-from-root
      (let* ((abspath (concat (projectile-project-root) path-from-root))
             (relpath (file-relative-name
                       abspath
                       (file-name-directory buffer-file-name)))
             ; make sure that relpath starts with "./"
             (relpath (if (string-match "^\\.\\./" relpath)
                          relpath
                        (concat "./" relpath))))
        ; "index.js can be left out so remove it if it's there
        (replace-regexp-in-string "/index.js$" "" relpath)))))

(use-package mocha
  :ensure t
  :config
  (evil-leader/set-key-for-mode 'js2-mode
    "SPC t p"   'mocha-test-project
    "SPC t f"   'mocha-test-file
    "SPC t t"   'mocha-test-at-point
    "SPC t d p" 'mocha-debug-project
    "SPC t d f" 'mocha-debug-file
    "SPC t d t" 'mocha-debug-at-point)
  (add-hook 'mocha-compilation-mode-hook (lambda () (setq truncate-lines nil))))

(use-package nodejs-repl
  :ensure t
  :config
  (add-hook 'nodejs-repl-mode-hook (lambda () (delim-pad-mode t))
  (define-key nodejs-repl-mode-map [tab] 'comint-dynamic-complete)
  (define-key nodejs-repl-mode-map (kbd "C-r") 'comint-history-isearch-backward)
  (define-key nodejs-repl-mode-map (kbd "C-p") 'comint-previous-input)
  (define-key nodejs-repl-mode-map (kbd "C-n") 'comint-next-input))

(use-package nvm
  :ensure t
  :config)

(provide 'my-javascript)
;;; my-javascript.el ends here
