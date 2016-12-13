;;; package --- My custom config for editing JS
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :after helm
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
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

  (use-package js2-refactor
    :ensure t
    :general
    (:states 'normal
     :keymaps 'js2-mode-map
     "] ]" 'js2r-forward-slurp
     "] [" 'js2r-forward-barf)
    (:prefix my/local-leader
     :states 'normal
     :keymaps 'js2-mode-map
     "r e f" 'js2r-extract-function
     "r e m" 'js2r-extract-method
     "r e v" 'js2r-extract-var
     "r l"   'js2r-log-this
     "r s o" 'js2r-expand-object
     "r s a" 'js2r-expand-array
     "r s f" 'js2r-expand-function
     "r s s" 'js2r-split-string
     "r j o" 'js2r-contract-object
     "r j a" 'js2r-contract-array
     "r j f" 'js2r-contract-function
     "r r"   'js2r-rename-var
     "r ."   'js2r-var-to-this
     "r 3"   'js2r-ternary-to-if)
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (which-key-add-key-based-replacements
      (concat my/local-leader " r")   "Refactor..."
      (concat my/local-leader " r e") "Extract..."
      (concat my/local-leader " r j") "Join..."
      (concat my/local-leader " r s") "Split..."))

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

  (require 'helm-source)
  (defvar my-javascript/helm-source-requirable-project-files
    (helm-build-in-buffer-source "Requirable files"
      :data (lambda ()
              (condition-case nil
                  (my-javascript/requirable-files)
                (error nil)))
      ; :fuzzy-match helm-projectile-fuzzy-match
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
          (replace-regexp-in-string "/index.js$" "" relpath))))))


(use-package mocha
  :ensure t
  :general
  (:prefix my/local-leader
   :states 'normal
   :keymaps 'js2-mode-map
   "t p"   'mocha-test-project
   "t f"   'mocha-test-file
   "t t"   'mocha-test-at-point
   "t d p" 'mocha-debug-project
   "t d f" 'mocha-debug-file
   "t d t" 'mocha-debug-at-point)
  :config
  (evil-leader/set-key-for-mode 'js2-mode
    (add-hook 'mocha-compilation-mode-hook (lambda () (setq truncate-lines nil)))))

(use-package nodejs-repl
  :ensure t
  :general
  (:keymaps 'nodejs-repl-mode-map
   [tab] 'comint-dynamic-complete
   "C-r" 'comint-history-isearch-backward
   "C-p" 'comint-previous-input
   "C-n" 'comint-next-input)
  :config
  (add-hook 'nodejs-repl-mode-hook (lambda () (delim-pad-mode t))))

(use-package nvm
  :ensure t
  :config
  (defun my/nvm-use ()
    (interactive)
    (when (projectile-project-p)
      (nvm-use-for (projectile-project-root))
      (message "Activated node %s" nvm-current-version)
      (exec-path-from-shell-copy-env "PATH")))
  (add-hook 'projectile-after-switch-project-hook #'my/nvm-use))

(require 'npm)
(general-define-key
 :states 'normal
 :keymaps 'js2-mode-map
 :prefix my/local-leader
  "n i" 'npm-install
  "n c" 'npm-new
  "n s" 'npm-new-dependency
  "n v" 'npm-version)

(which-key-add-key-based-replacements
  (concat my/local-leader " n") "npm")

(require 'yarn)
(general-define-key
 :states 'normal
 :keymaps '(js2-mode-map json-mode-map)
 :prefix my/local-leader
  "y a" 'yarn-add
  "y c" 'npm-new
  "y d" 'yarn-add-dev
  "y l" 'yarn-link
  "y L" 'yarn-link-package
  "y i" 'yarn-install
  "y o" 'yarn-outdated
  "y p" 'yarn-add-peer
  "y r" 'yarn-run
  "y v" 'yarn-version
  "y w" 'yarn-why)

(defun my/run-in-project-dir (orig-fun &rest args)
  "If currently in a project, run the given function with `default-directory' set to
the project's root. If not currently in a project, run the function normally.
To be used as an around-advice."
  (if (projectile-project-p)
      (let ((default-directory (projectile-project-root)))
        (apply orig-fun args))
    (apply orig-fun args)))

;; automatically run yarn-functions inside the project root
(dolist (fun '(yarn-add
               yarn-add-dev
               yarn-link
               yarn-link-package
               yarn-install
               yarn-outdated
               yarn-add-peer
               yarn-run
               yarn-version
               yarn-why))
  (advice-add fun :around #'my/run-in-project-dir))

(which-key-add-key-based-replacements
  (concat my/local-leader " y")    "yarn"
  (concat my/local-leader " y l")  "Link this package"
  (concat my/local-leader " y L")  "Link another package")

(setenv "NODE_NO_READLINE" "1")

(defun delim-pad-js2-fix (orig-fun &rest args)
  "Fix incorrect behaviour of delim-pad in js2-mode.

For some reason `forward-sexp' suddenly broke in js2-mode. This shims
`forward-sexp-function' in js2-mode when `delim-pad-cmd' is called so
that it behaves like in js-mode (which is correct for most cases)"
  (if (eq major-mode 'js2-mode)
      (let ((forward-sexp-function nil))
        (apply orig-fun args))
    (apply orig-fun args))
  )
(advice-add 'delim-pad-cmd :around #'delim-pad-js2-fix)


(provide 'my-javascript)
;;; my-javascript.el ends here
