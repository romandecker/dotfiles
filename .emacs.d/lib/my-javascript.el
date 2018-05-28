;;; package --- My custom config for editing JS
;;; Commentary:
;;; Code:

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

;; prevent a '.' in multi-line declarations to perform indentation
(setq js--indent-operator-re "[-+*/%<>&^|?:]\\([^-+*/.]\\|$\\)\\|!?=\\|\\_<\\(in\\(?:stanceof\\)?\\)\\_>")

(setq js-indent-level 2)

(use-package js2-mode
  :ensure t
  :demand t
  :after helm
  :config
  ;; do not show errors (use flycheck for that)
  (js2-mode-hide-warnings-and-errors)

  (require 'prettier-js)
  (setq prettier-target-modes '("js-mode" "js2-mode" "rjsx-mode" "typescript-mode" "scss-mode"))
  (setq prettier-args '("--print-width" "100" "--single-quote" "--jsx-bracket-same-line"))

  ;; make prettier available as a minor mode for easy toggling
  (define-minor-mode my/prettier-js-mode
    :init-value nil
    :lighter " Prty"
    :keymap (make-sparse-keymap)
    (if my/prettier-js-mode
        (add-hook 'before-save-hook 'prettier-before-save)
      (remove-hook 'before-save-hook 'prettier-before-save)))

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
    :general
    (:states 'normal
     :keymaps 'js2-mode-map
     "] ]" 'my/generic-slurp-right
     "] [" 'my/generic-barf-right
     "[ ]" 'my/generic-barf-left
     "[ [" 'my/generic-slurp-left)
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
     "r 3"   'js2r-ternary-to-if
     "r t a" 'js2r-toggle-arrow-function-and-expression
     )
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (setq js2r-always-insert-parens-around-arrow-function-params nil)
    (which-key-add-key-based-replacements
      (concat my/local-leader " r")   "Refactor..."
      (concat my/local-leader " r e") "Extract..."
      (concat my/local-leader " r j") "Join..."
      (concat my/local-leader " r s") "Split..."))

  (defun my-javascript/requirable-files ()
    "Get all project files that are requirable with node's `require`."
    (-filter
     (lambda (path)
       (string-match-p ".js\\(on\\|x\\)?$" path))
     (projectile-current-project-files)))

  (defcustom my/javascript-import-aliases '()
    "List of mappings from path to alias to use when importing files.

Example value: ((\"src/app/action\" . \"@action\"))")

  (defcustom my/javascript-import-omit-index t
    "Whether or not to automatically omit index files when importing javascript files.")

  (defcustom my/javascript-import-omit-extension nil
    "Whether or not to automatically omit file extensions when importing javascript files.")

  (defun my-javascript/process-require-candidate (candidate)
    (message "Processing %s" candidate)
    (let ((was-aliased nil))
      (message "Checking aliases in %s" my/javascript-import-aliases)
      (cl-loop for (prefix . replacement) in my/javascript-import-aliases do
               (message "Checking for %s" prefix)
               (when (string-prefix-p prefix candidate)
                 (message "match!")
                 (setq candidate (replace-regexp-in-string
                                  (concat "^" (regexp-quote prefix))
                                  replacement
                                  candidate)
                       was-aliased t)
                 (message "Replaced: %s" candidate)
                 (cl-return)))

      (message "candidate after: %s" candidate)
      ;; "index.js can be left out so remove it if it's there
      (let ((ret (if was-aliased candidate (my/javascript-relativize-candidate candidate))))

        (when my/javascript-import-omit-index
          (setq ret (replace-regexp-in-string "/index\\.jsx?$" "" ret)))

        (when my/javascript-import-omit-extension
          (setq ret (file-name-sans-extension ret)))

        ret)))

  (defun my/javascript-relativize-candidate (candidate)
    (let* ((abspath (concat (projectile-project-root) candidate))
          (relpath (file-relative-name
                    abspath
                    (file-name-directory buffer-file-name))))
      ;; make sure that relpath starts with "./"
      (if (string-match "^\\.\\./" relpath)
          relpath
        (concat "./" relpath))))

  (defvar my/helm-action-return-require-path-to-file
    (helm-make-actions
     "Select"
     'my-javascript/process-require-candidate))

  (require 'helm-source)
  (defvar my-javascript/helm-source-requirable-project-files
    (helm-build-in-buffer-source "Requirable files"
      :data (lambda ()
              (condition-case nil
                  (my-javascript/requirable-files)
                (error nil)))
                                        ; :fuzzy-match helm-projectile-fuzzy-match
      :action my/helm-action-return-require-path-to-file
      )
    "Helm source definition for files that can be required using node's `require`.")

  (defvar my-javascript/helm-source-node-builtins
    (helm-build-in-buffer-source "node builtins"
      :data '("fs" "stream" "path")
      :action my/helm-action-return-candidate))
  

  (defvar my-javascript/helm-source-npm-packages
    (helm-build-in-buffer-source "NPM packages"
      :data (lambda ()
              (my-javascript/get-npm-packages))
      :action my/helm-action-return-candidate
    )
  "Helm source definition for requirable npm packages.")

  (defun my-javascript/helm-get-requirable-project-file (&optional initial-input)
    (interactive)
    "Start helm to search for requirable project files and return the selected
candidate.
If INITIAL-INPUT is given, helm will initially be filled with the
given string."
    (helm :sources '(my-javascript/helm-source-requirable-project-files
                     my-javascript/helm-source-npm-packages
                     my-javascript/helm-source-node-builtins)
          :input initial-input))


  (use-package xref-js2
    :ensure t
    :config)
  )

(use-package rjsx-mode
  :ensure t
  :general
  ;; rjsx-electric-lt seems to hang emacs ?!?
  (:states 'insert
   :keymaps 'rjsx-mode-map
   "<" 'self-insert-command)
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

  (general-evil-define-key 'normal 'rjsx-mode-map
    "K" 'helm-dash-at-point))

(use-package flycheck-flow
  :ensure t
  :config
  ;; make sure flow is at the end of the checker list
  (setq flycheck-checkers (delete 'javascript-flow flycheck-checkers))
  (setq flycheck-checkers (delete 'javascript-flow-coverage flycheck-checkers))
  (add-to-list 'flycheck-checkers 'javascript-flow t)
  (add-to-list 'flycheck-checkers 'javascript-flow-coverage t)

  ;; make flow run after eslint
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

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
  :config)

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
      (message "Activated node %s" nvm-current-version)))
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

(require 'json)
(require 'subr-x)
(defun my-javascript/get-npm-packages ()
  "Returns a list of npm packages required by the current project."
  (ignore-errors
    (let* ((root (file-name-directory (cond
                                       ((projectile-project-p) (projectile-project-root))
                                       (t default-directory))))
           (package-json-path (concat root "package.json"))
           (package-json (with-temp-buffer
                           (insert-file-contents package-json-path)
                           (let ((json-object-type 'hash-table)) (json-read))))
           (deps (hash-table-keys (or (gethash "dependencies" package-json) (make-hash-table))))
           (dev-deps (hash-table-keys (or (gethash "devDependencies" package-json) (make-hash-table))))
           (peer-deps (hash-table-keys (or (gethash "peerDependencies" package-json) (make-hash-table))))
           )
      (append deps dev-deps peer-deps))))

(use-package company-flow
  :ensure t
  :config
  (add-to-list 'company-backends 'company-flow))


(use-package flow-minor-mode
  :ensure t
  :config
  (require 'flow-js2-mode))

(provide 'my-javascript)
;;; my-javascript.el ends here
