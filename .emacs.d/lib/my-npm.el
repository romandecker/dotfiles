;;; package --- 
;;; Commentary:
;;; Code:

(require 'semver)

(defun npm/not-inside-module-error ()
  (signal 'npm/outside-module default-directory))

(defun npm/get-closest-module (path)
  "Obtain the path to the closes package.json file from `PATH'.  If
there is no packag.json file in any parent directory, signal an
error."
  (let* ((cur-path (if (file-directory-p path)
                       (file-name-as-directory path)
                     (file-name-directory path)))
         (package-json-path (concat cur-path "package.json")))

    (while (and (not (file-exists-p package-json-path))
                (not (string= "/" cur-path)))
      (setq cur-path (file-name-directory (directory-file-name cur-path)))
      (setq package-json-path (concat cur-path "package.json")))

    (unless (file-exists-p package-json-path)
      (npm/not-inside-module-error))
    package-json-path))

(defvar-local npm/package-json nil)
(defvar-local npm/package-json-path nil)

(defun npm/generate-new-buffer (path)
  (let* ((package-json-path (npm/get-closest-module path))
         (buffer (generate-new-buffer "*npm*")))
    (with-current-buffer buffer
      (setq-local npm/package-json-path package-json-path))
    buffer))

(defun npm/update-status-buffer (&optional package-json-path)
  (let* ((package-json (json-read-file (or package-json-path npm/package-json-path)))
         (module-name (alist-get 'name package-json))
         (buffer-name (format "npm: %s" module-name))
         (version (alist-get 'version package-json))
         (dependencies (alist-get 'dependencies package-json))
         (dev-dependencies (alist-get 'devDependencies package-json))
         (peer-dependencies (alist-get 'peerDependencies package-json)))
    (setq-local npm/package-son package-json)
    (setq buffer-read-only nil)
    (erase-buffer)
    (rename-buffer buffer-name)
    (insert (format
             "Module: %s%s\n\n"
             module-name
             (propertize (concat "@" version) 'face font-lock-comment-face)))

    (insert (propertize (format "%d Dependencies:\n" (length dependencies))
                        'face
                        font-lock-keyword-face))
    (cl-loop for (module . source) in dependencies do
             (insert (format " %s: %s\n"
                             (propertize (symbol-name module) 'face font-lock-constant-face)
                             source)))

    (goto-char (point-min))
    (setq buffer-read-only t)))

(let ((default-directory "/Users/romande/projects/prs"))
  (npm/async-shell-command-to-json
   "npm list --depth=0 --json"
   (lambda (json)
     ("npm returned: %s" json))))

(defun npm/async-shell-command-to-json (command callback)
  (npm/async-shell-command-to-string command (lambda (raw) (message "raw: %s") (json-read-from-string raw))))


(defun npm/get-buffer (path)
  (let* ((package-json-path (npm/get-closest-module path))
         (package-json (json-read-file package-json-path))
         (module-name (alist-get 'name package-json))
         (buffer-name (format "npm: %s" module-name)))
    (get-buffer buffer-name)
    (or (get-buffer buffer-name)
        (npm/generate-new-buffer path))))

(defun npm/status (&optional path)
  (interactive)
  (let ((buf (npm/get-buffer (or path buffer-file-name))))
    (display-buffer buf)
    (with-current-buffer buf
      (npm/update-status-buffer))
    (select-window (get-buffer-window buf))))

(defun my/test ()
  (interactive)
  (npm/status "/Users/romande/projects/prs/prs-api/src/endpoints/v2/autosuggest.js"))

(defun npm/async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (message "Process sent: %s" signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(define-derived-mode npm-mode special-mode "NPM"
  "Major mode for managing npm-modules, used in npm-status buffers"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (hack-dir-local-variables-non-file-buffer))

(provide 'my-npm)
;;; my-npm.el ends here
