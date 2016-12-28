;;; package --- My custom modeline config
;;; Commentary:
;;; Code:
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'automatic)
  (sml/setup))


;; TODO some WIP-code for custom modeline below

(defun moduline/render-module (module)
  (let ((render (alist-get 'render module)))
    (funcall render module)))

;; use force-mode-line-update to update modeline!
(require 's)

(cl-defun moduline/buffer-path-create (&key (transformers
                                             '(moduline/buffer-path-shorten-home)))
  "Create a new buffer-path module. The buffer-path module displays
the current-buffer's path when rendered. It can be configured to
format the path based on various transformations."
  (let ((module `((type . buffer-path)
                  (transformers . ,transformers)
                  (render . moduline/buffer-path-render))))
    module))

(defun moduline/buffer-path-render (module)
  "Render a buffer-path module."
  (let ((path (buffer-file-name))
        (transformers (alist-get 'transformers module)))
    (dolist (replacer transformers)
      (when (functionp replacer)
          (setq path (funcall replacer path))))
    path))

(defun moduline/buffer-path-shorten-home (full-path)
  "Transform /Users/roman/foo or /home/roman/foo to ~/foo."
  (let ((home (expand-file-name "~")))
    (if (s-starts-with? home full-path)
        (concat "~" (substring full-path (length home)))
      full-path)))

(defun moduline/buffer-path-shorten-projects (path)
  (replace-regexp-in-string "~/projects/" ":P:/" path))

(defun moduline/buffer-path-shorten-dotfiles (path)
  (replace-regexp-in-string "~/.dotfiles/" ":.:/" path))


(defconst my/buffer-path-module (moduline/buffer-path-create))
(moduline/buffer-path-render my/buffer-path-module)


(require 'cl)
(require 'cl-lib)



(cl-defun moduline/progress-create (style
                                    &key (color "black")
                                    &key (length 1))
  "Create a new progress-module"
  (let ((module `((type . progress)
                  (style . ,style)
                  (progress . 0)
                  (length . ,length)
                  (render . moduline/progress-render))))
    (if (eq style 'clock)
        (setf (alist-get 'image-count module) 17)
      (setf (alist-get 'image-count module) 16))
    (setf (alist-get 'images module)
          (moduline/progress-load-images module color))
    module))

(cl-defun moduline/progress-render (module)
  (let* ((progress (alist-get 'progress module))
         (images (alist-get 'images module))
         (image-count (alist-get 'image-count module))
         (module-length (alist-get 'length module))
         (percent (* progress 100))
         (full-tile-count (floor (* module-length progress)))
         (empty-tile-count (max 0 (- module-length full-tile-count 1)))
         (percent-per-tile (/ 100 module-length))
         (progress-for-middle-tile (/ (mod percent percent-per-tile) (float percent-per-tile)))
         (full-tile (car (last images)))
         (empty-tile (first images))
         (middle-tile (nth
                       (floor (* progress-for-middle-tile (max 0 (- image-count 1))))
                       images))
         (head (moduline/repeat-image full-tile-count full-tile "|"))
         (tail (moduline/repeat-image empty-tile-count empty-tile))
         (joiner (moduline/repeat-image 1 middle-tile)))
    (if (>= progress 1)
        (moduline/repeat-image module-length full-tile)
      (append head joiner tail))))

(defun moduline/progress-set (progress module)
  (setf (alist-get 'progress module) progress))

(defun moduline/progress-get (module)
  (alist-get 'progress module))

(defun moduline/repeat-image (n image &optional fallback-str)
  (unless fallback-str
    (setq fallback-str "-"))
  (make-list n (propertize fallback-str 'display image)))

(defun moduline/progress-load-images (module &optional color)
  (let ((base-path (moduline/progress-image-directory module))
        (count (alist-get 'image-count module))
        (images ()))
    (dotimes (i count)
      (add-to-list
       'images
       (create-image
        (concat base-path
                (format "%02d" (- (- count 1) i))
                ".xpm")
        'xpm
        nil
        :ascent 'center
        :color-symbols `(("black" . ,color)))))
    images))

(defconst moduline/progress-default-color "black")

(defconst moduline/progress-style-directory
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "moduline/styles/"))

(defun moduline/progress-image-directory (module)
  (let ((subdir (symbol-name (alist-get 'style module))))
    (concat moduline/progress-style-directory subdir "/")))

(defun moduline/reload-images ()
  (setq moduline/progress-images ())
  (dotimes (i moduline/progress-image-count)
    (add-to-list 'moduline/progress-images
                 (create-image
                  (concat moduline/progress-images-directory
                          (format "%02d" (- (- moduline/progress-image-count 1) i))
                          ".xpm")
                  'xpm
                  nil
                  :ascent 'center
                  :color-symbols `(("black" . ,moduline/progress-bar-color))))))


(defconst my/progress-clock (moduline/progress-create 'clock :length 1 :color "green"))

(progn 
  (moduline/progress-set 0.6 my/progress-clock)
  (force-mode-line-update))

;; (progn
;;   (setq mode-line-format
;;         (list
;;          " "
;;          '(:eval (moduline/render-module my/buffer-path-module))
;;          " "
;;          '(:eval (moduline/render-module my/progress-clock))))
;;   (force-mode-line-update))

(moduline/progress-set 0 my/progress-clock)
;; (setq my/timer (run-at-time
;;                 1
;;                 0.1
;;                 (lambda () 
;;                   (let ((p (moduline/progress-get my/progress-clock)))
;;                     (moduline/progress-set 
;;                      (if (> p 1)
;;                          0
;;                        (+ p 0.01))
;;                      my/progress-clock)
;;                   (force-mode-line-update)))))

;;(cancel-timer my/timer)


(provide 'my-modeline)
;;; my-modeline.el ends here
