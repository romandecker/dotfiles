;;; package --- My custom modeline config
;;; Commentary:
;;; Code:
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'automatic)
  (sml/setup))


;; TODO some WIP-code for custom modeline below

;; use force-mode-line-update to update modeline!
(require 's)
(defcustom
  moduline/path-transformers
  '(moduline/shorten-home moduline/shorten-projects moduline/shorten-dotfiles)
  "List of replacers that will be applied in order to (buffer-file-name) before displaying it in the mode-line")


(defun moduline/shorten-home (full-path)
  "Transform /Users/roman/foo or /home/roman/foo to ~/foo."
  (let ((home (expand-file-name "~")))
    (if (s-starts-with? home full-path)
        (concat "~" (substring full-path (length home)))
      full-path)))

(defun moduline/shorten-projects (path)
  (replace-regexp-in-string "~/projects/" ":P:/" path))

(defun moduline/shorten-dotfiles (path)
  (replace-regexp-in-string "~/.dotfiles/" ":.:/" path))

(defun moduline/render-buffer-path ()
  (let ((path (buffer-file-name)))
    (dolist (replacer moduline/path-transformers)
      (if (functionp replacer)
          (progn
            (setq path (apply replacer `(,path))))))
    path))



(require 'cl)
(require 'cl-lib)


(cl-defun moduline/progress-create (style
                                    &key (color "black")
                                    &key (length 1))
  (let ((module `((type . progress)
                  (style . ,style)
                  (progress . 0)
                  (length . ,length))))
    (if (eq style 'clock)
        (setf (alist-get 'image-count module) 8)
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


(defconst my/progress-bar (moduline/progress-create 'bar :length 5 :color "light sea green"))

(progn
  (setf (alist-get 'progress my/progress-bar) 0.5)
  (force-mode-line-update))

; (progn
;   (setq mode-line-format
;         (list
;          '(:eval (moduline/render-buffer-path))
;          " "
;          '(:eval (moduline/progress-render my/progress-bar))
;          ))
;   (force-mode-line-update))

;; (setq my/progress 0)
;; (setq my/timer (run-at-time
;;                 1
;;                 0.1
;;                 (lambda () 
;;                   (setq my/progress (if (> my/progress 1)
;;                                         0
;;                                       (+ my/progress 0.01)))
;;                   (force-mode-line-update))))
;; ; (cancel-timer my/timer)


(provide 'my-modeline)
;;; my-modeline.el ends here
