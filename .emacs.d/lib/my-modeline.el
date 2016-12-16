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

;; (progn
;;   (setq mode-line-format
;;         (list
;;          '(:eval (moduline/render-buffer-path))
;;          " "
;;          '(:eval (moduline/render-progress-bar my/progress 5))
;;          ))
;;   (force-mode-line-update))

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


;; (require 'cl)
;; (defun moduline/render-progress (progress max-length images)
;;   (let* ((percent (* progress 100))
;;          (full-tile-count (floor (* max-length progress)))
;;          (empty-tile-count (max 0 (- max-length full-tile-count 1)))
;;          (percent-per-tile (/ 100 max-length))
;;          (progress-for-middle-tile (/ (mod percent percent-per-tile) (float percent-per-tile)))
;;          (full-tile (car (last images)))
;;          (empty-tile (first progress-images))
;;          (middle-tile (nth
;;                        (floor (* progress-for-middle-tile (max 0 (- moduline/progress-image-count 1))))
;;                        progress-images))
;;          (head (moduline/repeat-image full-tile-count full-tile "|"))
;;          (tail (moduline/repeat-image empty-tile-count empty-tile))
;;          (joiner (moduline/repeat-image 1 middle-tile)))
;;     (if (>= progress 1)
;;         (moduline/repeat-image max-length full-tile)
;;       (append head joiner tail))))

;; (defun moduline/repeat-image (n image &optional fallback-str)
;;   (unless fallback-str
;;     (setq fallback-str "-"))
;;   (make-list n (propertize fallback-str 'display image)))


;; (defconst moduline/progress-styles '('bar 'clock))
;; (defconst moduline/progress-image-count 16)
;; (defconst moduline/progress-bar-color "light sea green")
;; (defconst moduline/image-directory (concat (file-name-directory (or load-file-name buffer-file-name)) "../img/"))
;; (defconst moduline/progress-images-directory (concat moduline/image-directory "progress-bar/"))

;; (defconst moduline/progress-images ())

;; (defun moduline/reload-images ()
;;   (setq moduline/progress-images ())
;;   (dotimes (i moduline/progress-image-count)
;;     (add-to-list 'moduline/progress-images (create-image
;;                                       (concat
;;                                       moduline/progress-images-directory
;;                                       (format "%02d" (- (- moduline/progress-image-count 1) i))
;;                                       ".xpm")
;;                                       'xpm
;;                                       nil
;;                                       :ascent 'center
;;                                       :color-symbols `(("black" . ,moduline/progress-bar-color))))))




(provide 'my-modeline)
;;; my-modeline.el ends here
