;;; package --- My custom diffing config
;;; Commentary:
;;; Code:


(my/define-leader-map
  "d d" 'ediff-current-file
  "d t" 'my/diff-this)

(setq
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)


(defun my/diff-this ()
  "Take the top two currently open buffers and diff them."
  (interactive)
  (let* ((buffers (seq-take (mapcar 'window-buffer (window-list)) 2))
         (buffer-a (pop buffers))
         (buffer-b (pop buffers)))
    (ediff-buffers buffer-a buffer-b)))



(provide 'my-diff)
;;; my-diff.el ends here
