(defvar af/formatters)

(setq af/formatters (make-hash-table :test 'equal))

(defun af/define-formatter (name command)
  (let ((formatter (make-hash-table :test 'equal)))
    (puthash "command" command formatter)
    (puthash name formatter af/formatters)))

(defun af/run-formatter (name buffer)
  (with-current-buffer buffer
    (let* ((formatter (gethash name af/formatters))
           (command (gethash "command" formatter))
           (original-file (buffer-file-name))
           (original (buffer-string)))
      (with-temp-buffer
        (insert original)
        (if (not (zerop
                  (call-process-region (point-min) (point-max) command '(t t) t nil)))

            (message "Error calling %s:" command)

          (call-process-region (point-min) (point-max) "diff" t t nil "-n" original-file "-")
          (let ((diff-buffer (current-buffer)))
            (with-current-buffer buffer
              (message "Applying RCS patch: %s" (with-current-buffer diff-buffer (buffer-string)))
              (af/apply-rcs-patch diff-buffer)))
          )))))

(af/define-formatter "scssfmt"
                     "scssfmt")


(defun my/test-autoformat ()
  (interactive)
  (af/run-formatter "scssfmt" (get-buffer "CategoryInput.scss"))
  )


(defun af/apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in af/apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (af/goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (af/delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in af/apply-rcs-patch")))))))))


(defun af/goto-line (line)
  (goto-char (point-min))
    (forward-line (1- line)))

(defun af/delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))
