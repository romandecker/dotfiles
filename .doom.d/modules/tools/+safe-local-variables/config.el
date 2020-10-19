;;; tools/+safe-local-variables/config.el -*- lexical-binding: t; -*-

(defun +safe-local-variables/make-variable-value-safe (variable variable-value)
  (add-to-list 'safe-local-variable-values `(,variable . ,variable-value))
  (customize-save-variable 'safe-local-variable-values safe-local-variable-values))

(defun +safe-local-variables/counsel-make-variable-value-safe ()
  "Read a variable and mark it's current value as safe via customize"
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Variable: " obarray
              :predicate #'counsel--variable-p
              :require-match t
              :history 'counsel-describe-symbol-history
              :keymap counsel-describe-map
              :preselect (ivy-thing-at-point)
              :action (lambda (x)
                        (funcall '+safe-local-variables/counsel-make-variable-value-safe-function (intern x)))
              :caller '+safe-local-variables/counsel-make-variable-value-safe)))

(defun +safe-local-variables/counsel-make-variable-value-safe-function (variable)
  (let* ((variable-value (symbol-value variable)))
    (message "Setting %s to %s" variable variable-value)
    (+safe-local-variables/make-variable-value-safe variable variable-value)))
