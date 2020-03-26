;;; editor/quick-digraph/config.el -*- lexical-binding: t; -*-


(defun quick-digraph/read-key ( &optional prompt )
  "To be used as an advice overriding `read-key`. Always returns the
`:` key.  Will de-register itself, causing the next call to `read-key`
to reach the original implementation again."
  (message "removing advice again")
  (advice-remove 'read-key 'quick-digraph/read-key) ; un-advice self
  ?:)                                                  ; return `:`

(defun quick-digraph ()
  "Enters a pending digraph-state, where only a couple of characters can be
pressed which will result in german Umlauts. This is done by advising `read-key`
which is called by `evil-insert-digraph` to act as though the user has navigated
to the appropriate digraph-group by pressing `:`."
  (interactive)
  (advice-add 'read-key :override 'quick-digraph/read-key)
  (evil-insert-digraph 1))

(defun quick-digraph/insert-sharp-s ()
  "Quick helper to insert the `ß` character into the buffer at point."
  (interactive)
  (insert "ß"))

(map! :i "M-u" #'quick-digraph)
(map! :i "M-s" #'quick-digraph/insert-sharp-s)
