
(defun fixup-scratch-buffer ()
  (set-buffer "*scratch*")
  (lisp-interaction-mode))

(provide 'fixup-scratch-buffer)
