;;; my-lib --- a small library with helper functions     -*- lexical-binding: t -*-
;;; Code:

;;; Commentary:

;;;###autoload
(defun my/run-when-display-initialized
    (f)
  "Run the given function when the display is initialized"
  (if (daemonp)
      (letrec ((*load-theme* (lambda (frame)
                               (select-frame frame)
                               (funcall f)
                               (remove-hook 'after-make-frame-functions *load-theme*))))
        (add-hook 'after-make-frame-functions *load-theme*))
    (funcall f)))

;;;###autoload
(defun my/load-theme (theme)
  "Load the given `THEME' even when in daemon mode."
  (my/run-when-display-initialized (lambda()
                                     (load-theme theme t))))

(provide 'my-lib)
;;; my-lib.el ends here
