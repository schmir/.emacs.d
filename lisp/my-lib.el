;;; my-lib --- a small library with helper functions     -*- lexical-binding: t -*-
;;; Code:

;;; Commentary:

;;;###autoload
(defun my/load-theme (theme)
  "Load the given `THEME' even when in daemon mode."
  (if (daemonp)
      ;; load theme via hook
      (letrec ((*load-theme* (lambda (frame)
                               (select-frame frame)
                               (load-theme theme t)
                               (remove-hook 'after-make-frame-functions *load-theme*))))
        (add-hook 'after-make-frame-functions *load-theme*))
    (load-theme theme t)))

(provide 'my-lib)
;;; my-lib.el ends here
