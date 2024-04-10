;;; sup  --- what's up? show messages buffer during startup     -*- lexical-binding: t -*-

;;; Code:

(setq sup-previous-buffer nil)

(defun sup-reset()
  (when sup-previous-buffer
    (switch-to-buffer sup-previous-buffer)
    (setq sup-previous-buffer nil)
    (advice-remove 'messafe #'sup-redisplay)))

(defun sup-redisplay (&rest args)
  (redisplay))

;;;###autoload
(defun sup-show-messages ()
  (unless (or sup-previous-buffer after-init-time)
    (setq sup-previous-buffer (current-buffer))
    (advice-add 'message :after #'sup-redisplay)
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (switch-to-buffer (current-buffer)))
    (add-hook 'after-init-hook #'sup-reset -100)))

;;;###autoload
(defun sup-package-install (package)
  (unless (package-installed-p package)
    (sup-show-messages)
    (message "Installing package %s" package)
    (unless (assoc 'setup package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(provide 'sup)
;; sup.el ends here
