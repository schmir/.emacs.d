;;; sup  --- what's up? show messages buffer during startup     -*- lexical-binding: t -*-

;;; Code:

(setq sup-previous-buffer nil)

(defun sup-reset()
  (when sup-previous-buffer
    (switch-to-buffer sup-previous-buffer)
    (setq sup-previous-buffer nil)
    (advice-remove 'message #'sup-redisplay)))

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


(defun sup-package-symbol
    (package)
  (if (consp package)
      (car package)
    package))

(defun sup-package-or-library-installed-p
    (package)
  (let ((sym (sup-package-symbol package)))
    (or (package-installed-p sym)
        (locate-library (symbol-name sym)))))

;;;###autoload
(defun sup-package-install (package)
  (unless (sup-package-or-library-installed-p package)
    (sup-show-messages)
    (if (consp package)
        (progn
          (message "===========> Installing vc package %s" package)
          (package-vc-install package))
      (unless (assq package package-archive-contents)
        (message "===========> Refreshing package archive")
        (package-refresh-contents))
      (message "===========> Installing package %s" package)
      (package-install package))))

(provide 'sup)
;; sup.el ends here
