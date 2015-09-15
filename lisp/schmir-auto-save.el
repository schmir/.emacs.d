;;; schmir-auto-save.el --- automatically save scm managed buffers
;;; Commentary:
;;; Code:

(require 'projectile)
(defun schmir-scm-managed-p ()
  (if (buffer-file-name)
      (condition-case nil
	  (not (eq (projectile-project-vcs) 'none))
	(error nil))))

(defun schmir-save-all-git-project-buffers ()
  "Save all buffers that are managed with git."
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (and (buffer-modified-p) (schmir-scm-managed-p))
	    (progn
	      (message (format "auto-saving scm managed buffer %s" buffer))
	      (save-buffer)))))))

;; (add-hook 'focus-out-hook 'schmir-save-all-git-project-buffers)

(provide 'schmir-auto-save)
;;; schmir-auto-save.el ends here
