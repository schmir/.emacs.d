;;; setup-lisp-directory --- setup the lisp directory     -*- lexical-binding: t -*-

;;; Code:

(require 'loaddefs-gen)

;;;###autoload
(defun update-all-autoloads ()
  (interactive)
  (loaddefs-generate site-lisp-directory
                     (expand-file-name "loaddefs.el" site-lisp-directory)))

;;;###autoload
(defun setup-lisp-directory ()
  (interactive)
  (let ((loaddefs-path (expand-file-name "loaddefs.el" site-lisp-directory)))
    (when (not (file-exists-p loaddefs-path))
      (update-all-autoloads))
    (load loaddefs-path nil t)))

(provide 'setup-lisp-directory)
;;; setup-lisp-directory.el ends here
