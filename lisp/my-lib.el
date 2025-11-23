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

;;; Copied from https://pkaznowski.gitlab.io/blog/post/sort-words-in-region/
;;;###autoload
(defun my/sort-words-in-region (beg end &optional reversed)
  "In active region sort words alphabetically in ascending order.
With prefix argument REVERSED use descending order.
Don't use this function on regions with nested brackets."
  (interactive "r\nP")
  (unless (region-active-p) (user-error "No active region to sort!"))
  (let* ((str (s-trim (buffer-substring-no-properties beg end)))
         (com (string-match-p "," str))
         (cln (replace-regexp-in-string "[\]\[(){}\']+\\|\\.$" "" str))
         (wrd (split-string cln (if com "," " ") t " "))
         (new (s-join (if com ", " " ")
                      (sort wrd (if reversed #'string> #'string<)))))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (when (and (looking-back "[^ ]") (not (s-starts-with? " " str)))
        (insert " "))
      (insert
       (replace-regexp-in-string "[^\]\[(){}\'\.]+" new str)))))

;;;###autoload
(defun my/shift-left ()
  (interactive)
  (when mark-active
    (let ((deactivate-mark nil))
      (indent-rigidly (region-beginning) (region-end) (- standard-indent)))))

;;;###autoload
(defun my/shift-right ()
  (interactive)
  (when mark-active
    (let ((deactivate-mark nil))
      (indent-rigidly (region-beginning) (region-end) standard-indent))))

(provide 'my-lib)
;;; my-lib.el ends here
