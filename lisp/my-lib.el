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
  (require 's)
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

;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
;;;###autoload
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;;;###autoload
(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

;;;###autoload
(defun with-project-root-as-default-directory
    (orig-fun &rest args)
  "Run orig-fun with default-directory set to project's root directory"
  (let* ((root (when (project-current)
                 (project-root (project-current))))
         (default-directory (or root default-directory)))
    (apply orig-fun args)))

(provide 'my-lib)
;;; my-lib.el ends here
