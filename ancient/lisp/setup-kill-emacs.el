(defadvice save-buffers-kill-emacs (around emacs-die-hard activate)
  "really???"
  (if (or
       (not server-process)
       (string= "kill emacs" (read-from-minibuffer "to quit emacs type: 'kill emacs':")))
      ad-do-it))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; (global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)

(provide 'setup-kill-emacs)
