
(defun running-as-server-p ()
  "Returns true if `server-start' has been called."
  (condition-case nil
      (and (boundp 'server-process)
	   (memq (process-status server-process)
		 '(connect listen open run)))
    (error)))

;; Only start server mode if it isn't started already
(unless (= 0 (user-uid))
  (when (not (running-as-server-p))
    (server-start)))

;; (add-hook 'server-done-hook 'delete-frame)

(provide 'setup-server)
