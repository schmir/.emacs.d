(unless (= 0 (user-uid))
  ;; Only start server mode if it isn't started already
  (when (or (not (boundp 'server-process))
	    (not (eq (process-status server-process)
		     'listen)))
    (server-start)))


(add-hook 'server-done-hook 'delete-frame)

(provide 'setup-server)
