(defun update-last-modified()
  (interactive)
  (save-excursion
    (let ((case-replace t)
	  (case-fold-search t))
      (goto-char (point-min))
       (while (re-search-forward
	      "\\(Last[ -]\\(changed\\|modified\\):\\) [1-9].*"
	      nil t)

	(replace-match
	 (concat "\\1 "
		 (format-time-string "%Y-%m-%d %H:%M:%S")
		 " by "
		 (user-login-name)
		 )
	 nil nil)))))

(add-hook 'write-file-hooks 'update-last-modified)

;; Turn on time-stamp updating. Timestamp must be in first 8 lines of file and look like:
;; Time-stamp: <>
(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)

(provide 'setup-lastmodified)
