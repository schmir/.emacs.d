
(setq gnus-init-file (concat home-dir "/.gnus-init.el"))
(add-load-path (concat vendor-dir "gnus/lisp"))

;;; mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-server "mail"
;;       smtpmail-local-domain nil
;;       smtpmail-debug-info t)

(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail)

;; we substitute sendmail with msmtp
(setq sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header)
(setq message-sendmail-envelope-from 'header)



(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; quit gnus properly instead of leaving auto-save files around
(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
	       (gnus-alive-p)
	       (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
	(gnus-group-exit)))))

(defun my-gnus-summary-view-html-alternative-in-mozilla ()
      "Display the HTML part of the current multipart/alternative MIME message
    in mozilla."
      (interactive)
      (save-current-buffer
	(gnus-summary-show-article)
	(set-buffer gnus-article-buffer)
	(let ((file (make-temp-file "html-message-" nil ".html"))
	      (handle (nth 3 (assq 1 gnus-article-mime-handle-alist))))
	  (mm-save-part-to-file handle file)
	  (browse-url-firefox (concat "file://" file)))))


(provide 'setup-gnus)
