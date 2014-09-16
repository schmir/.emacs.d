(eval-after-load "rcirc"
  '(progn
    (message "loading schmir-irc")
    (require 'schmir-irc)))


(defun goto-irc-screen ()
  (interactive)
  (winring-jump-or-create "irc"))

(defun toggle-irc-screen ()
  (interactive)
  (if (string= winring-name "irc")
      (winring-prev-configuration)
    (goto-irc-screen)))

(defun my-irc-next-active()
  (interactive)
  (if (fboundp 'rcirc-next-active-buffer)
      (progn
	(if rcirc-activity
	    (progn
	      (goto-irc-screen)
	      (rcirc-next-active-buffer nil))
	  (if (or
	       (string= winring-name "irc")
	       (if (eq last-command 'my-irc-next-active)
		   t
		 (message "press key again in order to return to irc screen")
		 nil))
	      (toggle-irc-screen))))
    (goto-irc-screen)
    (my-irc)))

(global-set-key [f12] 'my-irc-next-active)

(provide 'setup-irc)
