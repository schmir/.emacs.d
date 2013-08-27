(eval-after-load "rcirc"
  '(progn
    (message "loading schmir-irc")
    (require 'schmir-irc)))


(setq irc-screen-number nil)
(defun goto-irc-screen ()
  (interactive)
  (if (not irc-screen-number)
      (progn
	(escreen-create-screen)
	(setq irc-screen-number escreen-current-screen-number)
	;; as i don't do this by default in escreen-create-screen
	(delete-other-windows))
    (escreen-goto-screen irc-screen-number)))

(defun toggle-irc-screen ()
  (interactive)
  (if (eq irc-screen-number escreen-current-screen-number)
      (escreen-goto-last-screen)
    (goto-irc-screen))
  (escreen-get-active-screen-numbers-with-emphasis))

(defun my-irc-next-active()
  (interactive)
  (if (fboundp 'rcirc-next-active-buffer)
      (progn
	(if rcirc-activity
	    (progn
	      (goto-irc-screen)
	      (rcirc-next-active-buffer nil)
	      (escreen-get-active-screen-numbers-with-emphasis))
	  (if (or
	       (eq irc-screen-number escreen-current-screen-number)
	       (if (eq last-command 'my-irc-next-active)
		   t
		 (message "press key again in order to return to irc screen")
		 nil))
	      (toggle-irc-screen))))
    (goto-irc-screen)
    (my-irc)))

(global-set-key [f12] 'my-irc-next-active)

(provide 'setup-irc)
