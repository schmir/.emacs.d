;; switch windows with shift-(up/down/left/right)
(require 'winring)
(require 'framemove)
(setq framemove-hook-into-windmove t)
(windmove-default-keybindings 'shift)
(setq frame-title-format `(,"%b    ---   ", (user-login-name) "@" ,(system-name)))
(defun schmir-setup-frame (frame)
  "Set display parameters for the current frame"
  (select-frame frame)
  (if (window-system frame)
      (progn
	(set-cursor-color "red")))
  (winring-set-name "default"))

(add-hook 'after-make-frame-functions 'schmir-setup-frame)
(schmir-setup-frame (selected-frame))

(provide 'setup-frame)
