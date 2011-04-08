;;; configure rcirc
(require 'rcirc)

(rcirc-track-minor-mode 1)

(setq rcirc-time-format "%Y-%m-%d %R "
      rcirc-buffer-maximum-lines 1000
      rcirc-fill-flag nil)

(set-face-foreground 'rcirc-nick-in-message "green" nil)
(set-face-foreground 'rcirc-timestamp "grey50" nil)
; Always keep the prompt at the bottom of the buffer

(defun setup-rcirc-mode()
  (interactive)
  (set (make-local-variable 'scroll-margin) 0)
  (set (make-local-variable 'scroll-conservatively) 8192))

(add-hook 'rcirc-mode-hook
	  'setup-rcirc-mode)

;; ; Wrap long lines according to the width of the window
;; ; does not work in daemon mode
;; (add-hook 'window-configuration-change-hook
;;           '(lambda ()
;;              (setq rcirc-fill-column (- (window-width) 2))))


(defun kill-all-mode-buffers(mode)
  "Kill all buffers that use major-mode mode."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
	(set-buffer buffer)
	(when (equal major-mode mode)
	  (setq count (1+ count))
	  (kill-buffer buffer)))
      (message "Killed %i buffer(s)." count ))))


(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-all-mode-buffers 'rcirc-mode))



;;; configure rcirc logging

(defun my-fsquote (fn)
  (replace-regexp-in-string
   "[&*#<>@\\]"
   (lambda (x)
     (cond
      ((equal x "@")  "-at-")
      ((equal x ">") "}")
      ((equal x "<") "{")
      (t "")))
   fn))

;; (my-fsquote "&#<foo@bar>")

(defun my-rcirc-log-filename-function (process target)
  (concat
   (my-fsquote (rcirc-generate-new-buffer-name process target))
   (format-time-string "-%Y-%m")))

(setq rcirc-log-flag t
      rcirc-log-directory "~/.irc-log/"
      rcirc-log-filename-function 'my-rcirc-log-filename-function)


(provide 'schmir-irc)
