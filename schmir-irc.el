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
    (let((count 0)
	 (kill-buffer-query-functions nil))
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


(defun my-mark-join (process sender response target text)
  ;; process=systemexit.de sender=schmir response=PRIVMSG target=&bitlbee text=bla
  ;; (message "rcirc process=%s sender=%s response=%s target=%s text=%s" process sender response target text)
  (if (and (string= target "&bitlbee")
	   (or (string= response "JOIN") (string= response "QUIT")))
      (progn
	;; (message "rcirc process=%s sender=%s response=%s target=%s text=%s" process sender response target text)
	(message "%s %s" response sender)
	(rcirc-record-activity (current-buffer)))))

(add-hook 'rcirc-print-hooks 'my-mark-join)

(defun my-rcirc-remove-color-control-characters(&rest ignore)
  (interactive)

  (while (re-search-forward "\C-c[0-9][0-9]\\(,[0-9][0-9]\\)?\\|[\C-o|\C-b]" nil t)
    (replace-match "" nil nil)))

(add-to-list 'rcirc-markup-text-functions 'my-rcirc-remove-color-control-characters)

;; 2011-05-31 16:55 <CIA-41> 03haypo 07roundup * 10#7978/SocketServer doesn't handle syscall interruption: Using signalfd() (require Linux 2.6.22+), specified signals ... * 14http://bugs.python.org/issue7978
;; 2011-05-31 16:55 <CIA-41> 03haypo 07roundup * 10#7978/SocketServer doesn't handle syscall interruption: I think that #12224 is a duplicate of this issue. * 14http://bugs.python.org/issue7978


(provide 'schmir-irc)
