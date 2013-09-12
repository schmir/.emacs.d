;;; escreen/gnus interaction
(require 'escreen)

(add-to-list 'escreen-frame-local-variables 'gnus-screen-number)
(add-to-list 'escreen-frame-local-variables 'irc-screen-number)
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
	(emphased ""))

    (dolist (s escreens)
      (setq emphased
	    (concat emphased (if (= escreen-current-screen-number s)
				 (propertize (number-to-string s)
					     ;;'face 'custom-variable-tag) " ")
					     ;; 'face 'info-title-3)
					     'face 'font-lock-warning-face)
			       ;;'face 'secondary-selection)
			       (number-to-string s))
		    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

(defun dim:escreen-goto-last-screen ()
  (interactive)
  (escreen-goto-last-screen)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-prev-screen (&optional n)
  (interactive "p")
  (escreen-goto-prev-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-next-screen (&optional n)
  (interactive "p")
  (escreen-goto-next-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(define-key escreen-map escreen-prefix-char 'dim:escreen-goto-last-screen)

(global-set-key '[M-S-right] (quote dim:escreen-goto-next-screen))
(global-set-key '[M-S-left] (quote dim:escreen-goto-prev-screen))

(global-set-key (kbd "C-\\ DEL") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "C-\\ SPC") 'dim:escreen-goto-next-screen)

;; (global-set-key '[S-mouse-4] 'dim:escreen-goto-prev-screen)
;; (global-set-key '[S-mouse-5] 'dim:escreen-goto-next-screen)

;; Oh, and as I'm in the terms in emacs part of universe (rather than using emacs -nw in some terminal emulator, but loosing sync between X clipbloard and emacs selection), I had to add this too:

;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'dim:escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'dim:escreen-goto-next-screen)


(defadvice gnus-group-exit (after remove-screen (&rest args) activate)
  (escreen-kill-screen)
  (setq gnus-screen-number nil))

;; i'm in the habit of quitting when i don't really need to
(add-hook 'gnus-group-mode-hook
	  (lambda ()
	    (local-set-key (kbd "q") 'escreen-goto-last-screen)
	    (local-set-key (kbd "Q") 'gnus-group-exit)))
(global-set-key [f11] 'my-switch-to-gnus)

(setq gnus-screen-number nil)
(defun my-switch-to-gnus()
  (interactive)
  (if (or (not (fboundp 'gnus-alive-p))
	  (not (gnus-alive-p))
	  (not gnus-screen-number))
      (progn
	(escreen-create-screen)
	(setq gnus-screen-number escreen-current-screen-number)
	;; as i don't do this by default in escreen-create-screen
	(delete-other-windows)
	(gnus))

    ;; if we're not in a gnus buffer, just switch to our gnus screen, thus
    ;; returning us to where we were previously. otherwise determine what we
    ;; should switch to
    (if (eq escreen-current-screen-number gnus-screen-number)
	(escreen-goto-last-screen)
      (escreen-goto-screen gnus-screen-number)
      (switch-to-buffer "*Group*")
      (gnus-group-get-new-news)))
  (escreen-get-active-screen-numbers-with-emphasis))

(provide 'setup-escreen)
