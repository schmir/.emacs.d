(require 'winring)

(winring-initialize)

(defun schmir-winring-create (name)
  (winring-save-current-configuration)
  (delete-other-windows)
  (switch-to-buffer winring-new-config-buffer-name)
  (winring-set-name name))

(defun winring-jump-or-create (&optional name)
  "Jump to or create configuration by name"
  (interactive)
  (let* ((ring (winring-get-ring))
	 (n (1- (ring-length ring)))
	 (current (winring-name-of-current))
	 (lst (list (cons current -1)))
	 index item)
    (while (<= 0 n)
      (push (cons (winring-name-of (ring-ref ring n)) n) lst)
      (setq n (1- n)))
    (setq name
	  (or name
	      (completing-read
	       (format "Window configuration name (%s): " current)
	       lst nil 'confirm nil 'winring-name-history current)))
    (setq index (cdr (assoc name lst)))
    (if (eq nil index)
	(progn
	  (winring-save-current-configuration)
	  (delete-other-windows)
	  (switch-to-buffer winring-new-config-buffer-name)
	  (winring-set-name name))
      (when (<= 0 index)
	(setq item (ring-remove ring index))
	(winring-save-current-configuration)
	(winring-restore-configuration item)))))

(winring-jump-or-create "gnus")
(winring-prev-configuration)
(winring-set-name "default")

;; (winring-jump-or-create "irc")
;; (winring-jump-or-create "default")

(defun my-switch-to-gnus()
  (interactive)
  (winring-jump-or-create "gnus")

  (if (or (not (fboundp 'gnus-alive-p))
	  (not (gnus-alive-p)))
      (progn
	(delete-other-windows)
	(gnus))

    ;; if we're not in a gnus buffer, just switch to our gnus screen, thus
    ;; returning us to where we were previously. otherwise determine what we
    ;; should switch to
    (progn
      (switch-to-buffer "*Group*")
      (gnus-group-get-new-news))))


(global-set-key [f11] 'my-switch-to-gnus)
(add-hook 'gnus-group-mode-hook
	  (lambda ()
	    (local-set-key (kbd "q") 'winring-prev-configuration)
	    (local-set-key (kbd "Q") 'gnus-group-exit)))


(provide 'setup-winring)
