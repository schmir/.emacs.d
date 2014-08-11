(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1))
  (turn-on-flyspell)
  (toggle-save-place 0))

(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)


;; make magit commit mode not close the frame
;; we have (add-hook 'server-done-hook 'delete-frame)
;; see https://github.com/magit/magit/issues/771
(defadvice git-commit-commit (around no-kill-frame activate)
  (flet ((delete-frame (&optional FRAME FORCE) ()))
    ad-do-it))

(setq magit-omit-untracked-dir-contents t)

(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
     (if (eq system-type 'windows-nt)
       (setq magit-git-executable (executable-find "git")))))
;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))


(global-set-key (kbd "C-c m") 'magit-status)

;; git-messenger
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(provide 'setup-magit)
