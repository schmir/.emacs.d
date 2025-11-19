;;; setup-git --- configure git/magit     -*- lexical-binding: t -*-

;;; Code:

;; (setup (:package gitignore-mode))

(setup (:package git-messenger)
  (:option git-messenger:show-detail t
           git-messenger:use-magit-popup t)
  (keymap-global-set "C-x v p" #'git-messenger:popup-message))

(setup (:package git-gutter)) ;; (global-git-gutter-mode +1)

;; colorize pre-commit output
;; (require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun magit-display-ansi-colors
    (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (display-ansi-colors)))

(defun yadm-status ()
  (interactive)
  (magit-status "/yadm::"))

(setup (:package magit)
  (keymap-global-set "C-c s"  #'magit-status)
  (keymap-global-set "C-c y"  #'yadm-status)
  (advice-add 'magit-process-filter :after #'magit-display-ansi-colors))

(setup (:package git-link)
  (:option git-link-use-commit 't))

(provide 'setup-git)
;;; setup-git.el ends here
