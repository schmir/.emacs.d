;;; setup-git --- configure git/magit     -*- lexical-binding: t -*-

;;; Code:

;; (setup (:package gitignore-mode))

;; git-messenger: Show git blame info in popup
(setup (:package git-messenger)
  (:option git-messenger:show-detail t
           git-messenger:use-magit-popup t)
  (keymap-global-set "C-x v p" #'git-messenger:popup-message))

;; git-gutter: Show git diff in fringe
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

;; magit: Git porcelain with yadm support
(setup (:package magit)
  (keymap-global-set "C-c m"  #'magit-status)
  (keymap-global-set "C-c y"  #'yadm-status)
  (advice-add 'magit-process-filter :after #'magit-display-ansi-colors))

;; git-link: Generate URLs to files on GitHub/GitLab
(setup (:package git-link)
  (:option git-link-use-commit 't))

(provide 'setup-git)
;;; setup-git.el ends here
