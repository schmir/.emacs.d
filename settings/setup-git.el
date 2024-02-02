;;; setup-git --- configure git/magit     -*- lexical-binding: t -*-

;;; Code:

;; (use-package gitignore-mode)

(use-package git-messenger
  :defer t
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package git-gutter
  :defer t
  :ensure t
  ;; :init (global-git-gutter-mode +1)
  :diminish)

;; colorize pre-commit output
;; (require 'ansi-color)
(defun display-ansi-colors
    ()
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

(use-package transient :defer t)
(use-package magit
  :defer t
  ;; :after transient
  :bind (("C-c s" . #'magit-status)
         ("C-c y" . #'yadm-status))
  :config
  (advice-add 'magit-process-filter :after #'magit-display-ansi-colors))

(use-package git-link :defer t
  :config
  (setq git-link-use-commit 't))

(provide 'setup-git)
;;; setup-git.el ends here
