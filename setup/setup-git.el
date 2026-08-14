;;; setup-git --- configure git/magit     -*- lexical-binding: t -*-

;;; Code:

;; (setup (:package gitignore-mode))

;; git-timemachine: browse historic versions of a file with p (previous) and n (next)
(setup (:package git-timemachine))

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

(defvar-local magit-ansi-colors-applied-to nil
  "Position up to which ANSI escapes have been interpreted in this buffer.")

(defun magit-display-ansi-colors
    (proc &rest _args)
  "Colorize the ANSI escapes PROC just wrote.  Advises `magit-process-filter'.

Only the newly arrived output is scanned; re-reading the whole buffer on
every chunk makes a long-running process quadratic in its own output.
When the previous chunk ended midway through an escape sequence,
`ansi-color-apply-on-region' resumes from it via
`ansi-color-context-region' and ignores the start given here."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (start (or magit-ansi-colors-applied-to (point-min))))
          (when (< start (point-max))
            (ansi-color-apply-on-region start (point-max))
            ;; Move the existing marker rather than making a new one each
            ;; time: every insertion has to adjust every marker in the
            ;; buffer, so accumulating them costs more than it saves.
            (setq magit-ansi-colors-applied-to
                  (if magit-ansi-colors-applied-to
                      (set-marker magit-ansi-colors-applied-to (point-max))
                    (copy-marker (point-max))))))))))

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
