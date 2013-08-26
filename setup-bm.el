(require 'bm)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
			      (bm-buffer-save-all)
			      (bm-repository-save)))

;; Update bookmark repository when saving the file.
(add-hook 'after-save-hook 'bm-buffer-save)

;; Restore bookmarks when buffer is reverted.
(add-hook 'after-revert-hook 'bm-buffer-restore)

(setq bm-highlight-style 'bm-highlight-only-fringe
      bm-recenter 1
      bm-wrap-immediately nil
      bm-buffer-persistence t
      bm-restore-repository-on-load t)

(defadvice bm-buffer-save  (around no-message activate)
  "be quiet when saving bookmarks"
  (flet ((message ())) ad-do-it))

(global-set-key (kbd "<C-f8>") 'bm-toggle)
(global-set-key (kbd "<f8>")   'bm-next)
(global-set-key (kbd "<S-f8>") 'bm-previous)

(provide 'setup-bm)
