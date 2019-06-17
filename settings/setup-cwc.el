(require 'cwc)

(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)
(add-to-list 'whitespace-style 'trailing)
(add-hook 'before-save-hook 'changed-whitespace-cleanup)

;; higlight changes in documents
;; (global-set-key (kbd "<f7>") 'highlight-changes-visible-mode)
;; shift -pgup/pgdown jump to the previous/next change
;; (global-set-key (kbd "<S-prior>") 'highlight-changes-next-change)
;; (global-set-key (kbd "<S-next>")  'highlight-changes-previous-change)

(provide 'setup-cwc)
