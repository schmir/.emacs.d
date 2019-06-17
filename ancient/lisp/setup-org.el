(defun schmir-setup-org-mode()
  ;; Make windmove work in org-mode:

  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(add-hook 'org-mode-hook 'schmir-setup-org-mode)
(setq org-replace-disputed-keys t
      org-startup-truncated nil)

(provide 'setup-org)
