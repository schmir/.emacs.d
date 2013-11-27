(defun schmir-elisp-hook ()
  (enable-paredit-mode)
  (local-set-key [(tab)] 'smart-tab)
  (flycheck-mode 1)
  (highlight-symbol-mode 1))

(add-hook 'emacs-lisp-mode-hook 'schmir-elisp-hook)

(provide 'setup-elisp)
