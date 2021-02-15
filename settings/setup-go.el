(defun setup-go-mode ()
  (eglot-ensure)
  (setq fill-column 99)
  (setq gofmt-command "gofumports")
  (company-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'setup-go-mode))

(dolist (cmd '("gofumports" "gofmt" "goimports"))
  (add-to-list 'safe-local-variable-values `(gofmt-command . ,cmd)))

(provide 'setup-go)
