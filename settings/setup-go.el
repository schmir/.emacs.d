
(defun setup-go-mode ()
  (setq fill-column 99)
  (setq gofmt-command "gofumports")
  (company-mode)
  (require 'lsp)
  ;;(add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'setup-go-mode))

(dolist (cmd '("gofumports" "gofmt" "goimports"))
  (add-to-list 'safe-local-variable-values `(gofmt-command . ,cmd)))

(provide 'setup-go)
