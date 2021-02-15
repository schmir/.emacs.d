(defun setup-go-mode ()
  (lsp-deferred)
  (setq fill-column 99)
  ;;(add-hook 'before-save-hook 'lsp-format-buffer)
  (setq gofmt-command "gofumports")
  (company-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-mode
  :ensure t
  :config
  (progn
    (add-hook 'go-mode-hook #'setup-go-mode)))

(add-to-list 'safe-local-variable-values '(gofmt-command . "gofumports"))
(add-to-list 'safe-local-variable-values '(gofmt-command . "gofmt"))
(add-to-list 'safe-local-variable-values '(gofmt-command . "goimports"))

(provide 'setup-go)
