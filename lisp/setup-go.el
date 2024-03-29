;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

(defun schmir/fix-imports()
  (with-demoted-errors "Error: %s" (call-interactively 'eglot-code-action-organize-imports)))

(defun setup-go-mode ()
  (setq fill-column 99
        gofmt-command "gofumports")
  ;; (company-mode)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook 'schmir/fix-imports nil t)
  (eglot-ensure))

(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook #'setup-go-mode))

(use-builtin go-ts-mode
  :defer t
  :config
  (add-hook 'go-ts-mode-hook #'setup-go-mode))

(dolist (cmd '("gofumports" "gofmt" "goimports"))
  (add-to-list 'safe-local-variable-values `(gofmt-command . ,cmd)))

(provide 'setup-go)
