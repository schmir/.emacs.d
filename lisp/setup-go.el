;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

(setup go-mode
  (defun my/fix-go-imports()
    (with-demoted-errors "Error: %s" (call-interactively #'eglot-code-action-organize-imports)))

  (defun my/setup-go-mode ()
    (setq-local fill-column 99
                gofmt-command "gofumports")
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'my/fix-go-imports nil t)
    (my/setup-eglot-flymake-backend)
    (flymake-mode)
    (eglot-ensure))

  (:with-mode (go-mode go-ts-mode)
    (:hook #'my/setup-go-mode))

  (dolist (cmd '("gofumports" "gofmt" "goimports"))
    (add-to-list 'safe-local-variable-values `(gofmt-command . ,cmd))))

(provide 'setup-go)
