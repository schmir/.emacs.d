;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode


;;; Code:

(setup (:package python-pytest))

(setup python
  (:package flymake-ruff)
  (:also-load python-pytest)
  (setopt python-shell-interpreter "python3")
  (:with-mode (python-mode python-ts-mode)
    (:hook #'eglot-ensure))
  (add-hook 'eglot-managed-mode-hook
            (lambda()
              (when (derived-mode-p 'python-base-mode)
                (flymake-ruff-load))))
  (when (treesit-ready-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(provide 'setup-python)
;;; setup-python.el ends here
