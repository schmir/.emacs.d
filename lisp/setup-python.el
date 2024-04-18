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
    (:hook #'flymake-ruff-load #'my/setup-eglot-flymake-backend #'flymake-mode #'eglot-ensure))
  (when (treesit-ready-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(provide 'setup-python)
;;; setup-python.el ends here
