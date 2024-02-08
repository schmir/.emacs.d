;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode


;;; Code:

(use-package python-pytest :defer t)

(use-builtin python
  :defer t
  :custom
  (python-shell-interpreter "python3")
  :config
  (when (fboundp #'eglot-ensure)
    (add-hook 'python-mode-hook #'eglot-ensure)
    (add-hook 'python-ts-mode-hook #'eglot-ensure))
  (when (executable-find "ruff")
    (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
    (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(provide 'setup-python)
;;; setup-python.el ends here
