;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode


;;; Code:
(defun my/eglot-workspace-config (server)
  (let ((config `(:python.analysis (:ignore ["**"]))))
    (when-let ((venv (pet-virtualenv-root)))
      (nconc config (list :python
                          `( :venvPath ,venv
                             :pythonPath ,(pet-executable-find "python") ))))
    config))

(defun setup-eglot-workspace-configuration()
  (setq-default eglot-workspace-configuration #'my/eglot-workspace-config))


(setup (:package (fm-ruff :url "https://github.com/schmir/fm-ruff")))
(setup (:package pet)
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(setup (:package python-pytest))

(setup python
  (:also-load python-pytest)
  (setopt python-shell-interpreter "python3")
  (:with-mode (python-mode python-ts-mode)
    (:hook #'setup-eglot-workspace-configuration  #'fm-ruff-setup #'my/setup-eglot-flymake-backend #'flymake-mode #'eglot-ensure))
  (when (treesit-ready-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(provide 'setup-python)
;;; setup-python.el ends here
