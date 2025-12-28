;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode


;;; Code:

;; pyright-compilation: Parse pyright errors in compilation mode
(setup
    ;; from https://robbmann.io/posts/006_emacs_2_python/
    (with-eval-after-load 'compile
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
      (add-to-list 'compilation-error-regexp-alist 'pyright)

      (add-to-list 'compilation-error-regexp-alist-alist
                   '(ty "^[[:blank:]]+--> \\(.*\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
      (add-to-list 'compilation-error-regexp-alist 'ty)))



;; toggle-pyright-errors: Toggle pyright diagnostics visibility
(setup toggle-pyright-errors
  (defvar show-pyright-errors (make-hash-table :test 'equal))

  (defun toggle-pyright-show-errors ()
    "Toggle showing pyright errors"
    (interactive)
    (let ((server (eglot--current-server-or-lose))
          (dir (file-truename (project-root (project-current)))))
      (puthash dir
               (not (gethash dir show-pyright-errors 't))
               show-pyright-errors)
      (eglot-signal-didChangeConfiguration server)))

  (defun my/eglot-workspace-config (server)
    (let* ((show? (gethash (file-truename default-directory) show-pyright-errors 't))
           (config `(:python.analysis (:ignore ,(if show? [] ["**"])))))
      (when-let* ((venv (pet-virtualenv-root)))
        (nconc config (list :python
                            `( :venvPath ,venv
                               :pythonPath ,(pet-executable-find "python") ))))
      config))

  ;; We must set eglot-workspace-configuration as directory local variable. We do this by defining a
  ;; directory class and registering the project root directory with that class.

  (add-to-list 'safe-local-variable-values '(eglot-workspace-configuration . my/eglot-workspace-config))
  (dir-locals-set-class-variables 'use-my/eglot-workspace-config
                                  '((python-base-mode . ((eglot-workspace-configuration . my/eglot-workspace-config)))))
  (defun setup-eglot-workspace-configuration()
    (when-let* ((project (project-current))
                (root (project-root project)))
      (dir-locals-set-directory-class (file-truename root)
                                      'use-my/eglot-workspace-config)))

  (:with-mode (python-mode python-ts-mode)
    (:hook #'setup-eglot-workspace-configuration)))

;; python: Python with eglot, ruff, and pet for virtualenvs
(setup python
  (:package (fm-ruff :url "https://github.com/schmir/fm-ruff")
            pet
            python-pytest)

  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (:also-load python-pytest)
  (setopt python-shell-interpreter "python3")
  (:with-mode (python-mode python-ts-mode)
    (:hook #'fm-ruff-setup
           #'my/setup-eglot-flymake-backend
           #'flymake-mode
           #'eglot-ensure))
  (when (treesit-ready-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(provide 'setup-python)
;;; setup-python.el ends here
