;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode

;; Enable parsing of pyright errors in compilation mode
;; from https://robbmann.io/posts/006_emacs_2_python/
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(ty "^[[:blank:]]+--> \\(.*\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'ty))


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

;;; Code:
(defun my/eglot-workspace-config (server)
  (let* ((show? (gethash (file-truename default-directory) show-pyright-errors 't))
         (config `(:python.analysis (:ignore ,(if show? [] ["**"])))))
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
