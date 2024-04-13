;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode


;;; Code:

(setup (:package python-pytest))

(setup python
  (:also-load python-pytest)
  (:option python-shell-interpreter "python3")
  (:with-mode (python-mode python-ts-mode)
    (:hook #'eglot-ensure))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(provide 'setup-python)
;;; setup-python.el ends here
