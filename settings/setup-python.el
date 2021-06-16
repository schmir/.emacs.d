;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode

(require 'use-package)

;;; Code:

(use-package python-pytest)

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  :config
  (progn
    (advice-add 'run-python :around #'with-project-root-as-default-directory)))

(provide 'setup-python)
;;; setup-python.el ends here
