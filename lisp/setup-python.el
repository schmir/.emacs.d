;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode


;;; Code:

(use-package python-pytest :defer t)

(use-package python
  :defer t
  :init
  (setq python-shell-interpreter "python3")
  :config
  (progn
    (advice-add 'run-python :around #'with-project-root-as-default-directory)))

(provide 'setup-python)
;;; setup-python.el ends here
