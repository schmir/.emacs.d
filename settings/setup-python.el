;;; setup-python --- setup python     -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configure python mode

(require 'use-package)

;;; Code:

(use-package python-pytest)
(use-package zimports)


(use-package blacken
  :config
  (setq blacken-only-if-project-is-blackened t))

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  :config
  (progn
    (advice-add 'run-python :around #'with-project-root-as-default-directory)
    (add-hook 'python-mode-hook #'blacken-mode))
  :bind
  (:map python-mode-map
        ("C-c b" . #'blacken-buffer)))

(provide 'setup-python)
;;; setup-python.el ends here
