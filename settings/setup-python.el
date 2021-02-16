;;; setup-python --- setup python

;;; Commentary:
;;
;; Configure python mode with eglot

(require 'use-package)

;;; Code:

(use-package python-pytest :defer t)
(use-package zimports :defer t)


(use-package blacken :defer t
  :config
  (setq blacken-only-if-project-is-blackened t))

(use-package python :defer t
  :init
  (setq python-shell-interpreter "python3")
  :config
  (progn
    (advice-add 'run-python :around #'with-project-root-as-default-directory)
    (add-hook 'python-mode-hook #'blacken-mode)
    (add-hook 'python-mode-hook #'eglot-ensure))
  :bind
  (:map python-mode-map
        ("C-c b" . #'blacken-buffer)))

(provide 'setup-python)
;;; setup-python.el ends here
