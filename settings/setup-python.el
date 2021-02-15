(use-package python-pytest :defer t)
(use-package zimports)
(use-package anaconda-mode :defer t)
(use-package company-anaconda :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-anaconda)))

(use-package blacken :defer t
  :config
  (setq blacken-only-if-project-is-blackened t))

(defun schmir/anaconda-eldoc-unless-tramp ()
  (if (tramp-tramp-file-p (buffer-file-name))
      (message "tramp file, disabling anaconda-eldoc-mode")
    (anaconda-eldoc-mode t)))

;; pythonic is used by anaconda-mode
(use-package pythonic :defer t
  :init
  (setq pythonic-interpreter "python3"))

(use-package python :defer t
  :init
  (setq python-shell-interpreter "python3")
  :config
  (progn
    (advice-add 'run-python :around #'with-project-root-as-default-directory)
    (add-hook 'python-mode-hook #'blacken-mode)
    (add-hook 'python-mode-hook #'anaconda-mode)
    (add-hook 'python-mode-hook #'schmir/anaconda-eldoc-unless-tramp))
  :bind (
         :map python-mode-map
         ("C-c b" . #'blacken-buffer)))

(provide 'setup-python)
