(unless (require-try 'python-mode)
  (require 'python))


(add-hook 'python-mode-hook
	  'schmir-python-hook)

(add-to-list 'auto-mode-alist
	     '("\\wscript$\\|\\SConstruct$\\|\\SConscript$\\|\\.py$\\|\\.jy$\\|\\.py\\.cov$" . python-mode))

(loop for i in '("jython" "pypy" "python" "python2" "python2.4" "python2.5" "python2.6" "python2.7" "python3" "python3.0" "python3.1" "python3.2" "python3.3")
      do (add-to-list 'interpreter-mode-alist `(,i . python-mode)))


;; highlight self in python-mode
(font-lock-add-keywords 'python-mode '(("\\<\\(self\\)" 1 font-lock-builtin-face)))

(setq py-XXX-tag-face font-lock-warning-face)

(if (fboundp 'cython-mode)
    (add-to-list 'auto-mode-alist '("\\.\\(pyx\\|pxi\\|pxd\\)$" . cython-mode)))

(provide 'setup-python)

