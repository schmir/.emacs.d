(unless (require-try 'python-mode)
  (require 'python))


(defun schmir-python-hook ()
  (interactive)
  (when (require-try 'unicode-symbols)
    (substitute-patterns-with-unicode
     (list
      (cons "\\<\\(lambda\\)\\>" 'lambda))))

  (jedi:setup)
  (eproject-maybe-turn-on)   ;; make sure the eproject-hook has run

  (add-hook 'find-file-hooks 'maybe-untabify 'nil 1)
  (setq py-smart-indentation 1
	indent-tabs-mode nil)
  (modify-syntax-entry ?_ "w") ;; make _ part of words.

  (highlight-symbol-mode 1)

  ;;(highlight-phrase "[Ss]elf" (quote bold))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-]") 'python-mark-block)
  (local-set-key (kbd "C-h n") 'schmir-pyhelp)
  ;; (local-set-key (kbd ",") 'schmir-python-smart-comma)

  (local-set-key [C-S-left]  '(lambda()
				(interactive)
				(shift-region -4)))
  (local-set-key [C-S-right] '(lambda()
				(interactive)
				(shift-region 4)))

  (local-set-key [(tab)] 'smart-tab)

  (if (not (file-remote-p (buffer-file-name)))
      (flymake-mode 1)))


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

(setq python-pep8-options '("--repeat"))

(provide 'setup-python)

