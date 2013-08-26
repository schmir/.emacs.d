(require 'auto-complete-config)

(ac-config-default)
(add-to-list 'ac-modes 'message-mode)
(add-to-list 'ac-modes 'cython-mode)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "vendor/auto-complete/dict"))


(require 'hippie-exp)
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

(global-set-key (quote [C-tab]) 'hippie-expand)

(provide 'setup-completion)
