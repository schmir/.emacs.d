(require 'cc-vars)

(defun schmir-setup-c-mode-common ()
  (local-set-key [(tab)] 'smart-tab)
  (highlight-symbol-mode 1)
  (setq indent-tabs-mode nil
	c-hungry-delete-key t)
  (make-local-variable 'local-write-file-hooks))

;; *.h files are C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'c-default-style '(c-mode . "python-new"))
(add-to-list 'c-default-style '(c++-mode . "python-new"))
(add-hook 'c-mode-common-hook
	  'schmir-setup-c-mode-common)
(c-add-style
 "python-new"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 4)
   (c-offsets-alist  . ((substatement-open . 0)
			(inextern-lang . 0)
			(arglist-intro . +)
			(case-label . +)
			(innamespace . 0)
			(knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
			      (brace-list-intro)
			      (brace-list-close)
			      (brace-entry-open)
			      (substatement-open after)
			      (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "* ")))

(provide 'setup-c-mode)

