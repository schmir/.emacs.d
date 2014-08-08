;; --- lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-hook 'lua-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'smart-tab)
	     (highlight-symbol-mode 1)
	     (setq lua-indent-level 4)))

(provide 'setup-lua)
