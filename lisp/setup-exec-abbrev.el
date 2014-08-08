(setq exec-abbrev-cmd-file "~/.emacs.d/exec-abbrev-cmd.dat")
(require 'exec-abbrev-cmd)
(exec-abbrev-cmd-mode 1)
(global-set-key (kbd "M-x") 'exec-abbrev-cmd)

(provide 'setup-exec-abbrev)

