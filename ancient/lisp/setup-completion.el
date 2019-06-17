(require 'auto-complete-config)

(ac-config-default)
(add-to-list 'ac-modes 'message-mode)
(add-to-list 'ac-modes 'cython-mode)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "vendor/auto-complete/dict"))


(provide 'setup-completion)
