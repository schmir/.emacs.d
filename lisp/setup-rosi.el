(require 'rosi)
(modify-coding-system-alist 'file "\\(\\.rsf\\|\\.msg\\)$" 'cp437)

(add-hook 'rosi-mode-hook 'turn-on-highlight-symbol-mode)
(provide 'setup-rosi)
