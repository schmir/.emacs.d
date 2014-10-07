
(require 'ace-jump-mode)

(define-key global-map (kbd "H-SPC") 'ace-jump-mode)
(setq ace-jump-word-mode-use-query-char nil)

(provide 'setup-ace-jump)
