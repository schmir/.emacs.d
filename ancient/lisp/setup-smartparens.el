(require 'smartparens-config)

(show-smartparens-global-mode +1)


(define-key sp-keymap (kbd "<M-right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "<M-left>") 'sp-forward-barf-sexp)

(define-key sp-keymap (kbd "<C-S-right>") 'sp-forward-sexp)
(define-key sp-keymap (kbd "<C-S-left>") 'sp-backward-sexp)

(define-key sp-keymap (kbd "H-s") 'sp-splice-sexp)

(define-key sp-keymap (kbd "\e\eh") 'sp-splice-sexp)


(provide 'setup-smartparens)
