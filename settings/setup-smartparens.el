(require 'smartparens-config)

(show-smartparens-global-mode +1)

(define-key smartparens-mode-map (kbd "<M-right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "<M-left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "<C-S-right>") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "<C-S-left>") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-S-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "<M-up>") 'sp-splice-sexp)


(define-key sp-keymap (kbd "H-s") 'sp-splice-sexp)

(define-key sp-keymap (kbd "\e\eh") 'sp-splice-sexp)

(smartparens-global-strict-mode)
(provide 'setup-smartparens)
