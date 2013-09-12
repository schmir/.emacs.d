(require 'paredit)

(define-key paredit-mode-map (kbd "<C-right>") nil)
(define-key paredit-mode-map (kbd "<C-left>") nil)
(define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)

(provide 'setup-paredit)
