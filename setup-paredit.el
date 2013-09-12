(require 'paredit)

(define-key paredit-mode-map (kbd "<C-right>") nil)
(define-key paredit-mode-map (kbd "<C-left>") nil)
(define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)

(define-key paredit-mode-map (kbd "M-9") 'paredit-wrap-sexp)
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "<C-S-right>") 'forward-sexp)
(define-key paredit-mode-map (kbd "<C-S-left>") 'backward-sexp)

(provide 'setup-paredit)
