;;; setup-smartparens.el --- setup smartparens     -*- lexical-binding: t -*-

;; smartparens: Structured editing (replaced by puni)
(setup (:package smartparens)
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-strict-mode)
  (:bind  "<M-right>"    #'sp-forward-slurp-sexp
          "<M-left>"     #'sp-forward-barf-sexp
          "<C-S-right>"  #'sp-forward-sexp
          "<C-S-left>"   #'sp-backward-sexp
          "C-S-k"        #'sp-kill-sexp
          "<M-up>"       #'sp-splice-sexp
          "H-s"          #'sp-splice-sexp
          " \e\eh"       #'sp-splice-sexp))

(provide 'setup-smartparens)
