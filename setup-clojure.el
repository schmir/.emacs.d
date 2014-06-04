;;; setup-clojure -- Summary
;;; Commentary:

;; (require 'cider)
;; (require 'clojure-mode)
;; (require 'nrepl-client)

;;; Code:
;; fix indentation of cond expressions
(put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4))

(setq nrepl-init-code
"(do
    (set! *print-length* 150)
    (set! *print-level* 20))
")

(defun schmir-setup-nrepl ()
  (interactive)
  (insert nrepl-init-code)
  (cider-repl-return))
(add-hook 'nrepl-connected-hook 'schmir-setup-nrepl)

(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(defun schmir-clojure-hook ()
  (paredit-mode 1)
  (highlight-symbol-mode 1))

(add-hook 'clojure-mode-hook 'schmir-clojure-hook)


(setq cider-prompt-save-file-on-load nil)


(defadvice cider-load-current-buffer (after switch-namespace activate compile)
  "switch to namespace"
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-repl-buffer))

(define-key cider-mode-map '[f10] 'cider-load-current-buffer)

(provide 'setup-clojure)
;;; setup-clojure.el ends here
