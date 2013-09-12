;; fix indentation of cond expressions
(put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4))

(setq nrepl-init-code
"(do
    (set! *print-length* 150)
    (set! *print-level* 20)
    (use 'clojure.pprint)
    (use 'clojure.reflect))
")

(defun schmir-setup-nrepl ()
  (interactive)
  (insert nrepl-init-code)
  (nrepl-return))
(add-hook 'nrepl-connected-hook 'schmir-setup-nrepl)

(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(defun schmir-clojure-hook ()
  (paredit-mode 1)
  (highlight-symbol-mode 1))

(add-hook 'clojure-mode-hook 'schmir-clojure-hook)

(provide 'setup-clojure)
