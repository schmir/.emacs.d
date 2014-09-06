;;; setup-clojure -- Summary
;;; Commentary:

(require 'cider)
(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'nrepl-client)
(require 'ac-cider)

;;; Code:
;; fix indentation of cond expressions
(put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4))


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


(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc)

(provide 'setup-clojure)
;;; setup-clojure.el ends here
