;;; setup-clojure -- Summary
;;; Commentary:

(require 'cider)
(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'nrepl-client)
(require 'company)

(defun cider--var-choice (var-info)
  var-info)

(setq company-idle-delay 0.8
      company-minimum-prefix-length 2)
;;; Code:
;; fix indentation of cond expressions
(put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4))


(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(defun schmir-clojure-hook ()
  (paredit-mode 1)
  (auto-complete-mode 0)
  (company-mode 1)
  (highlight-symbol-mode 1))

(add-hook 'clojure-mode-hook 'schmir-clojure-hook)

(defun schmir-cider-repl-hook ()
  (auto-complete-mode 0)
  (company-mode 1))

(add-hook 'cider-repl-mode-hook 'schmir-cider-repl-hook)

(setq cider-prompt-save-file-on-load nil)


(defadvice cider-load-current-buffer (after switch-namespace activate compile)
  "switch to namespace"
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-repl-buffer))

(define-key cider-mode-map '[f10] 'cider-load-current-buffer)

(define-key cider-repl-mode-map '[f10] 'delete-window)
(define-key cider-stacktrace-mode-map '[f10] 'cider-popup-buffer-quit-function)
(define-key cider-docview-mode-map '[f10] 'cider-popup-buffer-quit-function)
(define-key cider-docview-mode-map (kbd "H-h") 'cider-popup-buffer-quit-function)






(define-key cider-mode-map (kbd "H-h") 'cider-doc)
(define-key cider-repl-mode-map (kbd "H-h") 'cider-doc)

(provide 'setup-clojure)
;;; setup-clojure.el ends here
