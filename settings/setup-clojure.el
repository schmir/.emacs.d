(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)

(message "configuring clojure-mode")
(define-clojure-indent
  (event-handler 'defun))

(define-key clojure-mode-map (kbd "<f10>") #'cider-connect)

;; fix indentation of cond expressions
;; see https://github.com/clojure-emacs/clojure-mode/issues/337
(defun schmir/indent-cond (indent-point state)
  (goto-char (elt state 1))
  (let ((pos -1)
        (base-col (current-column)))
    (forward-char 1)
    ;; `forward-sexp' will error if indent-point is after
    ;; the last sexp in the current sexp.
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (clojure-forward-logical-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the
      ;; current sexp, we detect that by catching the
      ;; `scan-error'. In that case, we should return the
      ;; indentation as if there were an extra sexp at point.
      (scan-error (cl-incf pos)))
    (+ base-col (if (evenp pos) 4 2))))
(put-clojure-indent 'cond #'schmir/indent-cond)

(with-eval-after-load 'cider
  (defun schmir/cider-load-buffer-in-repl ()
    (interactive)
    (cider-load-buffer)
    (cider-repl-set-ns (cider-current-ns))
    (cider-switch-to-repl-buffer))

  (define-key cider-mode-map '[f10] #'schmir/cider-load-buffer-in-repl)

  (define-key cider-repl-mode-map '[f10] 'delete-window)
  (define-key cider-repl-mode-map (kbd "C-c C-w") 'cider-eval-last-sexp-and-replace)
  (define-key cider-stacktrace-mode-map '[f10] 'cider-popup-buffer-quit-function)
  (define-key cider-docview-mode-map '[f10] 'cider-popup-buffer-quit-function)
  (define-key cider-docview-mode-map (kbd "H-h") 'cider-popup-buffer-quit-function)
  (define-key cider-mode-map (kbd "H-a") 'helm-cider-apropos)
  (define-key cider-repl-mode-map (kbd "H-a") 'helm-cider-apropos)
  (define-key cider-mode-map (kbd "H-h") 'cider-doc)
  (define-key cider-repl-mode-map (kbd "H-h") 'cider-doc))

(provide 'setup-clojure)
