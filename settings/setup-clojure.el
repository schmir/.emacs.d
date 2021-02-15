(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package clojure-mode
  :defer t
  :config
  (progn
    (require 'clojure-mode-extra-font-locking)
    (define-clojure-indent
      (event-handler 'defun))
    (put-clojure-indent 'cond #'schmir/indent-cond))
  :bind (:map clojure-mode-map
              ("<f10>" . #'cider-connect)))


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
    (+ base-col (if (cl-evenp pos) 4 2))))

(defun schmir/cider-load-buffer-in-repl ()
  (interactive)
  (cider-load-buffer)
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-repl-buffer))

(use-package cider
  :defer t
  :bind
  (
   :map cider-mode-map
   ("<f10>" . #'schmir/cider-load-buffer-in-repl)
   ("H-a" . #'helm-cider-apropos)
   ("H-h" . #'cider-doc)

   :map cider-repl-mode-map
   ("<f10>" . #'delete-window)
   ("C-c C-w" . #'cider-eval-last-sexp-and-replace)
   ("H-a" . #'helm-cider-apropos)
   ("H-h" . #'cider-doc)

   :map cider-docview-mode-map
   ("<f10>" . #'cider-popup-buffer-quit-function)
   ("H-h" . #'cider-popup-buffer-quit-function)

   :map cider-stacktrace-mode-map
   ("<f10>" . #'cider-popup-buffer-quit-function)))

(provide 'setup-clojure)
