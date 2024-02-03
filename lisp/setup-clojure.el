;;; setup-clojure  --- setup clojure with cider     -*- lexical-binding: t -*-

;;; Code:
(require 'use-package)
(use-package clojure-mode-extra-font-locking :defer t)
(use-package flycheck-clj-kondo :defer t)

(use-package clojure-mode
  :defer t
  :config
  (progn
    (require 'clojure-mode-extra-font-locking)
    (require 'flycheck-clj-kondo)
    ;; (add-hook 'clojure-mode-hook #'company-mode)
    (add-hook 'clojure-mode-hook #'flycheck-mode)
    (define-clojure-indent
     (event-handler 'defun))
    (put-clojure-indent 'cond #'schmir/indent-cond))
  :bind (:map clojure-mode-map
              ("<f10>" . #'cider-connect)))


;; fix indentation of cond expressions
;; see https://github.com/clojure-emacs/clojure-mode/issues/337
(defun schmir/indent-cond (indent-point state)
  "Indent cond statement."
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
  "Load buffer and switch to it."
  (interactive)
  ;; (cider-ns-refresh)
  (cider-load-buffer)
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-repl-buffer))

(use-package cider
  :defer t
  :config
  (setq nrepl-hide-special-buffers t
        ;;nrepl-history-file "~/.emacs.d/nrepl-history"
        )
  ;; (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-save-file-on-load t
        cider-repl-result-prefix ""
        cider-repl-use-pretty-printing t
        cider-stacktrace-fill-column nil
        cider-prompt-for-symbol nil
        cider-print-fn 'fipp
        cider-clojure-cli-aliases ":dev"
        cider-print-options '(("print-length" 200)
                              ("print-level" 10)
                              ("width" 200))
        nrepl-buffer-name-show-port t ;; use port in repl buffer name
        cider-auto-select-error-buffer nil)


  :bind
  (
   :map cider-mode-map
   ("<f10>" . #'schmir/cider-load-buffer-in-repl)
   ("H-h" . #'cider-doc)

   :map cider-repl-mode-map
   ("<f10>" . #'cider-switch-to-last-clojure-buffer)
   ("C-c C-w" . #'cider-eval-last-sexp-and-replace)
   ("H-h" . #'cider-doc)

   :map cider-docview-mode-map
   ("<f10>" . #'cider-popup-buffer-quit-function)
   ("H-h" . #'cider-popup-buffer-quit-function)

   :map cider-stacktrace-mode-map
   ("<f10>" . #'cider-popup-buffer-quit-function)))

(provide 'setup-clojure)
;;; setup-clojure ends here
