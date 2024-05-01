;;; setup-clojure  --- setup clojure with cider     -*- lexical-binding: t -*-

;;; Code:
(setup (:package clojure-mode clojure-mode-extra-font-locking flymake-kondor)
  (defun my/setup-clojure-mode ()
    (flymake-kondor-setup)
    (flymake-mode)
    (when (executable-find "clojure-lsp")
      (eglot-ensure)))

  (:bind-into clojure-mode-map
    "<f10>"  #'cider-connect)

  (:hook #'my/setup-clojure-mode)

  (with-eval-after-load 'clojure-mode
    (require 'clojure-mode-extra-font-locking)
    (define-clojure-indent
     (event-handler 'defun))
    (put-clojure-indent 'cond #'schmir/indent-cond)))

(setup (:package clojure-ts-mode)
  ;; aggressive-indent doesn't seem to work in clojure-ts-mode as expected. So, for now let's use
  ;; the old clojure mode.
  ;; (when (treesit-ready-p 'clojure)
  ;;   (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))
  (setopt clojure-ts-ensure-grammars nil)
  (:hook #'my/setup-clojure-mode))

(setup (:package cider)
  (:hook #'eldoc-mode)

  (defun schmir/cider-load-buffer-in-repl ()
    "Load buffer and switch to it."
    (interactive)
    ;; (cider-ns-refresh)
    (cider-load-buffer)
    (cider-repl-set-ns (cider-current-ns))
    (cider-switch-to-repl-buffer))

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
        nrepl-hide-special-buffers t
        nrepl-buffer-name-show-port t ;; use port in repl buffer name
        cider-auto-select-error-buffer nil)

  (:bind-into cider-mode-map
    "<f10>" #'schmir/cider-load-buffer-in-repl
    "H-h" #'cider-doc)
  (:bind-into cider-repl-mode-map
    "<f10>" #'cider-switch-to-last-clojure-buffer
    "C-c C-w" #'cider-eval-last-sexp-and-replace
    "H-h"  #'cider-doc)
  (:bind-into cider-docview-mode-map
    "<f10>" #'cider-popup-buffer-quit-function
    "H-h" #'cider-popup-buffer-quit-function)

  (:bind-into cider-stacktrace-mode-map
    "<f10>" #'cider-popup-buffer-quit-function))

(provide 'setup-clojure)
;;; setup-clojure ends here
