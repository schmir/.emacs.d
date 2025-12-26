;;; setup-clojure  --- setup clojure with cider     -*- lexical-binding: t -*-

;;; Code:

;; clojure-mode: Clojure editing with eglot and kondor linting
(setup (:package clojure-mode clojure-mode-extra-font-locking flymake-kondor)
  (defun my/setup-clojure-mode ()
    (flymake-kondor-setup)
    (flymake-mode)
    (when (executable-find "clojure-lsp")
      (eglot-ensure)))

  ;; (:bind "<f10>"  #'cider-connect)

  (:hook #'my/setup-clojure-mode)

  (with-eval-after-load 'clojure-mode
    (require 'clojure-mode-extra-font-locking)
    (define-clojure-indent
     (event-handler 'defun))
    (put-clojure-indent 'cond #'schmir/indent-cond)))

;; clojure-ts-mode: Treesit-based Clojure mode
(setup (:package clojure-ts-mode)
  (when (treesit-ready-p 'clojure)
    (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))
  (setopt clojure-ts-ensure-grammars nil)
  (:hook #'my/setup-clojure-mode))

;; cider: Clojure Interactive Development Environment
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
  (with-eval-after-load 'cider
    (keymap-set cider-mode-map
                "<f10>" #'schmir/cider-load-buffer-in-repl)
    (keymap-set cider-mode-map
                "H-h" #'cider-doc)

    (keymap-set cider-repl-mode-map
                "<f10>" #'cider-switch-to-last-clojure-buffer)
    (keymap-set cider-repl-mode-map
                "C-c C-w" #'cider-eval-last-sexp-and-replace)
    (keymap-set cider-repl-mode-map
                "H-h"  #'cider-doc)
    (keymap-set cider-docview-mode-map
                "<f10>" #'cider-popup-buffer-quit-function)
    (keymap-set cider-docview-mode-map
                "H-h" #'cider-popup-buffer-quit-function)
    (keymap-set cider-stacktrace-mode-map
                "<f10>" #'cider-popup-buffer-quit-function)))

(provide 'setup-clojure)
;;; setup-clojure ends here
