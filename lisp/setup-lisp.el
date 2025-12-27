;;; setup-lisp.el --- Lisp-like languages (Elisp, Clojure)  -*- lexical-binding: t -*-

;;; Code:

;;;; Emacs Lisp

;; eldoc: Show function signatures in echo area
(setup eldoc
  (:hook-into emacs-lisp-mode clojure-mode clojure-ts-mode))

;; aggressive-indent: Keep code indented automatically
(setup (:package aggressive-indent)
  (:hook-into emacs-lisp-mode))

;; prism: Colorize code by depth for lisp modes
(setup (:package prism)
  (my/run-when-display-initialized
   (lambda()
     (message "init.el: initializing prism mode hooks")
     (:hook-into emacs-lisp-mode clojure-mode clojure-ts-mode))))

;; macrostep: Interactively expand macros in elisp
(setup (:package macrostep)
  (with-eval-after-load 'lisp-mode
    (keymap-set emacs-lisp-mode-map "C-c x" #'macrostep-expand)))

;; eros: Overlay elisp evaluation results near point
(setup (:package eros)
  (eros-mode +1))

;;;; Clojure

;; clojure-mode/clojure-ts-mode: Clojure editing with eglot and kondor linting
(setup (:package clojure-mode clojure-ts-mode clojure-mode-extra-font-locking flymake-kondor)
  (defun my/setup-clojure-mode ()
    (flymake-kondor-setup)
    (flymake-mode)
    (when (executable-find "clojure-lsp")
      (eglot-ensure)))

  (:with-mode (clojure-mode clojure-ts-mode)
    (:hook #'my/setup-clojure-mode))
  (setopt clojure-ts-ensure-grammars nil)

  ;;; XXX Disable clojure-ts-mode as it seem to be broken currently
  ;; (when (treesit-ready-p 'clojure)
  ;;   (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))

  (with-eval-after-load 'clojure-mode
    (require 'clojure-mode-extra-font-locking)
    (define-clojure-indent
     (event-handler 'defun))
    (put-clojure-indent 'cond #'schmir/indent-cond)))


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

(provide 'setup-lisp)
;;; setup-lisp.el ends here
