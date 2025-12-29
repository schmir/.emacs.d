;;; setup-languages.el --- Programming language configurations   -*- lexical-binding: t -*-

;; Various packages without additional configuration
(setup (:package cargo elixir-mode flymake-shellcheck htmlize just-mode lua-mode
                 ninja-mode solidity-flycheck terraform-mode yaml-mode))

;; flycheck: On-the-fly syntax checking with inline error display
(setup (:package flycheck flycheck-inline flycheck-package)
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (global-flycheck-inline-mode))

;; markdown-mode: Editing and previewing markdown files
(setup (:package markdown-mode markdown-preview-mode)
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist (cons "README\\.md\\'" #'gfm-mode)))

;; adoc-mode: Editing AsciiDoc files
(setup (:package adoc-mode)
  (:match-file "\\.adoc$"))

;; js-mode: JavaScript with eglot, eslint, and treesit support
(setup js-mode
  (:package add-node-modules-path flymake-eslint)
  (:with-mode (js-mode js-ts-mode)
    (:hook #'my/setup-eglot-flymake-backend
           #'flymake-mode
           #'eglot-ensure
           #'add-node-modules-path
           #'flymake-eslint-enable))
  (:option
   ;; If we don't defer the binary check, the hook will fail and dir-local.el variables will not
   ;; work.
   flymake-eslint-defer-binary-check t)

  (require 'treesit)
  (when (treesit-ready-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))))

;; eglot: LSP client with booster for improved performance
(setup (:package eglot
                 (eglot-booster :url "https://github.com/jdtsmith/eglot-booster.git"))
  (defun my/eglot-rename (newname)
    "Rename the current symbol to NEWNAME. like eglot-rename but provides the old symbol as default."
    (interactive
     (list (read-from-minibuffer
            (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                           "unknown symbol"))
            (thing-at-point 'symbol t) nil nil nil
            (symbol-name (symbol-at-point)))))
    (eglot-rename newname))

  (defun my/setup-eglot-flymake-backend ()
    "Enable eglot's flymake backend manually."
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))

  (when (executable-find "taplo")
    (add-hook 'conf-toml-mode-hook #'eglot-ensure)
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(conf-toml-mode . ("taplo" "lsp" "stdio")))))

  (:option eglot-autoshutdown t
           eglot-events-buffer-config '(:size 0 :format short) ;; didn't ever look at the log
           eglot-extend-to-xref t
           eglot-report-progress nil
           eglot-sync-connect 0)

  (with-eval-after-load 'eglot
    (when (executable-find "emacs-lsp-booster")
      (eglot-booster-mode))
    ;; let me manage flymake on my own
    (add-to-list 'eglot-stay-out-of 'flymake))

  (:bind  "C-c ." #'xref-find-references
          "C-c t" #'eglot-find-typeDefinition
          "C-c i" #'eglot-find-implementation
          "C-c r" #'my/eglot-rename))

;; protobuf-mode: Editing protocol buffer files
(setup (:package protobuf-mode)
  (:hook #'setup-protobuf)

  (defconst my-protobuf-style
    '((c-basic-offset . 8)
      (indent-tabs-mode . nil)))

  (defun setup-protobuf ()
    (c-add-style "my-style" my-protobuf-style t)))

;; solidity-mode: Ethereum smart contract development
(setup (:package solidity-mode company-solidity)
  (defun schmir/solidity-setup ()
    ;; https://stackoverflow.com/questions/6952369/java-mode-argument-indenting-in-emacs
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       (cape-company-to-capf #'company-solidity)
                       #'cape-dabbrev)))
    (c-set-offset 'arglist-intro '+)
    (setq-local c-basic-offset 4
                tab-width 8))
  (:hook #'schmir/solidity-setup)
  (with-eval-after-load 'solidity-mode
    (require 'company-solidity)
    (require 'cape)))

;; sh-mode: Shell scripts with shellcheck linting
(setup sh-mode
  (:hook #'flymake-shellcheck-load #'flymake-mode))

;; nix-mode: Nix expressions with eglot and treesit support
(setup (:package nix-mode nix-ts-mode)
  (:with-mode (nix-mode nix-ts-mode)
    (:hook #'eglot-ensure))
  (:match-file  "\\.nix\\'")
  (when (treesit-ready-p 'nix)
    (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nixd")))))

;; rust-mode: Rust with eglot and clippy linting
(setup (:package rust-mode flymake-clippy)
  (defun my/setup-rust ()
    (eglot-ensure)
    (require 'flymake-clippy)
    (flymake-clippy-setup-backend)
    (flymake-mode))
  (:hook #'my/setup-rust))

(provide 'setup-languages)

;;; setup-languages.el ends here
