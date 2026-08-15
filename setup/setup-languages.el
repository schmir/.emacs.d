;;; setup-languages.el --- Programming language configurations   -*- lexical-binding: t -*-

(require 'treesit)

;; Various packages without additional configuration
(setup (:package cargo elixir-mode flymake-shellcheck just-mode lua-mode
                 ninja-mode terraform-mode))

;; flymake: every checker in this configuration is a flymake backend, so show
;; its diagnostics inline rather than only in the fringe and echo area.
(setup flymake
  (setopt flymake-show-diagnostics-at-end-of-line 'short)

  ;; Wait longer after the last edit before rechecking, so diagnostics do not
  ;; appear mid-word.  This governs the backends flymake runs itself: ruff,
  ;; shellcheck, clj-kondo, clippy, eslint.  Diagnostics that a language
  ;; server pushes still arrive when the server sends them, since
  ;; `eglot-flymake-backend' reports as they are published.  Setting this to
  ;; nil would delay those too, at the cost of only ever checking on save.
  (setopt flymake-no-changes-timeout 2.0))

;; markdown-mode: Editing and previewing markdown files
(setup (:package markdown-mode markdown-preview-mode)
  (setq markdown-command "multimarkdown")

  (if (treesit-ready-p 'markdown)
      (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
    (add-to-list 'auto-mode-alist (cons "README\\.md\\'" #'gfm-mode))))

;; adoc-mode: Editing AsciiDoc files
(setup (:package adoc-mode)
  (:match-file "\\.adoc$"))

;; js-mode: JavaScript with eglot, eslint, and treesit support
(setup js-mode
  (:package add-node-modules-path flymake-eslint)
  (:with-mode (js-mode js-ts-mode)
    (:hook #'my/setup-eglot-flymake-backend
           #'flymake-mode
           #'my/eglot-ensure-when-project
           #'add-node-modules-path
           #'flymake-eslint-enable))
  (:option
   ;; If we don't defer the binary check, the hook will fail and dir-local.el variables will not
   ;; work.
   flymake-eslint-defer-binary-check t)

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

  (defun my/eglot-ensure-when-project ()
    "Enable eglot when in a project

If not in a project, emacs may otherwise hang [1]

[1] https://lists.nongnu.org/archive/html/bug-gnu-emacs/2023-01/msg00423.html
"
    (if (project-current)
        (eglot-ensure)
      (message "not in a project, not starting eglot")))

  (defun my/setup-eglot-flymake-backend ()
    "Enable eglot's flymake backend manually."
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))

  (when (executable-find "taplo")
    (add-hook 'conf-toml-mode-hook #'my/eglot-ensure-when-project)
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

;; consult-eglot: search the workspace symbols the server knows about.
(setup (:package consult-eglot)
  ;; `xref-find-apropos' already means "find symbols matching a pattern across
  ;; the project", so let the server answer it where one is attached.
  (with-eval-after-load 'eglot
    (keymap-set eglot-mode-map "<remap> <xref-find-apropos>"
                #'consult-eglot-symbols)))

;; protobuf-mode: Editing protocol buffer files
(setup (:package protobuf-mode)
  (:hook #'setup-protobuf)

  (defconst my-protobuf-style
    '((c-basic-offset . 8)
      (indent-tabs-mode . nil)))

  (defun setup-protobuf ()
    (c-add-style "my-style" my-protobuf-style t)))

;; sh-mode: Shell scripts with shellcheck linting
(setup sh-mode
  (:hook #'flymake-shellcheck-load #'flymake-mode))

;; nix-mode: Nix expressions with eglot and treesit support
(setup (:package nix-mode nix-ts-mode)
  (:with-mode (nix-mode nix-ts-mode)
    (:hook #'my/eglot-ensure-when-project))
  (:match-file  "\\.nix\\'")
  (when (treesit-ready-p 'nix)
    (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nixd")))))

;; rust-mode: Rust with eglot and clippy linting
(setup (:package rust-mode flymake-clippy)
  (defun my/setup-rust ()
    (my/eglot-ensure-when-project)
    (require 'flymake-clippy)
    (flymake-clippy-setup-backend)
    (flymake-mode))
  (:hook #'my/setup-rust))

;; yaml-ts-mode: tree-sitter YAML editing
(setup yaml-ts-mode
  ;; yaml-ts-mode defines no tree-sitter indentation rules, so it falls
  ;; back to `indent-relative', which always inserts whitespace and moves
  ;; point.  That defeats `tab-always-indent' = complete, because
  ;; `indent-for-tab-command' only reaches its completion branch when
  ;; indenting leaves both point and the buffer unchanged.  Indent only
  ;; when point is within the leading whitespace; otherwise return
  ;; `noindent' so TAB falls through to `completion-at-point'.
  (defun my/yaml-ts-indent-line ()
    (if (<= (current-column) (current-indentation))
        (indent-relative)
      'noindent))
  (defun my/setup-yaml-ts-indent ()
    (setq-local indent-line-function #'my/yaml-ts-indent-line))
  (:hook #'my/setup-yaml-ts-indent))

(provide 'setup-languages)

;;; setup-languages.el ends here
