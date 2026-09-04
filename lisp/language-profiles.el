;;; language-profiles.el --- Install language tooling profiles  -*- lexical-binding: t -*-

;;; Code:

(require 'project)
(require 'treesit)

(defvar my/language-profiles--installed nil
  "Non-nil after all language profiles are installed.")

(defconst my/language-profiles--installers
  '(my/language-profiles--install-emacs-lisp
    my/language-profiles--install-yaml
    my/language-profiles--install-protobuf
    my/language-profiles--install-markdown
    my/language-profiles--install-javascript
    my/language-profiles--install-toml
    my/language-profiles--install-shell
    my/language-profiles--install-nix
    my/language-profiles--install-rust
    my/language-profiles--install-python
    my/language-profiles--install-go
    my/language-profiles--install-clojure)
  "Private installers for the configured language profiles.

Each installer owns one language and touches no other language's hooks,
so this list may be reordered and its entries added or removed freely.
Where a profile needs its own steps to run in a fixed order, it says so
in one place: as sequential code in a `--setup-' function, as an explicit
`add-hook' depth, or as one `my/language-profiles--add-hooks' call.")

(defun my/language-profiles--ensure-eglot-in-project ()
  "Start Eglot only in a project because its non-project search can hang.

See https://lists.nongnu.org/archive/html/bug-gnu-emacs/2023-01/msg00423.html."
  (if (project-current)
      (eglot-ensure)
    (message "not in a project, not starting eglot")))

(defun my/language-profiles--add-eglot-flymake-backend ()
  "Add the Eglot Flymake backend to the current buffer."
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))

(defun my/language-profiles--add-hooks (hooks functions)
  "Add FUNCTIONS to each hook in HOOKS, so that they run in that order.

`add-hook' puts a function of the default depth in front of the ones
already there, so the list is added back to front.

The guarantee covers one call only.  FUNCTIONS run ahead of whatever a
later call adds to the same hook, so do not spread an ordered sequence
across two calls -- keep it in one call, or write it as sequential code."
  (dolist (hook hooks)
    (dolist (function (reverse functions))
      (add-hook hook function))))

(defun my/language-profiles--set-eglot-capf ()
  "Combine Eglot completion with templates in the current buffer."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'tempel-complete)
                    t)))

(defun my/eglot-rename (newname)
  "Rename the current symbol to NEWNAME, using its name as the default."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          (thing-at-point 'symbol t) nil nil nil
          (symbol-name (symbol-at-point)))))
  (eglot-rename newname))

(defun my/language-profiles--install-shared-policy ()
  "Install policy shared by all language profiles."
  ;; This delay governs the backends that Flymake runs: Ruff, ShellCheck,
  ;; clj-kondo, Clippy, and ESLint.  Language server diagnostics arrive when
  ;; the server sends them.  A nil delay would postpone those diagnostics too
  ;; and make Flymake check only when the buffer is saved.
  (setopt flymake-show-diagnostics-at-end-of-line 'short
          flymake-no-changes-timeout 2.0
          eglot-autoshutdown t
          eglot-events-buffer-config '(:size 0 :format short)
          eglot-extend-to-xref t
          eglot-report-progress nil
          eglot-sync-connect 0)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-stay-out-of 'flymake)
    (when (executable-find "emacs-lsp-booster")
      (eglot-booster-mode))
    (keymap-set eglot-mode-map "<remap> <xref-find-apropos>"
                #'consult-eglot-symbols)
    (keymap-set eglot-mode-map "C-c ." #'xref-find-references)
    (keymap-set eglot-mode-map "C-c t" #'eglot-find-typeDefinition)
    (keymap-set eglot-mode-map "C-c i" #'eglot-find-implementation)
    (keymap-set eglot-mode-map "C-c r" #'my/eglot-rename))

  (add-hook 'eglot-managed-mode-hook
            #'my/language-profiles--set-eglot-capf))

(defun my/language-profiles--setup-scratch-buffer ()
  "Use Lisp interaction mode in the initial scratch buffer."
  (when-let* ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (lisp-interaction-mode))))

(defun my/language-profiles--enable-prism (hooks)
  "Enable Prism in HOOKS once a frame exists.

Prism derives its colours from the theme, which a daemon only has after
the first frame is made."
  (my/run-when-display-initialized
   (lambda ()
     (message "language-profiles: initializing prism mode hooks for %S" hooks)
     (my/language-profiles--add-hooks hooks '(prism-mode)))))

(defun my/language-profiles--install-emacs-lisp ()
  "Install the Emacs Lisp language profile."
  (my/language-profiles--add-hooks
   '(emacs-lisp-mode-hook)
   '(eldoc-mode aggressive-indent-mode))
  (my/language-profiles--enable-prism '(emacs-lisp-mode-hook))
  (with-eval-after-load 'lisp-mode
    (keymap-set emacs-lisp-mode-map "C-c x" #'macrostep-expand))
  (eros-mode +1)
  (add-hook 'after-init-hook #'my/language-profiles--setup-scratch-buffer))

(defun my/language-profiles--yaml-indent-line ()
  "Indent YAML only when point is in the leading whitespace."
  (if (<= (current-column) (current-indentation))
      (indent-relative)
    'noindent))

(defun my/language-profiles--setup-yaml ()
  "Use completion instead of indentation after YAML content."
  (setq-local indent-line-function
              #'my/language-profiles--yaml-indent-line))

(defun my/language-profiles--install-yaml ()
  "Install the YAML language profile."
  ;; yaml-ts-mode defines no tree-sitter indentation rules, so it uses
  ;; indent-relative.  That function always inserts whitespace and prevents
  ;; indent-for-tab-command from reaching completion.  Return noindent after
  ;; content so TAB can complete there.
  (add-hook 'yaml-ts-mode-hook #'my/language-profiles--setup-yaml))

(defconst my/language-profiles--protobuf-style
  '((c-basic-offset . 8)
    (indent-tabs-mode . nil))
  "Editing style for Protocol Buffer files.")

(defun my/language-profiles--setup-protobuf ()
  "Apply the Protocol Buffer editing style to the current buffer."
  (c-add-style "my-style" my/language-profiles--protobuf-style t))

(defun my/language-profiles--install-protobuf ()
  "Install the Protocol Buffers language profile."
  (add-hook 'protobuf-mode-hook #'my/language-profiles--setup-protobuf))

(defun my/language-profiles--install-markdown ()
  "Install the Markdown language profile."
  (if (treesit-ready-p 'markdown)
      (add-to-list 'major-mode-remap-alist
                   '(markdown-mode . markdown-ts-mode))
    (add-to-list 'auto-mode-alist (cons "README\\.md\\'" #'gfm-mode)))
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))

(defun my/language-profiles--install-javascript ()
  "Install the JavaScript language profile."
  (my/language-profiles--add-hooks
   '(js-mode-hook js-ts-mode-hook)
   '(my/language-profiles--add-eglot-flymake-backend
     flymake-mode
     my/language-profiles--ensure-eglot-in-project
     add-node-modules-path
     flymake-eslint-enable))
  ;; An immediate binary check prevents directory-local variables from
  ;; supplying the project-specific executable.
  (setopt flymake-eslint-defer-binary-check t)
  (when (treesit-ready-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))))

(defun my/language-profiles--install-toml ()
  "Install the TOML language profile."
  (with-eval-after-load 'apheleia
    (setf (alist-get 'taplo apheleia-formatters) '("taplo" "format" "-"))
    (add-to-list 'apheleia-mode-alist '(conf-toml-mode . taplo)))
  (when (executable-find "taplo")
    (add-hook 'conf-toml-mode-hook
              #'my/language-profiles--ensure-eglot-in-project)
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(conf-toml-mode . ("taplo" "lsp" "stdio"))))))

(defun my/language-profiles--install-shell ()
  "Install the shell language profile."
  (my/language-profiles--add-hooks
   '(sh-mode-hook)
   '(flymake-shellcheck-load flymake-mode))
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))))

(defun my/language-profiles--install-nix ()
  "Install the Nix language profile."
  (my/language-profiles--add-hooks
   '(nix-mode-hook nix-ts-mode-hook)
   '(my/language-profiles--ensure-eglot-in-project))
  (when (treesit-ready-p 'nix)
    (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((nix-mode nix-ts-mode) . ("nixd"))))
  (with-eval-after-load 'apheleia
    ;; Apheleia does not yet map nix-ts-mode to nixfmt.
    ;; Remove this when https://github.com/radian-software/apheleia/issues/298
    ;; is resolved.
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))))

(defun my/language-profiles--setup-rust ()
  "Activate the Rust language profile in the current buffer."
  (my/language-profiles--ensure-eglot-in-project)
  (require 'flymake-clippy)
  (flymake-clippy-setup-backend)
  (flymake-mode))

(defun my/language-profiles--install-rust ()
  "Install the Rust language profile."
  (add-hook 'rust-mode-hook #'my/language-profiles--setup-rust))

;; The Python server is `ty' (see `eglot-server-programs' below).  It reads
;; none of pyright's settings, so the workspace configuration this profile
;; used to send -- `python.analysis.ignore' to hide diagnostics per project,
;; and `python.venvPath'/`python.pythonPath' to point the server at pet's
;; virtualenv -- was discarded by the server and has been removed.  ty's own
;; settings live under `diagnosticMode', `disableLanguageServices',
;; `inlayHints', `autoImport' and `pythonExtension.activeEnvironment', and it
;; has no equivalent of `ignore'.  `pet-mode' still resolves the virtualenv
;; for everything on the Emacs side.

(defun my/language-profiles--setup-python ()
  "Activate the Python language profile in the current buffer."
  (fm-ruff-setup)
  (my/language-profiles--add-eglot-flymake-backend)
  (flymake-mode)
  (my/language-profiles--ensure-eglot-in-project))

(defun my/language-profiles--install-python ()
  "Install the Python language profile."
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
                           1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'pyright)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(ty "^[[:blank:]]+--> \\(.*\\):\\([0-9]+\\):\\([0-9]+\\).*$"
                      1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'ty))

  (add-hook 'python-base-mode-hook #'pet-mode -10)
  (my/language-profiles--add-hooks
   '(python-mode-hook python-ts-mode-hook)
   '(my/language-profiles--setup-python))
  (setopt python-shell-interpreter "python3")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-base-mode :language-id "python")
                   . ("uvx" "ty" "server"))))
  (when (treesit-ready-p 'python)
    (add-to-list 'major-mode-remap-alist
                 '(python-mode . python-ts-mode)))
  (with-eval-after-load 'apheleia
    (when (executable-find "ruff")
      (add-to-list 'apheleia-mode-alist
                   '(python-mode . (ruff-isort ruff)))
      (add-to-list 'apheleia-mode-alist
                   '(python-ts-mode . (ruff-isort ruff)))))
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(defun my/language-profiles--organize-go-imports ()
  "Organize Go imports and demote errors from the code action."
  (with-demoted-errors "Error: %s"
    (call-interactively #'eglot-code-action-organize-imports)))

(defun my/language-profiles--setup-go ()
  "Activate the Go language profile in the current buffer."
  (setq-local fill-column 99
              gofmt-command "gofumports")
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook
            #'my/language-profiles--organize-go-imports nil t)
  (my/language-profiles--add-eglot-flymake-backend)
  (flymake-mode)
  (my/language-profiles--ensure-eglot-in-project))

(defun my/language-profiles--install-go ()
  "Install the Go language profile."
  (my/language-profiles--add-hooks
   '(go-mode-hook go-ts-mode-hook)
   '(my/language-profiles--setup-go))
  (dolist (command '("gofumports" "gofmt" "goimports"))
    (add-to-list 'safe-local-variable-values
                 `(gofmt-command . ,command))))

(defun my/language-profiles--setup-clojure ()
  "Activate the Clojure language profile in the current buffer."
  (flymake-kondor-setup)
  (flymake-mode)
  (when (executable-find "clojure-lsp")
    (my/language-profiles--ensure-eglot-in-project)))

(defun my/language-profiles--install-clojure ()
  "Install the Clojure language profile."
  (my/language-profiles--add-hooks
   '(clojure-mode-hook clojure-ts-mode-hook)
   '(eldoc-mode my/language-profiles--setup-clojure))
  (my/language-profiles--enable-prism
   '(clojure-mode-hook clojure-ts-mode-hook))
  (with-eval-after-load 'apheleia
    (when (executable-find "zprint")
      (setf (alist-get 'zprint apheleia-formatters) '("zprint"))
      (add-to-list 'apheleia-mode-alist '(clojure-mode . zprint))
      (add-to-list 'apheleia-mode-alist '(clojure-ts-mode . zprint)))))

(defun my/language-profiles-install ()
  "Install all language profiles once for this Emacs session.

Shared policy is installed first because every profile builds on the
Eglot and Flymake settings it establishes.  The profiles themselves are
independent of one another; see `my/language-profiles--installers'."
  (unless my/language-profiles--installed
    (my/language-profiles--install-shared-policy)
    (dolist (installer my/language-profiles--installers)
      (funcall installer))
    (setq my/language-profiles--installed t)))

(provide 'language-profiles)

;;; language-profiles.el ends here
