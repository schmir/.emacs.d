;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

;; focus initial frame, need this for macos
(if (eq 'ns (window-system))
    (x-focus-frame nil))
(add-hook 'after-make-frame-functions #'x-focus-frame)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "init.el: Emacs too old -- this config requires at least v%s" minver)))

;;; Initialize lisp directory
(progn
  (setopt site-lisp-directory (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path site-lisp-directory)
  (require 'setup-lisp-directory)
  (setup-lisp-directory))

(require 'setup-setup)
(require 'setup-ui)
(require 'setup-core)

(let* ((home "/home")
       (truename (file-truename home)))
  (when (not (string= home truename))
    (add-to-list 'directory-abbrev-alist (cons (concat "\\`" truename)  home))))

(defalias 'yes-or-no-p 'y-or-n-p)  ;; y/n is enough

;; some aliases for interactive use with M-x
(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

;; exec-path-from-shell: Inherit environment variables from shell
(setup (:package exec-path-from-shell)
  (require 'exec-path-from-shell)
  (dolist (var '("DICPATH" "XDG_DATA_DIRS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (dolist (var '("DIRENV_DIFF" "DIRENV_DIR" "DIRENV_FILE" "DIRENV_WATCHES"))
    (setenv var))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Various packages without additional configuration
(setup (:package boxquote cargo crux dockerfile-mode elixir-mode flymake-shellcheck htmlize just-mode leo lua-mode
                 ninja-mode package-lint prodigy s solidity-flycheck terraform-mode tldr yaml-mode))

;; markdown-mode: Editing and previewing markdown files
(setup (:package markdown-mode markdown-preview-mode)
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist (cons "README\\.md\\'" #'gfm-mode)))

;; zoom: Auto-resize windows to golden ratio
(setup (:package zoom)
  (zoom-mode))

;; flycheck: On-the-fly syntax checking with inline error display
(setup (:package flycheck flycheck-inline flycheck-package)
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (global-flycheck-inline-mode))

;; adoc-mode: Editing AsciiDoc files
(setup (:package adoc-mode)
  (:match-file "\\.adoc$"))

;; apheleia: Auto-format code on save without moving point
(setup (:package apheleia)
  (apheleia-global-mode +1)
  (keymap-global-set "C-c b" #'apheleia-format-buffer)
  (with-eval-after-load 'apheleia
    ;; apheleia currently does not configure a formatter for nix-ts-mode
    ;; see https://github.com/radian-software/apheleia/issues/298
    (setq apheleia-remote-algorithm 'local)
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))
    (when (executable-find "zprint")
      (setf (alist-get 'zprint apheleia-formatters) '("zprint"))
      (add-to-list 'apheleia-mode-alist '(clojure-mode . zprint))
      (add-to-list 'apheleia-mode-alist '(clojure-ts-mode . zprint)))

    (setf (alist-get 'blackzim apheleia-formatters)
          '("blackzim"))

    (setf (alist-get 'latexindent apheleia-formatters)
          '("latexindent" "--logfile=/dev/null" "-y" "defaultIndent: \"    \""))

    ;; toml
    (progn
      (setf (alist-get 'taplo apheleia-formatters)
            '("taplo" "format" "-"))
      (add-to-list 'apheleia-mode-alist '(conf-toml-mode . taplo)))

    (when (executable-find "ruff")
      (add-to-list 'apheleia-mode-alist '(python-mode . (ruff-isort ruff)))
      (add-to-list 'apheleia-mode-alist '(python-ts-mode . (ruff-isort ruff))))
    (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))))

;; easy-kill: Better kill-ring-save with expandable selection
(setup (:package easy-kill)
  (keymap-global-set "<remap> <kill-ring-save>" #'easy-kill))

;; so-long: Handle files with very long lines gracefully
(setup so-long
  (setq so-long-max-lines nil
        so-long-threshold 500)
  (global-so-long-mode +1))

;; uniquify: Disambiguate buffer names with path prefixes
(setup uniquify
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 4))

;; persistent-scratch: Preserve scratch buffer across sessions
(setup (:package persistent-scratch)
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode +1)))

;; which-key: Show available keybindings in popup
(setup (:package which-key)
  (which-key-mode))

;; envrc: Direnv integration for per-directory environments
(setup (:and (executable-find "direnv") (:package envrc))
  (require 'envrc)
  (keymap-set envrc-mode-map "C-c e" #'envrc-command-map)
  (add-hook 'after-init-hook #'envrc-global-mode))

;; text-scaling: Keybindings for adjusting font size
(setup text-scaling
  (keymap-global-set "C--"  #'text-scale-decrease)
  (keymap-global-set "C-="  #'text-scale-increase)
  (setq global-text-scale-adjust-resizes-frames nil))

;; highlight-symbol: Highlight and navigate symbols with F3
(setup (:package highlight-symbol)
  (:hook-into prog-mode)
  (:option highlight-symbol-occurrence-message '(explicit))
  (keymap-global-set "C-<f3>" #'highlight-symbol)
  (keymap-global-set "<f3>"   #'highlight-symbol-next)
  (keymap-global-set "S-<f3>" #'highlight-symbol-prev)
  (keymap-global-set "M-<f3>" #'highlight-symbol-query-replace))

;; goto-address-mode: Make URLs clickable in code
(setup goto-address-mode
  (:hook-into prog-mode))

;; pulsar: Pulse highlight line after navigation
(setup (:package pulsar)
  (setq pulsar-pulse t
        pulsar-delay 0.045
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))


;; smartscan: Jump between symbols with M-n/M-p
(setup (:package smartscan)
  (:hook-into prog-mode-hook))

;; howm: Personal wiki and note-taking system
(setup (:package howm)
  (require 'howm)
  (setq howm-history-file (expand-file-name ".howm-history" howm-directory))
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name))


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

(defun with-project-root-as-default-directory
    (orig-fun &rest args)
  "Run orig-fun with default-directory set to project's root directory"
  (let* ((root (when (project-current)
                 (project-root (project-current))))
         (default-directory (or root default-directory)))
    (apply orig-fun args)))

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

;; tramp: Remote file editing with yadm support
(setup tramp
  ;; (customize-set-variable 'tramp-syntax 'simplified)
  (setq tramp-default-method "ssh"
        password-cache-expiry (* 90 60))
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))))))

;; saveplace: Restore cursor position when reopening files
;; saveplace may need the yadm tramp method.
;; place cursor on same buffer position between editing sessions
(setup saveplace
  (save-place-mode))

;; recentf: Track recently opened files
(setup recentf
  (:option recentf-max-saved-items 200)
  (recentf-mode t)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|yadm\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; savehist: Persist minibuffer history across sessions
(setup savehist
  (setq history-length 10000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

;; super-save: Auto-save buffers on focus loss
(setup (:package super-save)
  (super-save-mode +1))

;; fix-project-try-vc: Workaround for project.el caching bug
(setup fix-project-try-vc
  (defun my/fix-project-try-vc (orig-fun dir)
    "Advice for project-try-vc.
project-try-fc recursively calls itself with project-vc-extra-root-markers set to nil and wrongly
caches the result of those calls via vc-file-setprop.
"
    (let ((res (cl-letf (((symbol-function 'vc-file-setprop) #'ignore))
                 (funcall orig-fun dir))))
      (when res
        (vc-file-setprop dir 'project-vc res))
      res))
  (with-eval-after-load 'project
    (advice-add 'project-try-vc :around #'my/fix-project-try-vc)))

;; project: Project management with compile and eshell bindings
(setup project
  (setopt project-vc-extra-root-markers '(".project" ".projectile")
          project-vc-merge-submodules nil)
  (keymap-global-set "<f9>" #'project-compile)
  (keymap-global-set "S-<f9>" #'project-eshell))

;; zoxide: Smart directory tracking and jumping
(setup (:and (executable-find "zoxide")
             (:package zoxide))
  (defun my/zoxide-add
      ()
    (zoxide-add)
    (when-let* ((proj (project-current))
                (root (project-root proj))
                (path (expand-file-name root)))
      (message "add project-root %s to zoxide" path)
      (zoxide-add path)))

  (add-hook 'find-file-hook #'my/zoxide-add))

;; compile: Compilation buffer with ANSI colors
(setup compile
  (setq compilation-scroll-output 'first-error)
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))


;; server: Enable emacsclient connections
(setup server
  (server-start))

;; gcmh: Garbage collector magic hack for better performance
(setup (:package gcmh)) ;; early-init.el enables gcmh-mode

;; pdf-tools: Enhanced PDF viewing and annotation
(setup (:package pdf-tools)
  (add-hook 'doc-view-mode-hook #'pdf-tools-install))

(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-lisp)
(require 'setup-go)
(require 'setup-python)
(require 'setup-shell)

;; puni: Structured editing with slurp/barf/splice
(setup (:package puni)
  (electric-pair-mode +1)
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (keymap-global-set "M-<right>"   #'puni-slurp-forward)
  (keymap-global-set "M-<left>"    #'puni-barf-forward)
  (keymap-global-set "M-<up>"      #'puni-splice)
  (keymap-global-set "C-S-<right>" #'puni-forward-sexp)
  (keymap-global-set "C-S-<left>"  #'puni-backward-sexp))

;; cwc: Run whitespace-cleanup only for changed lines
(setup cwc
  (my/run-when-display-initialized
   (lambda()
     (message "init.el: initializing cwc-global-mode")
     (cwc-global-mode +1))))


;; rg: Ripgrep frontend for fast code searching
(setup (:package rg)
  (require 'rg)
  (rg-enable-default-bindings))


;; selected: Keybindings active only when region is selected
(setup (:package selected expreg)
  (selected-global-mode)
  (:with-map selected-keymap
    (:bind "b" #'boxquote-region
           "g" #'selected-off
           "q" #'fill-paragraph
           "c" #'kill-ring-save
           "x" #'kill-region
           "s" #'sort-lines
           "S" #'my/sort-words-in-region
           "<" #'my/shift-left
           "," #'my/shift-left
           ">" #'my/shift-right
           "." #'my/shift-right
           ";" #'comment-dwim
           "l" #'git-link
           (kbd "SPC") #'expreg-expand
           "z" (lambda()
                 (interactive)
                 (let ((deactivate-mark nil))
                   (undo))))))

;; age: Age encryption with passage for auth-source
(setup (:package age
                 (passage :url "https://github.com/anticomputer/passage.el"))
  (setq age-program "rage"
        age-default-identity "~/.passage/identities"
        age-default-recipient '("~/.ssh/age_yubikey.pub"
                                "~/.ssh/age_recovery.pub"))

  (setenv "PINENTRY_PROGRAM" (or (executable-find "pinentry-mac")
                                 (executable-find "pinentry-gnome3")))

  (age-file-enable)
  ;; (auth-source-pass-enable)
  (auth-source-passage-enable)
  (setq auth-source-debug nil))


;;; init.el ends here
