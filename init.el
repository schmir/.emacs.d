;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

;; focus initial frame, need this for macos
(if (eq 'ns (window-system))
    (x-focus-frame nil))
(add-hook 'after-make-frame-functions #'x-focus-frame)

(let ((minver "29.0"))
  (when (version< emacs-version minver)
    (error "init.el: Emacs too old -- this config requires at least v%s" minver)))

;;; Initialize package.el
(progn
  ;; make sure to set this before we call (package-initialize). Otherwise site-lisp will bail out
  ;; with an error when we try to set the value.
  (setopt site-lisp-directory (expand-file-name "lisp" user-emacs-directory))
  (setopt package-user-dir (expand-file-name "var/elpa-packages" user-emacs-directory)
          package-gnupghome-dir (expand-file-name "var/elpa-gnupg" user-emacs-directory)
          package-archives
          '(("melpa" . "https://melpa.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize))

;;; Install setup.el
(progn
  (load-file (expand-file-name "sup.el" site-lisp-directory))
  (sup-package-install 'setup)
  (require 'setup)

  (setup-define :package
    (lambda (package)
      `(sup-package-install ',package))
    :documentation "Install PACKAGE if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
    :repeatable t
    :shorthand (lambda (form)
                 (let ((pkg-desc (cadr form)))
                   (if (consp pkg-desc)
                       (car pkg-desc)
                     pkg-desc)))))

(setup (:package no-littering)
  (require 'no-littering)
  (no-littering-theme-backups))

(setup (:package site-lisp)
  (site-lisp-initialise))

;; let's keep use-package as it's useful when trying out package, so we can copy and paste the
;; install instructions.
(setup (:package use-package)
  ;; Let imenu see `use-package' declarations
  (setopt use-package-enable-imenu-support t
          use-package-always-ensure t)
  (require 'use-package-ensure))

(setup (:package leuven-theme gruvbox-theme spacemacs-theme ef-themes zenburn-theme
                 anti-zenburn-theme kaolin-themes zerodark-theme)
  ;; Consider all themes safe to load
  (setq custom-safe-themes t))

(setup emacs
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (message "init.el: loading custom file %s" custom-file)
    (load custom-file))
  ;; load a theme unless we have customized one
  (when (not custom-enabled-themes)
    (message "setup-theme.el: loading default theme")
    (my/load-theme 'ef-day)))

(require 'setup-core)
(let* ((home "/home")
       (truename (file-truename home)))
  (when (not (string= home truename))
    (add-to-list 'directory-abbrev-alist (cons (concat "\\`" truename)  home))))

(setup (:package exec-path-from-shell)
  (require 'exec-path-from-shell)
  (dolist (var '("DICPATH" "XDG_DATA_DIRS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (dolist (var '("DIRENV_DIFF" "DIRENV_DIR" "DIRENV_FILE" "DIRENV_WATCHES"))
    (setenv var))
  (exec-path-from-shell-initialize))

(setup (:package boxquote cargo crux dockerfile-mode elixir-mode flymake-shellcheck htmlize leo lua-mode
                 prodigy solidity-flycheck tldr yaml-mode just-mode package-lint))

(setup (:package markdown-mode markdown-preview-mode)
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist (cons "README\\.md\\'" #'gfm-mode)))

(setup (:package zoom)
  (zoom-mode))

(setup (:package flycheck flycheck-inline flycheck-package)
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (global-flycheck-inline-mode))

(setup (:package adoc-mode)
  (:file-match "\\.adoc$"))

(setup (:package apheleia)
  (apheleia-global-mode +1)
  (with-eval-after-load 'apheleia
    ;; apheleia currently does not configure a formatter for nix-ts-mode
    ;; see https://github.com/radian-software/apheleia/issues/298
    (setq apheleia-remote-algorithm 'local)
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))
    (when (executable-find "zprint")
      (setf (alist-get 'zprint apheleia-formatters) '("zprint"))
      (add-to-list 'apheleia-mode-alist '(clojure-mode . zprint))
      (add-to-list 'apheleia-mode-alist '(clojure-ts-mode . zprint)))

    ;; pretty-format-yaml exits with non-zero exit code even if it succeeds; apheleia reads the
    ;; formatted file from stdout, so be need a bit of shell magic here
    (setf (alist-get 'macisamuele/pretty-format-yaml apheleia-formatters)
          '("bash" "-c" "input=$1; shift; pretty-format-yaml 1>&2 $@ ${input}; cat ${input}" "--" input "--autofix" "--indent" "2" "--offset" "2"))

    (setf (alist-get 'blackzim apheleia-formatters)
          '("blackzim"))

    (setf (alist-get 'latexindent apheleia-formatters)
          '("latexindent" "--logfile=/dev/null" "-y" "defaultIndent: \"    \""))

    (when (executable-find "ruff")
      (add-to-list 'apheleia-mode-alist '(python-mode . (ruff-isort ruff)))
      (add-to-list 'apheleia-mode-alist '(python-ts-mode . (ruff-isort ruff))))
    (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(conf-toml-mode . prettier)))
  (:global "C-c b" #'apheleia-format-buffer))

(setup eldoc
  (:hook-into emacs-lisp-mode clojure-mode clojure-ts-mode))

(setup (:package aggressive-indent)
  (:hook-into emacs-lisp-mode))

(setup (:package prism)
  (my/run-when-display-initialized
   (lambda()
     (message "init.el: initializing prism mode hooks")
     (:hook-into emacs-lisp-mode clojure-mode clojure-ts-mode))))

(setup (:package easy-kill)
  (global-set-key [remap kill-ring-save] #'easy-kill))

(setup (:package macrostep)
  (with-eval-after-load 'lisp-mode
    (define-key emacs-lisp-mode-map (kbd "C-c x") 'macrostep-expand)))

(setup so-long
  (setq so-long-max-lines nil
        so-long-threshold 500)
  (global-so-long-mode +1))

(setup uniquify
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 4))

;; apt install libvterm-dev libvterm-bin libtool-bin cmake
;; dnf install libvterm-devel libtool cmake
(setup (:package vterm)
  (setq vterm-max-scrollback 10000
        vterm-shell (executable-find "zsh"))
  (with-eval-after-load 'vterm
    (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window)))
  (:hook #'compilation-shell-minor-mode)
  (:bind "C-t" #'shell-pop)
  ;; :after shell-pop
  )

(setup (:package shell-pop)
  (:global "C-t"  #'shell-pop)
  (setq shell-pop-shell-type '("vterm" "*vterm*" #'vterm)
        shell-pop-term-shell (executable-find "zsh")
        shell-pop-window-size 40))

(setup (:package eat)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (setq eshell-visual-commands '()))

(setup (:package persistent-scratch)
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode +1)))

(setup (:package which-key)
  (which-key-mode))

(setup (:and (executable-find "direnv") (:package envrc))
  (require 'envrc)
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)
  (add-hook 'after-init-hook #'envrc-global-mode))

(setup (:package consult)
  (:package consult-project-extra)
  (global-set-key [remap project-find-file] #'consult-project-extra-find)
  (with-eval-after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-buffer
     consult-bookmark consult-recent-file consult-xref
     ;; consult--source-file
     ;; consult--source-project-file
     consult--source-bookmark
     :preview-key "M-.")))

(setup (:package consult-dir)
  (:global "C-x C-d" consult-dir)
  (with-eval-after-load 'vertico
    (keymap-set vertico-map "C-x C-d" #'consult-dir)
    (keymap-set vertico-map "C-x C-j" #'consult-dir-jump-file))

  (defvar my/consult-dir-source-zoxide
    `(:name "Zoxide"
            :narrow ?z
            :category file
            :face consult-file
            :enabled ,(lambda () (fboundp #'zoxide-query))
            :items ,(lambda()
                      (delete-dups (mapcar #'abbreviate-file-name (zoxide-query)))))
    "Zoxide directory source for `consult-dir--pick'.")

  (with-eval-after-load 'consult-dir
    (add-to-list 'consult-dir-sources #'my/consult-dir-source-zoxide)))

(setup eshell
  (defun my/insert-compile-command
      ()
    (interactive)
    (goto-char (max-char))
    (insert compile-command))

  (defvar my/consult-dir-source-eshell
    `(:name "Eshell"
            :narrow ?e
            :category file
            :face consult-file
            :enabled ,(lambda () (and (boundp 'eshell-last-dir-ring)
                                      eshell-last-dir-ring))
            :items ,(lambda()
                      (delete-dups
                       (mapcar 'abbreviate-file-name
                               (ring-elements eshell-last-dir-ring)))))
    "Eshell directory source for `consult-dir--pick'.")

  (with-eval-after-load 'consult-dir
    (add-to-list 'consult-dir-sources my/consult-dir-source-eshell))

  (defun my/zoxide-eshell-directory-changed
      ()
    (zoxide-add default-directory))

  (with-eval-after-load 'eshell
    (require 'esh-mode)

    (add-hook 'eshell-mode-hook #'my/zoxide-eshell-directory-changed)
    (add-hook 'eshell-mode-hook #'hack-dir-local-variables-non-file-buffer)
    (define-key eshell-mode-map (kbd "<f9>") #'my/insert-compile-command)
    (add-hook 'eshell-directory-change-hook #'my/zoxide-eshell-directory-changed)
    (add-hook 'eshell-directory-change-hook #'hack-dir-local-variables-non-file-buffer))


  (defun my/zoxide-query
      (s)
    (let ((r (zoxide-run nil "query" s)))
      (if (stringp r)
          (car (split-string r "\n" t))
        nil)))

  (autoload 'consult-dir--pick "consult-dir")
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((dir (if regexp
                   (or (my/zoxide-query regexp)
                       (eshell-find-previous-directory regexp))
                 (substring-no-properties
                  (consult-dir--pick "Switch directory: ")))))
      (eshell/cd dir))))

(setup (:package marginalia)
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  (marginalia-mode +1)
  (global-set-key [remap switch-to-buffer] #'consult-buffer))

(setup (:package  vertico)
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)
  (setq completion-styles '(basic substring))
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(setup (:package  default-text-scale)
  (:global
   "C--"  #'default-text-scale-decrease
   "C-="  #'default-text-scale-increase))

(setup (:package highlight-symbol)
  (:hook-into prog-mode)
  (:option highlight-symbol-occurrence-message '(explicit))
  (:global
   [(control f3)] #'highlight-symbol
   [f3]           #'highlight-symbol-next
   [(shift f3)]   #'highlight-symbol-prev
   [(meta f3)]    #'highlight-symbol-query-replace))

(setup goto-address-mode
  (:hook-into prog-mode))

(setup (:package  pulsar)
  (setq pulsar-pulse t
        pulsar-delay 0.045
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))


(setup (:package smartscan)
  (:hook-into prog-mode-hook))

(setup (:package howm)
  (require 'howm)
  (setq howm-history-file (expand-file-name ".howm-history" howm-directory))
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name))


(setup (:package denote)
  (:global
   "C-c n n"  #'denote
   "C-c n i"  #'denote-link-or-create
   "C-c n I"  #'denote-link
   "C-c n b"  #'denote-link-backlinks
   "C-c n a"  #'denote-add-front-matter
   "C-c n r"  #'denote-rename-file
   "C-c n R"  #'denote-rename-file-using-front-matter)
  (setq denote-directory (expand-file-name "~/m/notes")
        denote-known-keywords '("emacs" "cli" "dev" "linux" "git" "clojure" "python" "golang")))

(setup (:package orderless))

(setup (:package consult-notes)
  (:global "C-c n f" #'my/consult-notes)
  (:option consult-notes-denote-files-function (function denote-directory-text-only-files)
           consult-notes-file-dir-sources
           '(;; ("notes"             ?o "~/m//notes/")
             ("deft"      ?r "~/m/deft/")))
  (defun my/consult-notes ()
    (interactive)
    (let ((completion-styles '(orderless)))
      (consult-notes)))

  ;; (setq consult-notes-file-dir-sources '(("Name"  ?key  "path/to/dir"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-denote-mode))

(setup js-mode
  (:with-mode (js-mode js-ts-mode)
    (:hook #'my/setup-eglot-flymake-backend #'flymake-mode #'eglot-ensure))
  (require 'treesit)
  (when (treesit-ready-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))))

(setup (:package add-node-modules-path)
  (:with-mode (js-mode js2-mode)
    (:hook #'add-node-modules-path)))

(setup (:package flymake-eslint)
  (:with-mode (js-mode js2-mode)
    (:hook #'flymake-eslint-enable))

  (:option
   ;; If we don't defer the binary check, the hook will fail and dir-local.el variables will not
   ;; work.
   flymake-eslint-defer-binary-check t))

(setup (:package (eglot-booster :url "https://github.com/jdtsmith/eglot-booster.git"))
  (require 'eglot-booster))

(setup (:package eglot)
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

  (with-eval-after-load 'eglot
    (when (executable-find "emacs-lsp-booster")
      (eglot-booster-mode))
    (setopt eglot-autoshutdown t)
    ;; let me manage flymake on my own
    (add-to-list 'eglot-stay-out-of 'flymake))

  (:bind  "C-c ." #'xref-find-references
          "C-c t" #'eglot-find-typeDefinition
          "C-c i" #'eglot-find-implementation
          "C-c r" #'my/eglot-rename))

(setup (:package treesit-auto)
  (:option treesit-auto-langs '(python go gomod bash yaml)
           treesit-auto-install 'prompt)
  (require 'treesit-auto)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

(setup sh-mode
  (:hook #'flymake-shellcheck-load #'flymake-mode))

(setup (:package terraform-mode))

(setup (:package nix-mode)
  (:file-match  "\\.nix\\'")
  (:hook #'eglot-ensure)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nixd")))))

(setup (:package nix-ts-mode)
  (:hook #'eglot-ensure)
  (when (treesit-ready-p 'nix)
    (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))))

(setup (:package flymake-clippy))

(setup (:package rust-mode)
  (defun my/setup-rust ()
    (eglot-ensure)
    (require 'flymake-clippy)
    (flymake-clippy-setup-backend)
    (flymake-mode))
  (:hook #'my/setup-rust))

;; configure tramp before saveplace, because it might use tramp
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

;; saveplace may need the yadm tramp method.
;; place cursor on same buffer position between editing sessions
(setup saveplace
  (save-place-mode))

(setup recentf
  (:option recentf-max-saved-items 200)
  (recentf-mode t)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|yadm\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setup savehist
  (setq history-length 10000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(setup (:package super-save)
  (super-save-mode +1))

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

(setup project
  (setopt project-vc-extra-root-markers '(".project" ".projectile")
          project-vc-merge-submodules nil)
  (:global "<f9>" #'project-compile
           "<S-f9>" #'project-eshell))

(setup (:and (executable-find "zoxide")
             (:package zoxide))
  (defun my/zoxide-add
      ()
    (zoxide-add)
    (when-let ((proj (project-current))
               (root (project-root proj))
               (path (expand-file-name root)))
      (message "add project-root %s to zoxide" path)
      (zoxide-add path)))

  (add-hook 'find-file-hook #'my/zoxide-add))

(setup compile
  ;; scroll, but stop at first error
  (setq compilation-scroll-output 'first-error)
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))

(setup (:package ninja-mode))


(setup (:package (framemove :url "https://github.com/emacsmirror/framemove"))
  (require 'framemove)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(setup server
  (server-start))

(setup (:package  gcmh)) ;; early-init.el enables gcmh-mode

(setup (:package pdf-tools)
  (add-hook 'doc-view-mode-hook #'pdf-tools-install))

(require 'setup-mail)
(require 'setup-completion)
(require 'setup-git)
(require 'setup-clojure)
(require 'setup-go)
(require 'setup-python)

;; (require 'setup-smartparens)
(setup (:package puni)
  (electric-pair-mode +1)
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)

  (keymap-global-set "M-<right>"   #'puni-slurp-forward)
  (keymap-global-set "M-<left>"    #'puni-barf-forward)
  (keymap-global-set "M-<up>"      #'puni-splice)
  (keymap-global-set "C-S-<right>" #'puni-forward-sexp)
  (keymap-global-set "C-S-<left>"  #'puni-backward-sexp))

(setup (:package eros)
  (eros-mode +1))

(setup cwc
  (my/run-when-display-initialized
   (lambda()
     (message "init.el: initializing cwc-global-mode")
     (cwc-global-mode +1))))

(setup git-grep
  (:global "<f5>" #'git-grep))


;; --- Configure display-buffer-alist
(setup emacs
  (setq display-buffer-alist
        '(("e?shell*\\|ielm\\|eat\\|compilation\\|vterm\\|Help\\*\\(?:<[[:digit:]]+>\\)?\\'"
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (reusable-frames . visible)
           (side . bottom)
           (window-height . 0.4))
          ("\\`\\*cider-repl\\|.*.clj"
           (display-buffer-reuse-window
            display-buffer-pop-up-window)
           (reusable-frames . t)
           (inhibit-switch-frames . nil))))

  (defun lunaryorn-quit-bottom-side-windows ()
    "Quit side windows of the current frame."
    (interactive)
    (dolist (window (window-at-side-list))
      (quit-window nil window)))

  (:global "C-c q" #'lunaryorn-quit-bottom-side-windows
           "C-c C-q" #'lunaryorn-quit-bottom-side-windows))


;;; init.el ends here
