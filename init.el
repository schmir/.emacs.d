;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

(if (eq 'ns (window-system))
    (x-focus-frame nil))

(if (version< emacs-version "27")
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(add-to-list 'load-path
	     (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path
	     (expand-file-name "settings" user-emacs-directory))

(require 'setup-straight)

;; Help keep ~/.emacs.d clean; see https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :config
  (progn
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (if (file-exists-p custom-file)
        (load custom-file))))

(require 'setup-theme)
(require 'setup-core)

(use-package bbdb)
(use-package boxquote)
(use-package cargo)
(use-package crux)
(use-package dockerfile-mode)
(use-package elixir-mode)
(use-package flymake-shellcheck)
(use-package htmlize)
(use-package leo)
(use-package lua-mode)
(use-package prodigy)
(use-package solidity-flycheck)
(use-package tldr)
(use-package yaml-mode)

(use-package diminish)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode)

(use-package zoom
  :init (zoom-mode)
  :diminish)

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled)))

(use-package flycheck-inline
  :init
  (global-flycheck-inline-mode))

(use-package adoc-mode :ensure t
  :mode "\\.adoc$")

(use-package apheleia
  :straight '(apheleia :host github :repo "raxod502/apheleia")
  :init
  (apheleia-global-mode +1)
  :config
  (setf (alist-get 'blackzim apheleia-formatters)
        '("blackzim"))
  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(conf-toml-mode . prettier))

  :bind ("C-c b" . #'apheleia-format-buffer))

(use-package eldoc
  :straight nil
  :hook ((emacs-lisp-mode clojure-mode) . eldoc-mode))

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(eval
 `(use-package so-long
    ,@(if (version<= "27.1" emacs-version)
          '(:straight nil))
    :config
    (setq so-long-max-lines nil
          so-long-threshold 500)
    :init
    (global-so-long-mode +1)))

(use-package uniquify
  :straight nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 4))


;; apt install libvterm-dev libvterm-bin libtool-bin cmake
;; dnf install libvterm-devel libtool cmake
(use-package vterm
  :init
  (setq vterm-max-scrollback 10000)
  :config
  (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
  (add-hook 'vterm-mode-hook #'compilation-shell-minor-mode)
  :bind (:map vterm-mode-map
              ("C-t" . #'shell-pop)))

(use-package shell-pop
  :bind
  ("C-t" . #'shell-pop)
  :init
  (setq shell-pop-shell-type '("vterm" "*vterm*" #'vterm)
        shell-pop-term-shell "/usr/bin/zsh"
        shell-pop-window-size 40))

(use-package company
  :diminish
  :init
  (setq company-idle-delay 0.8
        company-minimum-prefix-length 0
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t))
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))

(use-package persistent-scratch :demand t
  :config
  (persistent-scratch-setup-default))

(use-package which-key :demand t
  :diminish
  :config
  (which-key-mode))


(use-package projectile :demand t
  :diminish
  :init
  (setq-default projectile-completion-system 'default)

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  :bind
  (("<f9>" . #'projectile-compile-project)))

(when (executable-find "direnv")
  (use-package direnv :demand t
    :config
    (direnv-mode)))

(use-package consult :demand t
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-buffer
   consult-bookmark consult-recent-file consult-xref
   ;; consult--source-file
   ;; consult--source-project-file
   consult--source-bookmark
   :preview-key "M-."))

(use-package marginalia :demand t
  :init
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))

  :config
  (marginalia-mode +1)
  (global-set-key [remap switch-to-buffer] 'consult-buffer))


(use-package vertico
  :init
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


(use-package default-text-scale
  :bind
  ("C--" . #'default-text-scale-decrease)
  ("C-=" . #'default-text-scale-increase))

(use-package highlight-symbol
  :diminish
  :hook
  (prog-mode . highlight-symbol-mode)
  :config
  (defadvice highlight-symbol-count (around turn-off-symbol-counting activate)
    (interactive))
  :bind
  ([(control f3)] . #'highlight-symbol)
  ([f3] . #'highlight-symbol-next)
  ([(shift f3)] . #'highlight-symbol-prev)
  ([(meta f3)] . #'highlight-symbol-query-replace))

(use-package smartscan
  :init
  (add-hook 'prog-mode-hook #'smartscan-mode))

(use-package deft
  :init
  (setq deft-directory (expand-file-name "~/m/deft/")
        deft-default-extension "org"
        deft-extensions '("org" "md" "txt")
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

;; --- setup typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :init
  (setq tide-completion-detailed 't
        tide-always-show-documentation 't)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")

(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

(use-package flymake-eslint
  :hook (js-mode . flymake-eslint-enable)
  :init
  ;; If we don't defer the binary check, the hook will fail and dir-local.el variables will not
  ;; work.
  (setq flymake-eslint-defer-binary-check t))



(eval
 `(use-package eglot
    ,@(if (version< "29.0" emacs-version)
          '(:straight nil))
    :custom
    (eglot-autoshutdown t)
    :bind (:map eglot-mode-map
                ("C-c ." . #'xref-find-references)
                ("C-c t" . #'eglot-find-typeDefinition)
                ("C-c i" . #'eglot-find-implementation)
                ("C-c r" . #'eglot-rename))))

(use-package tree-sitter-langs :demand t)
(use-package tree-sitter
  :demand t
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(direnv-envrc-mode . bash))
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package protobuf-mode
  :config
  (progn
    (defconst my-protobuf-style
      '((c-basic-offset . 8)
        (indent-tabs-mode . nil)))

    (defun setup-protobuf ()
      (c-add-style "my-style" my-protobuf-style t))

    (add-hook 'protobuf-mode-hook #'setup-protobuf)))

(defun with-project-root-as-default-directory
    (orig-fun &rest args)
  "Run orig-fun with default-directory set to (projectile-project-root)"
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (apply orig-fun args)))

(defun schmir/solidity-setup ()
  ;; https://stackoverflow.com/questions/6952369/java-mode-argument-indenting-in-emacs
  (company-mode +1)
  (c-set-offset 'arglist-intro '+)
  (setq c-basic-offset 4)
  (setq tab-width 8))

(use-package company-solidity)
(use-package solidity-mode
  :config
  (progn
    (require 'company-solidity)
    (add-hook 'solidity-mode-hook #'schmir/solidity-setup)))

(use-package sh-script
  :config
  (progn
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
    (add-hook 'sh-mode-hook 'flymake-mode)))

(use-package terraform-mode)
(use-package nix-mode
  :mode "\\.nix\\'")

;; configure tramp before saveplace, because it might use tramp
(require 'tramp)
(setq tramp-default-method "ssh")
;; (customize-set-variable 'tramp-syntax 'simplified)
(add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))

;; saveplace may need the yadm tramp method.
;; place cursor on same buffer position between editing sessions
(use-package saveplace :demand t
  :straight nil
  :config
  (save-place-mode))


(use-package recentf
  :straight nil
  :init
  (progn
    (add-to-list 'recentf-exclude "^/\\(?:ssh\\|yadm\\|su\\|sudo\\)?:")
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))
(use-package compile
  :init
  ;; scroll, but stop at first error
  (setq compilation-scroll-output 'first-error)
  :config
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))

(use-package ninja-mode)

(use-package writegood-mode
  :init
  (progn
    (add-hook 'text-mode-hook #'writegood-mode)
    (add-hook 'markdown-mode-hook #'writegood-mode))
  :bind (("C-c g" . #'writegood-mode)))

(use-package framemove :demand t
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(use-package server :demand t
  :config
  (server-start))

(use-package gcmh
  :diminish) ;; early-init.el enables gcmh-mode

(require 'setup-git)
(require 'setup-cwc)
(require 'setup-smartparens)
(require 'setup-clojure)
(require 'setup-go)
(require 'setup-python)

(autoload 'git-grep "git-grep")
(global-set-key (kbd "<f5>") #'git-grep)

(dolist (mode '(eldoc-mode highlight-changes-mode))
  (diminish mode))

;; --- Configure display-buffer-alist

(setq display-buffer-alist
      '(("\\`\\*e?shell\\|compilation\\|vterm\\|Help\\*\\(?:<[[:digit:]]+>\\)?\\'"
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

(global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)
(global-set-key (kbd "C-c C-q") #'lunaryorn-quit-bottom-side-windows)


;;; init.el ends here
