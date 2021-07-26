;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

(if (version< emacs-version "27")
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(add-to-list 'load-path
	     (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path
	     (expand-file-name "settings" user-emacs-directory))

;; Help keep ~/.emacs.d clean; see https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :config
  (progn
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (if (file-exists-p custom-file)
        (load custom-file))))

(require 'setup-core)


(setq
 schmir/packages
 '(aggressive-indent
   bbdb
   boxquote
   cargo
   consult
   crux
   dockerfile-mode
   elixir-mode
   flycheck-rust
   flymake-shellcheck
   gitignore-mode
   golden-ratio
   htmlize
   leo
   lua-mode
   markdown-mode
   markdown-preview-mode
   prodigy
   solidity-flycheck
   tldr
   yaml-mode))

(dolist (pkg schmir/packages)
  (straight-use-package pkg))

(use-package diminish)

(use-package apheleia
  :straight '(apheleia :host github :repo "raxod502/apheleia")
  :init
  (apheleia-global-mode +1)
  :config
  (setf (alist-get 'blackzim apheleia-formatters)
        '("blackzim"))
  (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(conf-toml-mode . prettier))

  :bind ("C-c b" . #'apheleia-format-buffer))

(use-package aggressive-indent
  :diminish aggressive-indent-mode)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; place cursor on same buffer position between editing sessions
(use-package saveplace :demand t
  :straight nil
  :config
  (save-place-mode))


(eval
 `(use-package so-long
    ,@(if (version<= "27.1" emacs-version)
          '(:straight nil))
    :config
    (setq so-long-max-lines nil
          so-long-threshold 400)
    :init
    (global-so-long-mode +1)))

(use-package uniquify
  :straight nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 2))

(use-package recentf
  :straight nil
  :init
  (progn
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package shell-pop
  :bind ("C-t" . #'shell-pop))

;; (use-package good-scroll :demand t
;;   :config (good-scroll-mode 1))

(use-package git-messenger
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.8
        company-minimum-prefix-length 0
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t))

(use-package persistent-scratch :demand t
  :config
  (persistent-scratch-setup-default))

(use-package which-key :demand t
  :diminish which-key-mode
  :config
  (which-key-mode))


(use-package projectile :demand t
  :diminish projectile-mode
  :init
  (setq-default projectile-completion-system 'default)

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  :bind
  (("<f9>" . #'projectile-compile-project)))

(use-package direnv :demand t
  :config
  (direnv-mode))

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

(use-package selectrum :demand t
  :config
  (progn
    (selectrum-mode +1)))

(use-package selectrum-prescient :demand t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package ctrlf :demand t
  :config
  (ctrlf-mode +1))

(use-package git-gutter
  :ensure t
  ;; :init (global-git-gutter-mode +1)
  :diminish git-gutter-mode)

(use-package default-text-scale
  :bind
  ("C--" . #'default-text-scale-decrease)
  ("C-=" . #'default-text-scale-increase))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)

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
  (setq deft-default-extension "org"
        deft-extensions '("org" "md" "txt")
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

(use-package racer
  :init
  (progn
    (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
    (setq racer-rust-src-path (expand-file-name "~/vendor/rust/src"))) ;; Rust source code PATH
  )

(use-package rust-mode
  :config
  (progn
    (require 'racer)
    (require 'flycheck)
    (define-key rust-mode-map (kbd "C-c b") 'rust-format-buffer)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'rust-mode-hook #'flycheck-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))


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



(use-package lsp-mode
  :init
  (setq schmir/lsp-settings nil)  ;; this is meant to be set via .dir-locals.el
  (setq lsp-keymap-prefix "C-c l"
        lsp-eldoc-render-all t
        lsp-before-save-edits t
        lsp-enable-imenu t
        lsp-idle-delay 0.1
        lsp-headerline-breadcrumb-enable nil)
  (add-hook
   'hack-local-variables-hook
   (lambda ()
     (when (derived-mode-p 'go-mode)
       (require 'lsp)
       (message "lsp-register-custom-settings: %s" schmir/lsp-settings)
       (lsp-register-custom-settings schmir/lsp-settings)
       (lsp-register-custom-settings
        '(("gopls.experimentalWorkspaceModule" t t)))
       (lsp))))
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.gofumpt" t t)))
  (setq lsp-enable-file-watchers nil)
  :bind (:map lsp-mode-map
              ("C-c ." . #'lsp-find-references)
              ("C-c t" . #'lsp-find-type-definition)
              ("C-c i" . #'lsp-find-implementation)
              ("C-c r" . #'lsp-rename))
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)


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

(defun schmir/shfmt-buffer ()
  (interactive)
  (save-excursion
    (save-buffer)
    (shell-command (format "shfmt -w -i 2 %s" (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(use-package sh-script
  :config
  (progn
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
    (add-hook 'sh-mode-hook 'flymake-mode))
  :bind (:map sh-mode-map
              ("C-c b" . #'schmir/shfmt-buffer)))

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  :bind (:map terraform-mode-map
              ("C-c b" . #'terraform-format-buffer)))

;; colorize pre-commit output
;; (require 'ansi-color)
(defun display-ansi-colors
    ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun magit-display-ansi-colors
    (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (display-ansi-colors)))

(use-package magit
  :bind ("C-c s" . #'magit-status)
  :config (advice-add 'magit-process-filter :after #'magit-display-ansi-colors))

(use-package compile
  :init
  ;; scroll, but stop at first error
  (setq compilation-scroll-output 'first-error)
  :config
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))

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
  :diminish gcmh-mode) ;; early-init.el enables gcmh-mode

(require 'setup-cwc)
(require 'setup-smartparens)
(require 'setup-clojure)
(require 'setup-go)
(require 'setup-python)

;;; Configure emacs lisp mode
(defun schmir/elisp-hook ()
  (aggressive-indent-mode 1)
  (eldoc-mode 1)
  (company-mode 1))
(add-hook 'emacs-lisp-mode-hook #'schmir/elisp-hook)

(autoload 'git-grep "git-grep")
(global-set-key (kbd "<f5>") #'git-grep)

(dolist (mode '(eldoc-mode highlight-changes-mode))
  (diminish mode))
;;; init.el ends here
