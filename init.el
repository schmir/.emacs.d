;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

(add-to-list 'load-path
	     (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path
	     (expand-file-name "settings" user-emacs-directory))


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
   racer
   rust-mode
   solidity-flycheck
   prettier-js
   tide
   tldr
   yaml-mode))

(dolist (pkg schmir/packages)
  (straight-use-package pkg))

(use-package diminish :defer t)

(use-package aggressive-indent :defer t
  :diminish aggressive-indent-mode)

(use-package easy-kill :defer t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; place cursor on same buffer position between editing sessions
(use-package saveplace
  :init
  (setq-default save-place-file (expand-file-name "places" user-emacs-directory))
  :config
  (save-place-mode))

(use-package shell-pop
  :bind ("C-t" . #'shell-pop))

(use-package git-messenger
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.8
        company-minimum-prefix-length 0))

(use-package persistent-scratch :demand t
  :config
  (persistent-scratch-setup-default))

(use-package which-key :demand t
  :diminish which-key-mode
  :config
  (which-key-mode))

(require 'setup-core)
(require 'setup-cwc)
(require 'setup-smartparens)

(use-package projectile :demand t
  :diminish projectile-mode
  :init
  (setq-default projectile-completion-system 'default)

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  :bind
  (("<f9>" . #'projectile-compile-project)))

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

(use-package git-gutter :defer t
  :ensure t
  :init (global-git-gutter-mode +1)
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

(use-package deft :defer t
  :init
  (setq deft-default-extension "org"
        deft-extensions '("org" "md" "txt")
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

(with-eval-after-load 'rust-mode
  (require 'racer)
  (require 'flycheck)
  (define-key rust-mode-map (kbd "C-c b") 'rust-format-buffer)
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH

  (setq racer-rust-src-path (expand-file-name "~/vendor/rust/src")) ;; Rust source code PATH

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; --- setup typescript
(setq tide-completion-detailed 't
      tide-always-show-documentation 't)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :interpreter "node")


(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package eglot :defer t
  :bind (
         :map eglot-mode-map
         ("C-c ." . #'xref-find-references)
         ("C-c t" . #'eglot-find-typeDefinition)
         ("C-c r" . #'eglot-rename)
         ("C-c h" . #'eldoc)))

(require 'setup-clojure)
(require 'setup-go)
(require 'setup-python)

(use-package protobuf-mode
  :defer t
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

(use-package solidity-mode
  :defer t
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
  :defer t
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  :bind (:map terraform-mode-map
              ("C-c b" . #'terraform-format-buffer)))

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(use-package hippie-exp
  :init
  (setq hippie-expand-try-functions-list
        '(try-complete-abbrev
          try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
  :bind (("<C-tab>" . #'hippie-expand)))

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

(use-package magit :defer t
  :bind ("C-c s" . #'magit-status)
  :config (advice-add 'magit-process-filter :after #'magit-display-ansi-colors))

(use-package compile :defer t
  :init
  ;; scroll, but stop at first error
  (setq compilation-scroll-output 'first-error)
  :config
  ;; colorize compile mode output
  (add-hook 'compilation-filter-hook #'display-ansi-colors))

(use-package writegood-mode :defer t
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

(use-package gcmh :defer t
  :diminish gcmh-mode) ;; early-init.el enables gcmh-mode

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
