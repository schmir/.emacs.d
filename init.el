;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/


;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")

(add-to-list 'load-path
	     (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path
	     (expand-file-name "settings" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(global-set-key (kbd "<f12>") 'toggle-menu-bar-mode-from-frame)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
;; (require 'cl)

(setq
 schmir/packages
 '(aggressive-indent
   bbdb
   boxquote
   cargo
   ctrlf
   company-solidity
   consult
   crux
   default-text-scale
   dockerfile-mode
   easy-kill
   elixir-mode
   flycheck-rust
   flymake-shellcheck
   gitignore-mode
   golden-ratio
   highlight-symbol
   htmlize
   leo

   lua-mode
   magit
   marginalia
   markdown-mode
   markdown-preview-mode
   persistent-scratch
   projectile
   racer
   rust-mode
   shell-pop
   selectrum
   selectrum-prescient
   smartparens
   smartscan
   ;; smex
   solidity-flycheck
   solidity-mode
   prettier-js
   ;; swiper
   terraform-mode
   tide
   tldr
   which-key
   yaml-mode))

(dolist (pkg schmir/packages)
  (straight-use-package pkg))


(global-set-key [remap kill-ring-save] 'easy-kill)
;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; place cursor on same buffer position between editing sessions
(setq-default save-place-file (expand-file-name "places" user-emacs-directory))

(use-package saveplace
  :config (save-place-mode))


(global-set-key (kbd "C-t") #'shell-pop)
(global-set-key (kbd "C-z") #'undo)

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

(global-hl-line-mode)
(global-auto-revert-mode 1)
(auto-image-file-mode 1)
(column-number-mode 1)

(recentf-mode t)
(setq recentf-max-saved-items 200)

(setq mark-even-if-inactive t)
(transient-mark-mode 1)

(setq line-move-visual nil) ;; what did they think ?

(auto-compression-mode t)

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(setq line-move-visual nil) ;; what did they think ?

(setq change-major-mode-with-file-name t
      create-lockfiles nil
      ;; Filename completion ignores these.
      completion-ignored-extensions (append completion-ignored-extensions
                                            '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx" ".rsm" ".rsr"))
      backward-delete-char-untabify-method 'nil	;; don´t untabify, just delete one char
      font-lock-maximum-decoration t			;; maximum decoration
      next-line-add-newlines nil			;; don´t add newlines when trying to move cursor behind eof
      show-paren-style 'expression
      default-indicate-empty-lines t
      line-number-display-limit-width 100000
      kill-whole-line t				;; make kill-line at beginning of line kill the whole line
      woman-use-own-frame nil				;; don't create new frame for manpages
      vc-handled-backends '(Git Hg)
      vc-follow-symlinks t				;; follow symlinks and don't ask
      enable-recursive-minibuffers t)

(use-package company
  :init
  (setq company-idle-delay 0.8
        company-minimum-prefix-length 0))
;; (add-hook 'after-init-hook 'global-company-mode)

(persistent-scratch-setup-default)
(require 'which-key)
(which-key-mode)

(setq smart-tab-using-hippie-expand 't)


(setq tramp-default-method "ssh")
;; (customize-set-variable 'tramp-syntax 'simplified)

(require 'setup-cwc)
(require 'setup-smartparens)

(require 'projectile)
(setq-default projectile-completion-system 'default)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(marginalia-mode +1)

;; Prefer richer, more heavy, annotations over the lighter default variant.
;; E.g. M-x will show the documentation string additional to the keybinding.
;; By default only the keybinding is shown as annotation.
;; Note that there is the command `marginalia-cycle-annotators` to
;; switch between the annotators.
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))

(global-set-key [remap switch-to-buffer] 'consult-buffer)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
(ctrlf-mode +1)

(require 'git-grep)
(global-set-key (kbd "<f5>") 'git-grep)

(global-set-key (kbd "C--") 'default-text-scale-decrease)
(global-set-key (kbd "C-=") 'default-text-scale-increase)

(progn
  (require 'highlight-symbol)
  (defadvice highlight-symbol-count (around turn-off-symbol-counting activate)
    (interactive))
  (global-set-key [(control f3)] #'highlight-symbol)
  (global-set-key [f3] #'highlight-symbol-next)
  (global-set-key [(shift f3)] #'highlight-symbol-prev)
  (global-set-key [(meta f3)] #'highlight-symbol-query-replace)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode))

(add-hook 'prog-mode-hook #'smartscan-mode)

(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header      
      gnus-init-file (expand-file-name "~/.gnus-init.el"))

;; we substitute sendmail with msmtp if it's installed
(let ((msmtp (executable-find "msmtp")))
  (when msmtp
    (setq sendmail-program msmtp)))

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

;; let me use windmove keybindings even in org-mode
(setq org-replace-disputed-keys t)

(use-package deft
  :defer t
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

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(use-package eglot :defer t
  :bind (
         :map eglot-mode-map
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


(with-eval-after-load 'terraform-mode
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  (define-key terraform-mode-map (kbd "C-c b") 'terraform-format-buffer))

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
(require 'ansi-color)
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


;; colorize compile mode output
(add-hook 'compilation-filter-hook #'display-ansi-colors)
(global-set-key (kbd "<f9>") 'projectile-compile-project)
(setq compilation-scroll-output 'first-error)  ;; scroll, but stop at first error

(use-package writegood-mode
  :defer t
  :init
  (progn
    (add-hook 'text-mode-hook #'writegood-mode)
    (add-hook 'markdown-mode-hook #'writegood-mode))
  :bind (("C-c g" . #'writegood-mode)))


(global-set-key (kbd "S-SPC") (lambda() (interactive) (cycle-spacing -1)))

(use-package server
  :config
  (server-start))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

;; mouse avoidance mode is buggy, see
;; https://groups.google.com/g/gnu.emacs.help/c/W_1VhwJrelE
;; (mouse-avoidance-mode 'banish)

(setq make-pointer-invisible nil)

(use-package gcmh
  :config
  (gcmh-mode 1))
