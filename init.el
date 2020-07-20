;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;; on lexical-binding: https://nullprogram.com/blog/2016/12/22/

;; increase some internal limits related to elisp execution
(setq load-prefer-newer t
      gc-cons-threshold 20000000
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

;; get rid of visual clutter
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

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

;; bootstrap straight.el
;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package
 '(gcmh
   :type git
   :host github
   :repo "emacsmirror/gcmh"))
(require 'gcmh)
(gcmh-mode 1)


(global-set-key (kbd "<f12>") 'toggle-menu-bar-mode-from-frame)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
(require 'cl)

(setq
 schmir/packages
 '(aggressive-indent
   bbdb
   blacken
   boxquote
   cargo
   cider
   clojure-mode
   clojure-mode-extra-font-locking
   company
   company-solidity
   counsel
   counsel-projectile
   crux
   default-text-scale
   deft
   dockerfile-mode
   anaconda-mode
   company-anaconda
   elixir-mode
   flycheck-rust
   flymake-shellcheck
   gitignore-mode
   git-messenger
   go-mode
   highlight-symbol
   htmlize
   ivy
   js2-mode
   leuven-theme
   lsp-mode
   lsp-ui
   lsp-java
   company-lsp
   magit
   markdown-mode
   markdown-preview-mode
   persistent-scratch
   projectile
   racer
   rust-mode
   shell-pop
   smartparens
   smex
   solidity-flycheck
   solidity-mode
   prettier-js
   spacemacs-theme
   swiper
   terraform-mode
   tide
   tldr
   which-key
   writegood-mode
   yaml-mode
   zenburn-theme))

(dolist (pkg schmir/packages)
  (straight-use-package pkg))

;; (load-theme 'spacemacs-dark)
(load-theme 'leuven)

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; place cursor on same buffer position between editing sessions
(setq-default save-place-file (expand-file-name "places" user-emacs-directory))
(require 'saveplace)
(save-place-mode)

(global-set-key (kbd "C-t") 'shell-pop)
(global-set-key (kbd "C-z") 'undo)
(require 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
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

(setq company-idle-delay 0.8
      company-minimum-prefix-length 2)
(add-hook 'after-init-hook 'global-company-mode)

(persistent-scratch-setup-default)
(require 'which-key)
(which-key-mode)

(setq smart-tab-using-hippie-expand 't)


(setq tramp-default-method "ssh")
;; (customize-set-variable 'tramp-syntax 'simplified)

(require 'setup-cwc)
(require 'setup-smartparens)

(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(ivy-mode 1)
;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
(setq ivy-use-virtual-buffers t
      ivy-count-format "%d/%d ")

(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 10)
;; no regexp by default
(setq ivy-initial-inputs-alist nil)
;; configure regexp engine.
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))

(global-set-key (kbd "M-x") 'counsel-M-x)

;; (global-set-key (kbd "<f5>") 'counsel-git-grep)
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

(defalias 'br 'boxquote-region)
(defalias 'cc 'cider-connect)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'g 'gnus)
(defalias 'ee 'eval-expression)
(defalias 'rb 'revert-buffer)

(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail

      ;; we substitute sendmail with msmtp
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header      
      gnus-init-file (expand-file-name "~/.gnus-init.el"))

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

;; let me use windmove keybindings even in org-mode
(setq org-replace-disputed-keys t)

(setq deft-default-extension "org"
      deft-extensions '("org" "md" "txt")
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-use-filter-string-for-filename t)

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

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))



;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(require 'setup-clojure)

(straight-use-package
 '(zimports :host github :repo "schmir/zimports.el"
            :branch "master"))

(defun schmir/anaconda-eldoc-unless-tramp ()
  (if (tramp-tramp-file-p (buffer-file-name))
      (message "tramp file, disabling anaconda-eldoc-mode")
    (anaconda-eldoc-mode t)))

(setq blacken-only-if-project-is-blackened t)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'schmir/anaconda-eldoc-unless-tramp)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-anaconda))

(defun with-project-root-as-default-directory
    (orig-fun &rest args)
  "Run orig-fun with default-directory set to (projectile-project-root)"
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (apply orig-fun args)))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c b") 'blacken-buffer)
  (advice-add 'run-python :around #'with-project-root-as-default-directory))

(add-hook 'go-mode-hook #'lsp-deferred)

(defun schmir/solidity-setup ()
  ;; https://stackoverflow.com/questions/6952369/java-mode-argument-indenting-in-emacs
  (c-set-offset 'arglist-intro '+)
  (setq c-basic-offset 4)
  (setq tab-width 8))
(add-hook 'solidity-mode-hook 'schmir/solidity-setup)

(defun schmir/shfmt-buffer ()
  (interactive)
  (save-excursion
    (save-buffer)
    (shell-command (format "shfmt -w -i 2 %s" (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(with-eval-after-load 'sh-script
  (define-key sh-mode-map (kbd "C-c b") 'schmir/shfmt-buffer)
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

(with-eval-after-load 'terraform-mode
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  (define-key terraform-mode-map (kbd "C-c b") 'terraform-format-buffer))

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

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

(global-set-key (kbd "<C-tab>") 'hippie-expand)
(global-set-key (kbd "C-S-s") 'swiper-isearch)
(global-set-key (kbd "C-c s") 'magit-status)

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

(with-eval-after-load 'magit
  (advice-add 'magit-process-filter :after #'magit-display-ansi-colors))

;; colorize compile mode output
(add-hook 'compilation-filter-hook #'display-ansi-colors)
(global-set-key (kbd "<f9>") 'projectile-compile-project)
(setq compilation-scroll-output 'first-error)  ;; scroll, but stop at first error

(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'markdown-mode-hook 'writegood-mode)
(global-set-key (kbd "C-c g") 'writegood-mode)

(global-set-key (kbd "S-SPC") (lambda() (interactive) (cycle-spacing -1)))
(require 'setup-server)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
