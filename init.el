;; -*- mode: emacs-lisp; coding: utf-8 -*-

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



(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable"     . 9)
        ("melpa"            . 5)
	("gnu"              . 1)))


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
   boxquote
   cider
   clojure-mode
   clojure-mode-extra-font-locking
   company
   company-solidity
   counsel
   counsel-projectile
   default-text-scale
   deft
   dockerfile-mode
   elpy
   git-messenger
   highlight-symbol
   htmlize
   ivy
   leuven-theme
   magit
   markdown-mode
   persistent-scratch
   projectile
   shell-pop
   smartparens
   smex
   solidity-flycheck
   solidity-mode
   spacemacs-theme
   swiper
   which-key
   yaml-mode
   zenburn-theme))

(setq package-pinned-packages
      '((swiper    . "melpa")
	(ivy       . "melpa")))


(defun abedra/packages-installed-p ()
  (loop for pkg in schmir/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (abedra/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg schmir/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; (load-theme 'spacemacs-dark)
(load-theme 'leuven)

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; place cursor on same buffer position between editing sessions
(setq-default save-place t
	      save-place-file (expand-file-name "places" user-emacs-directory))
(require 'saveplace)

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
(windmove-default-keybindings)

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
      vc-handled-backends nil
      vc-follow-symlinks t				;; follow symlinks and don't ask
      enable-recursive-minibuffers t)

(setq company-idle-delay 0.8
      company-minimum-prefix-length 2)
(add-hook 'after-init-hook 'global-company-mode)

(persistent-scratch-setup-default)
(require 'which-key)
(which-key-mode)

(setq smart-tab-using-hippie-expand 't)

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
(global-set-key (kbd "<f5>") 'counsel-git-grep)

(global-set-key (kbd "C--") 'default-text-scale-decrease)
(global-set-key (kbd "C-=") 'default-text-scale-increase)

(progn
  (require 'highlight-symbol)
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

(defun schmir/black-buffer ()
  (interactive)
  (save-excursion
    (save-buffer)
    (shell-command (format "black %s" (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(require 'setup-clojure)
(elpy-enable)
;; these conflict with setup-smartparens
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<M-down>") nil)


(define-key python-mode-map (kbd "C-c b") 'schmir/black-buffer)

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
(defun display-ansi-colors
    (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(with-eval-after-load 'magit
  (advice-add 'magit-process-filter :after #'display-ansi-colors))

(global-set-key (kbd "S-SPC") (lambda() (interactive) (cycle-spacing -1)))
(require 'setup-server)
