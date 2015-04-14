;; -*- mode: emacs-lisp; coding: utf-8 -*-

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(setq load-prefer-newer t
      gc-cons-threshold 20000000
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")

(require 'cl)

(defvar dotfiles-dir nil "location of dotfiles-directory")

(setq dotfiles-dir
      (file-name-directory
       (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))
(require 'setup-pre-init)

(if (string= system-name "W71580")
    (progn
      (add-to-list 'exec-path "C:/u6397/portable-git/bin")
      (setenv "PATH" (concat "C:\\u6397\\portable-git\\bin;" (getenv "PATH")))))


(require 'setup-gnus)
(require 'setup-package)
(require 'use-package)

(when (>= emacs-major-version 24)
  (use-package color-theme :ensure t)
  (use-package zenburn-theme :ensure t))

;; delay loading of elisp mode and it's dependencies
(setq initial-major-mode 'fundamental-mode)
(use-package fixup-scratch-buffer
  :defer 3
  :config (fixup-scratch-buffer))

(require 'schmir-fun)

(use-package ace-jump-mode :ensure t
  :bind ("H-SPC" . ace-jump-mode)
  :config (setq ace-jump-word-mode-use-query-char nil))

(use-package paredit :ensure t
  :commands (paredit-mode enable-paredit-mode)
  :config (progn
	    (define-key paredit-mode-map (kbd "M-q") nil)
	    (define-key paredit-mode-map (kbd "<C-right>") nil)
	    (define-key paredit-mode-map (kbd "<C-left>") nil)
	    (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
	    (define-key paredit-mode-map (kbd "<M-left>") 'paredit-forward-barf-sexp)

	    (define-key paredit-mode-map (kbd "M-9") 'paredit-wrap-sexp)
	    (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
	    (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
	    (define-key paredit-mode-map (kbd "<C-S-right>") 'forward-sexp)
	    (define-key paredit-mode-map (kbd "<C-S-left>") 'backward-sexp)))

(require 'setup-smartparens)

(use-package highlight-symbol :ensure t
  :commands highlight-symbol-mode
  :bind (([(control f1)]	. highlight-symbol-at-point)
	 ([f1]			. highlight-symbol-next)
	 ([(shift f1)]		. highlight-symbol-prev)
	 ([(meta f1)]		. highlight-symbol-query-replace))
  :init
  (progn
    (defun turn-on-highlight-symbol-mode ()
      (interactive)
      (highlight-symbol-mode 1)))

  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package aggressive-indent :ensure t :defer t
  :commands (aggressive-indent-mode))

(use-package lisp-mode
  :config
  (progn
    (defun schmir-elisp-hook ()
      (aggressive-indent-mode 1)
      (eldoc-mode)
      (enable-paredit-mode)
      (local-set-key [(tab)] 'smart-tab)
      (flycheck-mode 1)
      (highlight-symbol-mode 1))

    (add-hook 'emacs-lisp-mode-hook 'schmir-elisp-hook)))

(use-package company :ensure t
  :commands (company-mode)
  :config
  (progn
    (setq company-idle-delay 0.8
	  company-minimum-prefix-length 2)))

(use-package cider :ensure t
  :commands (cider-connect cider-jack-in)
  :config
  (progn
    (require 'cider-repl)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (if (eq system-type 'windows-nt)
	(add-hook 'cider-repl-mode-hook 'remove-dos-eol))
    (setq nrepl-hide-special-buffers t
	  repl-popup-stacktraces-in-repl t
	  nrepl-history-file "~/.emacs.d/nrepl-history")
    (defun schmir-cider-repl-hook ()
      (auto-complete-mode 0)
      (company-mode 1))

    (add-hook 'cider-repl-mode-hook 'schmir-cider-repl-hook)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (setq cider-prompt-save-file-on-load nil
	  cider-repl-result-prefix ";; => ")


    (defadvice cider-load-buffer (after switch-namespace activate compile)
      "switch to namespace"
      (cider-repl-set-ns (cider-current-ns))
      (cider-switch-to-repl-buffer))

    (define-key cider-mode-map '[f10] 'cider-load-buffer)

    (define-key cider-repl-mode-map '[f10] 'delete-window)
    (define-key cider-repl-mode-map (kbd "C-c C-w") 'cider-eval-last-sexp-and-replace)
    (define-key cider-stacktrace-mode-map '[f10] 'cider-popup-buffer-quit-function)
    (define-key cider-docview-mode-map '[f10] 'cider-popup-buffer-quit-function)
    (define-key cider-docview-mode-map (kbd "H-h") 'cider-popup-buffer-quit-function)
    (define-key cider-mode-map (kbd "H-h") 'cider-doc)
    (define-key cider-repl-mode-map (kbd "H-h") 'cider-doc)))

(use-package clojure-mode-extra-font-locking :ensure t :defer t)
(use-package clojure-mode :ensure t :mode "\\.clj$\\|\\.boot$"
  :config
  (progn
    (message "configuring clojure-mode")
    (require 'clojure-mode-extra-font-locking)
    (defun schmir-clojure-hook ()
      (aggressive-indent-mode 1)
      (enable-paredit-mode)
      (auto-complete-mode 0)
      (company-mode 1)
      (highlight-symbol-mode 1))

    (add-hook 'clojure-mode-hook 'schmir-clojure-hook)

    ;; fix indentation of cond expressions
    (put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4))
    (put-clojure-indent 'cond nil)))

(use-package magit :ensure t
  :commands (magit-status)
  :bind (("C-c m" . magit-status))
  :config (progn
	    (defun magit-commit-mode-init ()
	      (when (looking-at "\n")
		(open-line 1))
	      (turn-on-flyspell)
	      (toggle-save-place 0))

	    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init)


	    ;; make magit commit mode not close the frame
	    ;; we have (add-hook 'server-done-hook 'delete-frame)
	    ;; see https://github.com/magit/magit/issues/771
	    (defadvice git-commit-commit (around no-kill-frame activate)
	      (flet ((delete-frame (&optional FRAME FORCE) ()))
		ad-do-it))

	    (defadvice magit-status (around magit-fullscreen activate)
	      (window-configuration-to-register :magit-fullscreen)
	      ad-do-it
	      (delete-other-windows))

	    (defun magit-quit-session ()
	      "Restores the previous window configuration and kills the magit buffer"
	      (interactive)
	      (kill-buffer)
	      (jump-to-register :magit-fullscreen))


	    (setq magit-omit-untracked-dir-contents t)
	    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
	    (if (eq system-type 'windows-nt)
		(setq magit-git-executable (executable-find "git")))))

(use-package git-messenger :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)))

(use-package compile-dwim
  :bind (([f9] . compile-dwim)
	 ("c" . compile-dwim))
  :config (progn
	    (defadvice yes-or-no-p (around no-query-compilation-always-kill activate)
	      "make `compile' always kill existing compilation."
	      (if (string-match "A compilation process is running; kill it\\?"
				prompt)
		  (setq ad-return-value t)
		ad-do-it))

	    (setq compilation-ask-about-save nil
		  compilation-scroll-output t)))


(use-package bm :ensure t
  :commands (bm-buffer-restore bm-buffer-save bm-buffer-save-all bm-repository-save bm-toggle bm-next bm-previous)
  :bind (([C-f8] . bm-toggle)
	 ([f8] . bm-next)
	 ([S-f8] . bm-previous))
  :config (progn ;; Restoring bookmarks when on file find.
	    (add-hook 'find-file-hooks 'bm-buffer-restore)

	    ;; Saving bookmark data on killing a buffer
	    (add-hook 'kill-buffer-hook 'bm-buffer-save)

	    ;; Saving the repository to file when on exit.
	    ;; kill-buffer-hook is not called when emacs is killed, so we
	    ;; must save all bookmarks first.
	    (add-hook 'kill-emacs-hook '(lambda nil
					  (bm-buffer-save-all)
					  (bm-repository-save)))

	    ;; Update bookmark repository when saving the file.
	    (add-hook 'after-save-hook 'bm-buffer-save)

	    ;; Restore bookmarks when buffer is reverted.
	    (add-hook 'after-revert-hook 'bm-buffer-restore)

	    (setq bm-highlight-style 'bm-highlight-only-fringe
		  bm-recenter 1
		  bm-wrap-immediately nil
		  bm-buffer-persistence t
		  bm-restore-repository-on-load t)

	    (defadvice bm-buffer-save  (around no-message activate)
	      "be quiet when saving bookmarks"
	      (flet ((message ())) ad-do-it))))

(use-package anzu :ensure t
  :defer 5
  :commands global-anzu-mode
  :config (global-anzu-mode))

(require 'setup-isearch)
(require 'setup-tramp)
(require 'setup-frame)
(require 'setup-winring)
(require 'setup-org)
(require 'setup-ido)
(require 'setup-print)

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))


(defun maybe-untabify ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(if (y-or-n-p "Buffer contains tabs. Replace with spaces? ")
	    (untabify-buffer)))))

(use-package python
  :mode ("\\wscript$\\|\\SConstruct$\\|\\SConscript$\\|\\.py$\\|\\.jy$\\|\\.py\\.cov$" . python-mode)
  :init
  (progn
    (use-package jedi :ensure t :commands (jedi:setup))
    (loop for i in '("jython" "pypy" "python" "python2" "python2.4" "python2.5" "python2.6" "python2.7" "python3" "python3.0" "python3.1" "python3.2" "python3.3")
	  do (add-to-list 'interpreter-mode-alist `(,i . python-mode))))
  :config
  (progn
    (defun schmir-python-hook ()
      (interactive)
      (require 'unicode-symbols)
      (substitute-patterns-with-unicode
       (list
	(cons "\\<\\(lambda\\)\\>" 'lambda)))

      (jedi:setup)
      (add-hook 'find-file-hooks 'maybe-untabify 'nil 1)
      (setq py-smart-indentation 1
	    indent-tabs-mode nil)
      (modify-syntax-entry ?_ "w") ;; make _ part of words.

      (highlight-symbol-mode 1)

      ;;(highlight-phrase "[Ss]elf" (quote bold))
      (local-set-key (kbd "RET") 'newline-and-indent)
      (local-set-key (kbd "M-]") 'python-mark-block)
      (local-set-key (kbd "C-h n") 'schmir-pyhelp)

      (local-set-key [C-S-left]  '(lambda()
				    (interactive)
				    (shift-region -4)))
      (local-set-key [C-S-right] '(lambda()
				    (interactive)
				    (shift-region 4)))

      (local-set-key [(tab)] 'smart-tab)

      (setq flycheck-checker 'python-flake8)
      (if (not (file-remote-p (buffer-file-name)))
	  (flycheck-mode 1)))

    (add-hook 'python-mode-hook 'schmir-python-hook)

    ;; highlight self in python-mode
    (font-lock-add-keywords 'python-mode '(("\\<\\(self\\)" 1 font-lock-builtin-face)))

    (setq py-XXX-tag-face font-lock-warning-face)

    (if (fboundp 'cython-mode)
	(add-to-list 'auto-mode-alist '("\\.\\(pyx\\|pxi\\|pxd\\)$" . cython-mode)))

    (setq python-pep8-options '("--repeat"))))

(require 'setup-c-mode)

(use-package key-chord :ensure t
  :config
  (progn
    (setq key-chord-two-keys-delay 0.05)
    (key-chord-mode 1)
    (key-chord-define-global "nm"     'ido-switch-buffer)
    (key-chord-define-global "m,"     'ido-find-file)
    (key-chord-define-global "./"     'save-buffer)
    (key-chord-define-global ":\""    'comment-dwim)
    (key-chord-define-global ";\""    'comment-dwim)
    (key-chord-define-global ";'"    'comment-dwim)))

(require 'setup-fancy-comment)
(require 'setup-lastmodified)
(require 'setup-completion)

(use-package hippie-exp
  :bind (([C-tab] . hippie-expand))
  :config (progn
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
		    try-complete-lisp-symbol))))

(use-package git-grep
  :bind (([f5] . git-grep)))

(use-package setup-grep
  :bind (([f7] . search-all-buffers)))

(require 'setup-uniquify)

(use-package lua-mode :ensure t :mode "\\.lua$"
  :config
  (progn
    (add-hook 'lua-mode-hook
	      '(lambda()
		 (local-set-key [(tab)] 'smart-tab)
		 (highlight-symbol-mode 1)
		 (setq lua-indent-level 4)))))

(require 'setup-irc)
(use-package evimodeline
  :commands (evimodeline-find-file-hook)
  :init (add-hook 'find-file-hook 'evimodeline-find-file-hook))

(require 'setup-file-hook)
(require 'setup-abbrev)
(require 'setup-cwc)
(require 'setup-whole-line-or-region)

(use-package setup-sequential-command
  :ensure sequential-command
  :bind (([home] . my-home)
	 ([end] . my-end)))

(use-package rosi
  :commands rosi-mode
  :mode ("\\.rsf\\|\\.rsi\\'" . rosi-mode)
  :init (modify-coding-system-alist 'file "\\(\\.rsf\\|\\.msg\\)$" 'cp437)
  :config (add-hook 'rosi-mode-hook 'turn-on-highlight-symbol-mode))

(use-package projectile :ensure t
  :commands (projectile-global-mode)
  :defer 5
  :config (projectile-global-mode))


;; shell-pop
(use-package shell-pop :ensure t
	     :bind (("C-t" . shell-pop)))


(require 'setup-kill-emacs)
(require 'setup-server)
(require 'setup-face)
(require 'setup-mouse)


(setq echo-keystrokes 0.1)

(savehist-mode 1) ;; keep track of minibuffer commands
(size-indication-mode 1) ;; show file size

(use-package rainbow-delimiters :ensure t :defer t
  :commands (rainbow-delimiters-mode)
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(global-auto-revert-mode 1) ;; re-read buffers from disk unless they're `dirty'
(display-time-mode 1)
(auto-compression-mode t) ; allow loading of compressed (e.g. gzipped) files
(global-font-lock-mode t)

(setq gist-view-gist t)
(put 'narrow-to-region 'disabled nil)
(setq line-move-visual nil) ;; what did they think ?

(recentf-mode t)
(setq recentf-max-saved-items 200)

(require 'which-func)
(add-to-list 'which-func-modes 'org-mode)
(which-func-mode 1)

(ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(setq dired-recursive-deletes 'always)

(blink-cursor-mode 1)
(auto-image-file-mode 1)
(global-cwarn-mode 1)

(global-hl-line-mode 1)   ;; highlight line where cursor is

(column-number-mode 1)
(show-paren-mode 1)
(put 'overwrite-mode 'disabled nil)
(setq mark-even-if-inactive t)
(transient-mark-mode 1)




(setq visible-bell 1
      require-final-newline t
      display-time-24hr-format t
      inhibit-startup-message t)

(setq scroll-margin 2
      scroll-step 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq change-major-mode-with-file-name t
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


;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)

;; place cursor on same buffer position between editing sessions
(setq-default save-place t
	      save-place-file (concat dotfiles-dir "emacs-places"))
(require 'saveplace)




(eval-after-load "multi-term"
  '(progn
     (multi-term-keystroke-setup)))



;; Change backup behavior to save in a directory, not in a miscellany
;; of files all over the place.
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)


(autoload 'fm-start "fm" "follow mode for compilation like buffers")
(use-package markdown-mode :ensure t
  :mode "\\.md$\\|\\.markdown$")

(use-package ssh-config-mode :ensure t
  :mode "\\.ssh/config$\\|sshd?_config$")

(use-package sh-script
  :mode ("\\PKGBUILD$\\|\\.sh$" . sh-mode))

(add-to-list 'auto-mode-alist '("\\.wsdl$" . sgml-mode))


(add-to-list 'auto-mode-alist '("\\.pas$\\|\\.dpr" .  delphi-mode))


(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-hook 'javascript-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'smart-tab)
	     (highlight-symbol-mode 1)
	     (setq c-basic-offset 2)))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(use-package rst
  :mode ("\\.rst\\'" . rst-mode)
  :config (progn
	    (clear-abbrev-table rst-mode-abbrev-table)
	    (add-hook 'rst-mode-hook 'auto-fill-mode)))

;; (require 'schmir-flymake)

;; highlight XXX, FIXME, ... in these modes
(mapc 'schmir-hl-fixme
      '(python-mode c-mode c++-mode emacs-lisp-mode listp-mode js2-mode))


(setq smart-tab-using-hippie-expand 't)

(use-package deft :ensure t
  :bind ("C-c d" . deft))

;; (global-set-key (kbd "C-c d") 'deft)
(global-set-key (quote [S-iso-lefttab]) 'tab-to-tab-stop)
;; (global-set-key "" (quote comment-region))

(global-set-key (kbd "C-o") 'delete-blank-lines)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

(global-set-key (kbd "M-:") 'align-regexp)
(global-set-key (kbd "M-RET") 'fullscreen)

(global-set-key (quote [S-return]) 'open-line-below)
(global-set-key (kbd "M-\"") 'comment-dwim)

(setq suggest-key-bindings t)

(use-package misc-cmds :ensure t
  :commands (mark-buffer-before-point mark-buffer-after-point)
  :init (progn (define-key ctl-x-map [home] 'mark-buffer-before-point)
		 (define-key ctl-x-map [end]  'mark-buffer-after-point)))

(global-set-key (kbd "C-z") 'undo)


(require 'repeatable)
(repeatable-command-advice next-buffer)
(repeatable-command-advice exchange-point-and-mark)


(global-set-key (kbd "C-j")
		'(lambda()
		   (interactive)
		   (delete-indentation 1)))

(global-set-key (kbd "C-S-j")
		'(lambda()
		   (interactive)
		   (delete-indentation)))
(global-set-key "\M-p" 'goto-line)

(global-set-key [C-backspace] 'backward-kill-word)

(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)

(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))

(require 'help-mode)

(use-package exec-abbrev-cmd
  :bind ("M-x" . exec-abbrev-cmd)
  :init (setq exec-abbrev-cmd-file "~/.emacs.d/exec-abbrev-cmd.dat")
  :config (exec-abbrev-cmd-mode 1))

(put 'minibuffer-complete-and-exit 'disabled nil)
(message "initialization complete")
