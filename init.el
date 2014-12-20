;; -*- mode: emacs-lisp; coding: utf-8 -*-

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(setq load-prefer-newer t
      gc-cons-threshold 20000000
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

(require 'cl)

(defvar dotfiles-dir nil "location of dotfiles-directory")

(setq dotfiles-dir
      (file-name-directory
       (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))
(add-to-list 'load-path (concat dotfiles-dir "use-package"))
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
  :idle (fixup-scratch-buffer))

(require 'schmir-fun)

(use-package ace-jump-mode
  :bind ("H-SPC" . ace-jump-mode)
  :config (setq ace-jump-word-mode-use-query-char nil))

(use-package paredit
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
(require 'setup-elisp)
(require 'setup-clojure)

(use-package magit
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

(use-package git-messenger
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

(use-package highlight-symbol
  :commands highlight-symbol-mode
  :bind (([(control f1)]	. highlight-symbol-at-point)
	 ([f1]			. highlight-symbol-next)
	 ([(shift f1)]		. highlight-symbol-prev)
	 ([(meta f1)]		. highlight-symbol-query-replace))
  :config (setq highlight-symbol-idle-delay 0.3))

(use-package bm
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

(require 'setup-isearch)
(require 'setup-tramp)
(require 'setup-frame)
(require 'setup-winring)
(require 'setup-org)
(require 'setup-ido)
(require 'setup-print)
(require 'setup-python)
(require 'setup-c-mode)
(require 'setup-key-chord)
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
(require 'setup-lua)
(require 'setup-irc)
(require 'setup-file-hook)
(require 'setup-abbrev)
(require 'setup-cwc)
(require 'setup-whole-line-or-region)

(use-package setup-sequential-command
     :bind (([home] . my-home)
	    ([end] . my-end)))

(use-package rosi
	     :commands rosi-mode
	     :init (progn
		     (add-to-list 'auto-mode-alist '("\\.rsf\\|\\.rsi\\'" . rosi-mode))
		     (modify-coding-system-alist 'file "\\(\\.rsf\\|\\.msg\\)$" 'cp437))
	     :config (add-hook 'rosi-mode-hook 'turn-on-highlight-symbol-mode))

(require 'projectile)
(projectile-global-mode)

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
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
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


;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")

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

(add-to-list 'auto-mode-alist
	     '("\\.md$\\|\\.markdown$" . markdown-mode))


(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))

(add-to-list 'auto-mode-alist '("\\PKGBUILD$\\|\\.sh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
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

(use-package deft
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

(use-package misc-cmds
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

(use-package
 exec-abbrev-cmd
 :commands exec-abbrev-cmd
 :init (progn
	 (global-set-key (kbd "M-x") 'exec-abbrev-cmd)
	 (setq exec-abbrev-cmd-file "~/.emacs.d/exec-abbrev-cmd.dat"))
 :config (progn (exec-abbrev-cmd-mode 1)
		))
;; (require 'setup-exec-abbrev)

(put 'minibuffer-complete-and-exit 'disabled nil)
(message "initialization complete")
