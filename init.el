;; -*- mode: emacs-lisp; coding: utf-8 -*-

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(require 'cl)

(setq dotfiles-dir
      (file-name-directory
       (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(require 'setup-pre-init)

(load-theme 'sinburn t)

(require 'setup-package)
(require 'schmir-fun)
(require 'setup-clojure)
(require 'setup-magit)
(require 'setup-gnus)
(require 'setup-compile)
(require 'setup-highlight-symbol)
(require 'setup-bm)
(require 'setup-isearch)
(require 'setup-tramp)
(require 'setup-frame)
(require 'setup-org)
(require 'setup-ido)
(require 'setup-print)
(require 'setup-python)
(require 'setup-c-mode)
(require 'setup-key-chord)
(require 'setup-fancy-comment)
(require 'setup-escreen)
(require 'setup-lastmodified)
(require 'setup-completion)
(require 'setup-grep)
(require 'setup-uniquify)
(require 'setup-lua)
(require 'setup-irc)
(require 'setup-file-hook)
(require 'setup-abbrev)

;; shell-pop
(require 'shell-pop)
(global-set-key (kbd "C-t") 'shell-pop)

(defadvice save-buffers-kill-emacs (around emacs-die-hard activate)
  "really???"
  (if (or
       (not server-process)
       (string= "kill emacs" (read-from-minibuffer "to quit emacs type: 'kill emacs':")))
      ad-do-it))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(schmir-maybe-server)


(savehist-mode 1) ;; keep track of minibuffer commands
(size-indication-mode 1) ;; show file size
(global-rainbow-delimiters-mode 1)
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
(mouse-wheel-mode 1)
(auto-image-file-mode 1)
(global-cwarn-mode 1)

(global-hl-line-mode 1)   ;; highlight line where cursor is

(column-number-mode 1)
(show-paren-mode 1)
(put 'overwrite-mode 'disabled nil)
(setq mark-even-if-inactive t)
(transient-mark-mode 1)


;; Drive out the mouse when it's too near to the cursor.
(mouse-avoidance-mode 'exile)
(setq mouse-avoidance-threshold 10
      mouse-avoidance-nudge-dist 20
      mouse-avoidance-nudge-var 5)


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
					    '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx"))
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


(add-hook 'server-done-hook 'delete-frame)


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

(defface completion-setup-directory-face  '((t (:foreground "dark orange")))
  "Face to use for directories."
  :group 'color-file-completion)

(defcustom color-file-completion-always t "If true, always turn on regexps in
completion buffers."
  :group 'color-file-completion
  :type 'boolean)

(defun completion-setup-directory-face()
  "When we are completing a filename, highlight directories."
  (interactive)
  ;;if this is completing a filename... highlight faces...
  (when (or color-file-completion-always
            (eq minibuffer-completion-table 'read-file-name-internal))
    (let((font-lock-verbose nil))
      (font-lock-mode 1)
      (font-lock-add-keywords nil '(("[^ \n]+/" 0 'completion-setup-directory-face keep)))
      (font-lock-fontify-buffer))))

(add-hook 'completion-list-mode-hook 'completion-setup-directory-face)

(autoload 'fm-start "fm" "follow mode for compilation like buffers")

(add-hook 'emacs-lisp-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'smart-tab)
	     (highlight-symbol-mode 1)))

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

(add-hook 'rst-mode-hook 'auto-fill-mode)

(require 'schmir-flymake)


(if (fboundp 'schmir-hl-fixme)
    (mapc 'schmir-hl-fixme
	  '(python-mode c-mode c++-mode emacs-lisp-mode listp-mode js2-mode)))


(setq smart-tab-using-hippie-expand 't)

(global-set-key (quote [S-iso-lefttab]) 'tab-to-tab-stop)
;; (global-set-key "" (quote comment-region))

(global-set-key (kbd "C-o") 'delete-blank-lines)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)


;; (global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)

(global-set-key (kbd "M-:") 'align-regexp)
(global-set-key (kbd "M-b") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-RET") 'fullscreen)


(global-set-key (quote [S-return]) 'open-line-below)
(global-set-key (kbd "M-\"") 'comment-dwim)


(global-set-key [mouse-3] 'imenu)


(setq suggest-key-bindings t)

(when (require-try 'misc-cmds)
  (define-key ctl-x-map [home] 'mark-buffer-before-point)
  (define-key ctl-x-map [end]  'mark-buffer-after-point))

;; http://www.emacswiki.org/emacs-ru/WholeLineOrRegion
;; This minor mode allows functions to operate on the current line if
;; they would normally operate on a region and region is currently
;; undefined.

(when (require-try 'whole-line-or-region)
  (add-to-list 'whole-line-or-region-extensions-alist '(comment-dwim whole-line-or-region-comment-dwim nil))
  (whole-line-or-region-mode t)
  (diminish 'whole-line-or-region-mode)
  (defalias 'whole-line-or-region-kill-region 'schmir-whole-line-or-region-kill-region))


;; "funky stuff" ;; proceed with caution

(global-set-key (kbd "C-z") 'undo)



(when (require-try 'sequential-command)
  (define-sequential-command my-home
    back-to-indentation
    ;; beginning-of-line
    beginning-of-buffer
    seq-return)

  (define-sequential-command my-end
    end-of-line
    end-of-buffer
    seq-return)

  (global-set-key (quote [home]) 'my-home)
  (global-set-key (quote [end]) 'my-end))


(when (require-try 'repeatable)
  (repeatable-command-advice next-buffer)
  (repeatable-command-advice exchange-point-and-mark))

(global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu)

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

(require 'setup-exec-abbrev)
(message "initialization complete")
