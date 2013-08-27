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

;; autoloads
(autoload 'gid "id-utils" t)
(autoload 'sgml-quote "sgml-mode"
  "Quote SGML text in region START ... END.
Only &, < and > are quoted, the rest is left untouched.
With prefix argument UNQUOTEP, unquote the region." t)
(defalias 'html-quote 'sgml-quote)

(autoload 'git-grep "git-grep" "Run git grep" t)
(defalias 'gg 'git-grep)



;; shell-pop
(require 'shell-pop)
(global-set-key (kbd "C-t") 'shell-pop)



(require 'setup-highlight-symbol)

;; git-messenger
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)


(defadvice save-buffers-kill-emacs (around emacs-die-hard activate)
  "really???"
  (if (or
       (not server-process)
       (string= "kill emacs" (read-from-minibuffer "to quit emacs type: 'kill emacs':")))
      ad-do-it))


(require 'setup-bm)


(schmir-maybe-server)


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


(require 'setup-isearch)

;; -- from http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (>= (recursion-depth) 1)
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


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

;; show pathnames for buffers with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(setq visible-bell 1
      require-final-newline t
      display-time-24hr-format t
      inhibit-startup-message t)

(setq scroll-margin 2
      scroll-step 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


(setq save-abbrevs t
      default-abbrev-mode t)
(abbrev-mode 1)

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(setq change-major-mode-with-file-name t
      ;; Filename completion ignores these.
      completion-ignored-extensions (append completion-ignored-extensions
					    '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx"))
      backward-delete-char-untabify-method 'nil	;; don´t untabify, just delete one char
      font-lock-maximum-decoration t			;; maximum decoration
      next-line-add-newlines nil			;; don´t add newlines when trying to move cursor behind eof
      show-paren-style 'expression
      compilation-scroll-output t
      default-indicate-empty-lines t
      line-number-display-limit-width 100000
      kill-whole-line t				;; make kill-line at beginning of line kill the whole line
      woman-use-own-frame nil				;; don't create new frame for manpages
      vc-handled-backends nil
      vc-follow-symlinks t				;; follow symlinks and don't ask
      enable-recursive-minibuffers t
      )


;; prevent emacs from asking for coding-system...
(set-language-environment "utf-8")

;; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)

;; place cursor on same buffer position between editing sessions
(setq-default save-place t
	      save-place-file (concat dotfiles-dir "emacs-places"))
(require 'saveplace)

;; automatically chmod +x when the file has shebang "#!"
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'server-done-hook 'delete-frame)


(eval-after-load "multi-term"
  '(progn
     (multi-term-keystroke-setup)))

(require 'setup-tramp)

(require 'setup-frame)

(require 'evimodeline)
(add-hook 'find-file-hook 'evimodeline-find-file-hook)

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



(require 'setup-org)
(require 'setup-ido)



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


(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'smart-tab)
	     (highlight-symbol-mode 1)
	     (setq c-basic-offset 2)
	     ))

(add-hook 'javascript-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'smart-tab)
	     (highlight-symbol-mode 1)
	     (setq c-basic-offset 2)
	     ))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-hook 'rst-mode-hook 'auto-fill-mode)


;; --- lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-hook 'lua-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'smart-tab)
	     (highlight-symbol-mode 1)
	     (setq lua-indent-level 4)
	     (flymake-mode)))



(require 'setup-python)
(require 'setup-c-mode)
(require 'schmir-flymake)


(if (fboundp 'schmir-hl-fixme)
    (mapc 'schmir-hl-fixme
	  '(python-mode c-mode c++-mode emacs-lisp-mode listp-mode js2-mode)))


(setq smart-tab-using-hippie-expand 't)


(global-set-key [f5] 'git-grep)




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

(eval-after-load "rcirc"
  '(progn
    (message "loading schmir-irc")
    (require 'schmir-irc)))


(setq irc-screen-number nil)
(defun goto-irc-screen ()
  (interactive)
  (if (not irc-screen-number)
      (progn
	(escreen-create-screen)
	(setq irc-screen-number escreen-current-screen-number)
	;; as i don't do this by default in escreen-create-screen
	(delete-other-windows))
    (escreen-goto-screen irc-screen-number)))

(defun toggle-irc-screen ()
  (interactive)
  (if (eq irc-screen-number escreen-current-screen-number)
      (escreen-goto-last-screen)
    (goto-irc-screen))
  (escreen-get-active-screen-numbers-with-emphasis))

(defun my-irc-next-active()
  (interactive)
  (if (fboundp 'rcirc-next-active-buffer)
      (progn
	(if rcirc-activity
	    (progn
	      (goto-irc-screen)
	      (rcirc-next-active-buffer nil)
	      (escreen-get-active-screen-numbers-with-emphasis))
	  (if (or
	       (eq irc-screen-number escreen-current-screen-number)
	       (if (eq last-command 'my-irc-next-active)
		   t
		 (message "press key again in order to return to irc screen")
		 nil))
	      (toggle-irc-screen))))
    (goto-irc-screen)
    (my-irc)))

(global-set-key [f12] 'my-irc-next-active)

(global-set-key [mouse-3] 'imenu)

(require 'setup-key-chord)

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
(require 'setup-print)
(put 'downcase-region 'disabled nil)


(require 'setup-fancy-comment)

(put 'set-goal-column 'disabled nil)


(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)



(require 'setup-escreen)





(blink-cursor-mode 1)


(require 'setup-lastmodified)

(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))

(require 'setup-exec-abbrev)

;;; enable smerge mode
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)


(require 'help-mode)
(require 'setup-completion)

(require 'grep)
(setq search-all-buffers-ignored-files '(".bbdb" ".newsrc-dribble"))

(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
		     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))

(global-set-key [f7] 'search-all-buffers)


(message "initialization complete")
